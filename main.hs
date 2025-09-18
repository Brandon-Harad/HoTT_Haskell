module Main where
import Control.Monad

data Name =
    Global String
    | Local Int
    | Quote Int
    deriving (Show, Eq)

data InferableTerm =
    Ann CheckableTerm CheckableTerm
    | Bound Int
    | Free Name
    | App InferableTerm CheckableTerm
    | Univ Int
    | Fun CheckableTerm CheckableTerm
    deriving (Show, Eq)

data CheckableTerm =
    Inf InferableTerm
    | Lam CheckableTerm
    deriving (Show, Eq)

data Value =
    VLam (Value -> Value)
    | VNeutral Neutral
    | VUniv Int
    | VFun Value Value

data Neutral =
    NFree Name
    | NApp Neutral Value

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

type Env = [Value]

evalInferable' :: InferableTerm -> Env -> Value
evalInferable' (Ann t _) e = evalCheckable' t e
evalInferable' (Bound i) e = e !! i
evalInferable' (Free n) e = vfree n
evalInferable' (App f x) e = vapp (evalInferable' f e) (evalCheckable' x e)
evalInferable' (Univ n) e = VUniv n
evalInferable' (Fun t t') e = VFun (evalCheckable' t e) (evalCheckable' t' e)

evalInferable :: InferableTerm -> Value
evalInferable t = evalInferable' t []

evalCheckable' :: CheckableTerm -> Env -> Value
evalCheckable' (Lam f) e = VLam (\x -> evalCheckable' f (x : e))
evalCheckable' (Inf t) e = evalInferable' t e

evalCheckable :: CheckableTerm -> Value
evalCheckable t = evalCheckable' t []

type Judgements = [(Name, CheckableTerm)]
type Equalities = [(Name, CheckableTerm)]

type Result a = Either String a
throwError = Left

inferType :: Judgements -> InferableTerm -> Result CheckableTerm
inferType = inferType' 0

inferType' :: Int -> Judgements -> InferableTerm -> Result CheckableTerm
-- If we receive an annotated type, just check if the annotation works
inferType' i judgements (Ann e t) = do
    checkType' i judgements e t
    return t
inferType' i judgements (Free x) = case
    lookup x judgements of
        Just t -> return t
        Nothing -> throwError "unknown identifier"
inferType' i judgements (App e e') = do
    t <- inferType' i judgements e
    case t of
        Inf (Fun a b) -> do checkType' i judgements e' a; return b
        _ -> throwError "illegal application"
inferType' i judgements (Univ n) = 
    if n < 0 then throwError "Universe indices must be nonnegative"
    else Right (Inf (Univ (n+1)))
inferType' i judgements (Fun t t') = case (t, t') of
    (Inf u, Inf u') -> do
        v <- inferType judgements u
        v' <- inferType judgements u'
        case (v, v') of
            (Inf (Univ j), Inf (Univ k)) -> Right (Inf (Univ (max j k)))
            _ -> throwError "improperly formed function type"
    _ -> throwError "improperly formed function type"
inferType' _ _ (Bound t) = throwError "something has gone horribly wrong"

checkType :: Judgements -> CheckableTerm -> CheckableTerm -> Result ()
checkType = checkType' 0

checkType' :: Int -> Judgements -> CheckableTerm -> CheckableTerm -> Result ()
checkType' i judgements (Inf e) t = do
    t' <- inferType' i judgements e
    unless (t == t') (throwError "incorrect annotation")
checkType' i judgements (Lam e) (Inf (Fun t t')) =
    checkType' (i+1) ((Local i, t) : judgements) (substCheckable 0 (Free (Local i)) e) t'
checkType' i judgements _ _ = throwError "invalid typing"

substInferable :: Int -> InferableTerm -> InferableTerm -> InferableTerm
substInferable i r (Ann e t) = Ann (substCheckable i r e) t
substInferable i r (Bound j) = if i == j then r else Bound j
substInferable i r (Free n) = Free n
substInferable i r (App e e') = App (substInferable i r e) (substCheckable i r e')
substInferable i r (Univ j) = Univ j
substInferable i r (Fun t t') = Fun (substCheckable i r t) (substCheckable i r t')

substCheckable :: Int -> InferableTerm -> CheckableTerm -> CheckableTerm
substCheckable i r (Inf e) = Inf (substInferable i r e)
substCheckable i r (Lam e) = Lam (substCheckable (i+1) r e)

quote :: Value -> CheckableTerm
quote = quote' 0

quote' :: Int -> Value -> CheckableTerm
quote' i (VLam f) = Lam (quote' (i+1) (f (vfree (Quote i))))
quote' i (VNeutral n) = Inf (neutralQuote i n)
quote' i (VUniv j) = Inf (Univ j)
quote' i (VFun v v') = Inf (Fun (quote' i v) (quote' i v'))

neutralQuote :: Int -> Neutral -> InferableTerm
neutralQuote i (NFree n) = Free n
neutralQuote i (NApp n v) = App (neutralQuote i n) (quote' i v)

assume :: String -> CheckableTerm -> Judgements -> (InferableTerm, Judgements)
assume s t j = (Free (Global s), (Global s, t) : j)

main :: IO ()
main = do
    putStrLn ":3"