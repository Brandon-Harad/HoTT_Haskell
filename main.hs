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
type Equalities = [(Name, CheckableTerm)]

evalInferable' :: Env -> Equalities -> InferableTerm -> Value
evalInferable' env eqs (Ann t _) = evalCheckable' env eqs t
evalInferable' env eqs (Bound i) = env !! i
evalInferable' env eqs (Free n) = case lookup n eqs of
        Just t -> evalCheckable' env eqs t
        Nothing -> vfree n
evalInferable' env eqs (App f x) = vapp (evalInferable' env eqs f) (evalCheckable' env eqs x)
evalInferable' env eqs (Univ n) = VUniv n
evalInferable' env eqs (Fun t t') = VFun (evalCheckable' env eqs t) (evalCheckable' env eqs t')

evalInferable :: InferableTerm -> Value
evalInferable = evalInferable' [] []

evalCheckable' :: Env -> Equalities -> CheckableTerm -> Value
evalCheckable' env eqs (Lam f) = VLam (\x -> evalCheckable' (x : env) eqs f)
evalCheckable' env eqs (Inf t) = evalInferable' env eqs t

evalCheckable :: CheckableTerm -> Value
evalCheckable = evalCheckable' [] []

type Judgements = [(Name, CheckableTerm)]

type Result a = Either String a
throwError = Left

inferType :: Judgements -> Equalities -> InferableTerm -> Result CheckableTerm
inferType = inferType' 0

inferType' :: Int -> Judgements -> Equalities -> InferableTerm -> Result CheckableTerm
-- If we receive an annotated type, just check if the annotation works
inferType' i judgements eqs (Ann e t) = do
    checkType' i judgements eqs e t
    return t
inferType' i judgements eqs (Free x) = case lookup x eqs of
        Just t -> return t
        Nothing -> case lookup x judgements of
            Just t -> return t
            Nothing -> throwError "unknown identifier"
inferType' i judgements eqs (App e e') = do
    t <- inferType' i judgements eqs e
    case t of
        Inf (Fun a b) -> do checkType' i judgements eqs e' a; return b
        _ -> throwError "illegal application"
inferType' i judgements eqs (Univ n) =
    if n < 0 then throwError "Universe indices must be nonnegative"
    else Right (Inf (Univ (n+1)))
inferType' i judgements eqs (Fun t t') = case (t, t') of
    (Inf u, Inf u') -> do
        v <- inferType' i judgements eqs u
        v' <- inferType' i judgements eqs u'
        case (v, v') of
            (Inf (Univ j), Inf (Univ k)) -> Right (Inf (Univ (max j k)))
            _ -> throwError "improperly formed function type"
    _ -> throwError "improperly formed function type"
inferType' _ _ _ (Bound t) = throwError "something has gone horribly wrong"

checkType :: Judgements -> Equalities -> CheckableTerm -> CheckableTerm -> Result ()
checkType = checkType' 0

checkType' :: Int -> Judgements -> Equalities -> CheckableTerm -> CheckableTerm -> Result ()
checkType' i judgements eqs (Inf e) t = do
    t' <- inferType' i judgements eqs e
    unless (t == t') (throwError "incorrect annotation")
checkType' i judgements eqs (Lam e) (Inf (Fun t t')) =
    checkType' (i+1) ((Local i, t) : judgements) eqs (substCheckable 0 (Free (Local i)) e) t'
checkType' _ _ _ _ _ = throwError "invalid typing"

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

define :: String -> CheckableTerm -> Equalities -> (InferableTerm, Equalities)
define s t e = (Free (Global s), (Global s, t) : e)

main :: IO ()
main = do
    putStrLn ":3"