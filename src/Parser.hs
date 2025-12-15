module Parser (lexer, parseStatement, Token(..), parseTermNode) where
import Data.List (stripPrefix)
import Data.Char (isDigit, isAlphaNum, isAlpha)

data Token =
    AssumeToken
    | DefToken
    | LeftParen
    | RightParen
    | Colon
    | Name String
    | Univ Int
    | Fun
    | Assign
    | Lambda
    | Dot
    deriving (Show, Eq)
-- TODO: complete

isValidNameChar :: Char -> Bool
isValidNameChar x = isAlphaNum x || x == '_' || x == '\''

lexer :: String -> Maybe [Token]
lexer [] = Just []
lexer (' ':s') = lexer s'
lexer s -- there has got to be a better way :sob:
    | Just s' <- stripPrefix "assume" s
        = (AssumeToken :) <$> lexer s'
    | Just s' <- stripPrefix "def" s
        = (DefToken :) <$> lexer s'
    | Just s' <- stripPrefix "(" s
        = (LeftParen :) <$> lexer s'
    | Just s' <- stripPrefix ")" s
        = (RightParen :) <$> lexer s'
    | Just s' <- stripPrefix "Fun" s
        = (Fun :) <$> lexer s'
    | Just s' <- stripPrefix "Lam" s
        = (Lambda :) <$> lexer s'
    | Just s' <- stripPrefix "." s
        = (Dot :) <$> lexer s'
    | Just s' <- stripPrefix ":=" s
        = (Assign :) <$> lexer s'
    | Just s' <- stripPrefix ":" s
        = (Colon :) <$> lexer s'
    | Just s' <- stripPrefix "U " s,
        (n, s'') <- span isDigit s'
        = (Univ (read n) :) <$> lexer s''
    | (w, s') <- span isValidNameChar s,
        not (null w),
        isAlpha (head w)
        = (Name w :) <$> lexer s'
lexer _ = Nothing

data Statement =
    Assume AssumeExpr
    | Def DefExpr
    deriving Show

type AssumeExpr = (String, TermNode)
type DefExpr = (String, TermNode, TermNode)

data TermNode =
    Lam String TermNode -- the string represents the variable name
    | NameT String
    | UnivT Int
    | FunT TermNode TermNode
    | Parened TermNode
    | Annoted TermNode TermNode
    | App TermNode TermNode
    deriving Show

parseStatement :: [Token] -> Maybe Statement
parseStatement (AssumeToken : LeftParen : Name f : Colon : tks)
    | Just (termNode, tks') <- parseTermNode tks,
    tks' == [RightParen]
    = Just (Assume (f, termNode))
parseStatement (DefToken : Name f : Colon : tks)
    | Just (g, tks') <- parseTermNode tks,
    not (null tks'),
    Assign == head tks',
    Just (h, tks'') <- parseTermNode (tail tks'),
    null tks''
    = Just (Def (f, g, h))
parseStatement _ = Nothing

-- The input looks like tokens corresponding to a termnode followed by some extra tokens :3
parseTermNode :: [Token] -> Maybe (TermNode, [Token])
parseTermNode (LeftParen : tks)
    | Just (tmNode, tks') <- parseTermNode tks,
    not (null tks'),
    head tks' == RightParen
    = peekAndParse (Parened tmNode, tail tks')
    | Just (tmNode1, tks') <- parseTermNode tks,
    not (null tks'),
    head tks' == Colon,
    Just (tmNode2, tks'') <- parseTermNode (tail tks'),
    not (null tks''),
    head tks'' == RightParen
    = peekAndParse (Annoted tmNode1 tmNode2, tail tks'')
parseTermNode (Fun : tks)
    | Just (tmNode1, tks') <- parseTermNode tks,
    Just (tmNode2, tks'') <- parseTermNode tks'
    = peekAndParse (FunT tmNode1 tmNode2, tks'')
parseTermNode (Univ n : tks) = peekAndParse (UnivT n, tks)
parseTermNode (Name f : tks) = peekAndParse (NameT f, tks)
parseTermNode (Lambda : Name x : Dot : tks)
    | Just (tmNode, tks') <- parseTermNode tks
    = peekAndParse (Lam x tmNode, tks')
parseTermNode _ = Nothing

peekAndParse :: (TermNode, [Token]) -> Maybe (TermNode, [Token])
peekAndParse (tmNode, []) = Just (tmNode, [])
peekAndParse (tmNode, tks) = do
    if head tks == LeftParen then do
        (tmNode2, tks') <- parseTermNode (tail tks)
        if not (null tks') && head tks' == RightParen 
            then return (App tmNode tmNode2, tail tks')
            else Nothing
    else return (tmNode, tks)

