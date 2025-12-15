module Parser where
import Backend ( Result, throwError )

data Token =
    AssumeToken
    | DefToken
    | LeftParen
    | RightParen
    | RightArrow
    | Colon
    | Name String
    | Univ
-- TODO: complete

-- data Statement =
--     Assume AssumeExpr
--     | Def DefExpr

-- Proper syntax: assume (name : term)
type AssumeExpr = (Name, Term)
-- Proper syntax: def name : term := term
type DefExpr = (Name, Term, Term)

type Name = String

-- Parsers
parseName :: [Token] -> Result ([Token], Name)
parseName (Name s : tks) = do return (tks, s)
parseName _ = throwError "no name found"