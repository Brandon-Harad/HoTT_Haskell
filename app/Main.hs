module Main (main) where
import Parser (lexer, parseStatement, Token(..), parseTermNode)

-- judgements1 = []
-- eqs1 = []
-- (aType, judgements2) = Backend.assume "A" (Inf (Backend.Univ 0)) judgements1
-- (aElem, judgements3) = Backend.assume "a" (Backend.Inf aType) judgements2
-- (func, eqs2) = Backend.define "f" (Backend.Lam (Inf aElem)) eqs1

main :: IO ()
main = do
    -- print func
    -- print (quote (Backend.evalInferable' [] eqs2 (Backend.App (Backend.Ann (Inf func) (Inf (Backend.Fun (Inf aType) (Inf aType)))) (Inf aElem))))
    let f = \x -> lexer x >>= parseStatement
    print (f "def f : g(h(j)) := i(j)") 