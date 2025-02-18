module Parser_TP where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST_TP

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "while","do", "repeat", "?",
                                                     "switch", "case", "default", "promedio",
                                                     "linexp", "[", "]", ",", ":"
                                                     ]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , "~"
                                                       , ":="
                                                       ]
                                   }
                                 )
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
{--
chainl p op x
parsea 0 o más ocurrencias de p separadas por op
Retorna el valor que se obtiene al aplicar todas las
funciones retornadas por op a los valores retornados
por p

t := t + t | m
term = chainl factor (do {symbol "+"; return (+)})
factor = integer <|> parens term
--}
intexp :: Parser IntExp
intexp  = chainl1 term addopp
          <|> try (do reserved lis "promedio"
                      nums <- many intexp
                      return (Promedio nums))
          <|> try (do reserved lis "linexp"
                      reserved lis "["
                      c <- endBy comm (reserved lis ",")
                      n <- intexp
                      reserved lis "]"
                      return (LinExp c n))

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer lis
                 return (Const n)
              <|> do str <- identifier lis
                     return (Var str))

multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))

comm2 = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved lis "repeat"
                    c <- comm
                    reserved lis "until"
                    cond <- boolexp
                    reserved lis "end"
                    return (Repeat c cond))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- intexp
                    return (Let str e))
        <|> try (do cond <- boolexp
                    reserved lis "?"
                    case1 <- comm
                    reserved lis ":"
                    case2 <- comm
                    reserved lis "end"
                    return (Ternario cond case1 case2))
        <|> try (do reserved lis "switch"
                    e <- intexp
                    cases <- many switchCase
                    reserved lis "default"
                    c <- comm
                    reserved lis "end"
                    return (Switch e cases c))

-- Parser para los casos del switch
switchCase :: Parser (Integer, Comm)
switchCase = try (do reserved lis "case"
                     n <- integer lis
                     reservedOp lis ":"
                     c <- comm
                     return (n, c))

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
