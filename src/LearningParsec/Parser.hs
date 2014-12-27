-- learning http://uid0130.blogspot.jp/2013/07/haskellparsec.html

module LearningParsec.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

generateS_exp :: String -> String -> String -> String
generateS_exp op left right = "(" ++ op ++ " " ++ left ++ " " ++ right ++ ")"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser(emptyDef)

symbol = Token.symbol lexer
number = Token.natural lexer
ident = Token.identifier lexer

parseInfix = (do {exp <- psExp; return exp})

psExp = (do {left <- psTerm; exp <- (psExpStar left); return exp})
psExpStar left = (do {symbol "+";
                      right <- psTerm;
                      append <- psExpStar (generateS_exp "+" left right);
                      return append})
             <|> (do {symbol "-";
                      right <- psTerm;
                      append <- psExpStar (generateS_exp "+" left right);
                      return append})
             <|> return left

psTerm = (do {left <- psFactor; exp <- (psTermStar left); return exp})
psTermStar left = (do {symbol "*";
                        right <- psFactor;
                        append <- psTermStar (generateS_exp "*" left right);
                        return append})
              <|> (do {symbol "/";
                        right <- psFactor;
                        append <- psTermStar (generateS_exp "/" left right);
                        return append})
              <|> return left

psFactor = (do {obj <- number; return (show obj)})
      <|> (do {obj <- ident; return obj})
      <|> (do {symbol "(";
                obj <- psExp;
                symbol ")";
                return obj})

convertI2S s = parse parseInfix "i2s" s
