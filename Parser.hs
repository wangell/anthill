{-# LANGUAGE OverloadedStrings #-}

module Parser
( parseAH
, LogoPrim(..)
, LogoExpr(..)
, LogoCommand(..)
, LogoStmt(..)
) where

import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Control.Applicative
import Control.Monad
import qualified Data.List
import qualified Data.ByteString.Char8 as B

data LogoPrim = LogoNum Int | LogoChar Char | LogoBool Bool | LogoList [LogoExpr] 
	deriving (Show, Eq)

--LogoFunc always returns an expression
data LogoExpr = LogoPrim LogoPrim | LogoFunc String [LogoExpr]
	deriving (Show, Eq)

data LogoCommand = Forward LogoExpr | Backward LogoExpr | Turn LogoExpr | Pendown | Penup | Print LogoExpr
	deriving (Show, Eq)

--LogoCommand doesn't return anything
data LogoStmt = LogoExpr LogoExpr | LogoCommand LogoCommand
	deriving (Show, Eq)

--- Expresion Parsing
parseNumber :: Parser LogoPrim
parseNumber = do
	d <- decimal
	return $ LogoNum d

parseChar :: Parser LogoPrim
parseChar = do
	char '\''
	c <- letter_ascii
	char '\''
	return $ LogoChar c

parseBool :: Parser LogoPrim
parseBool = (string "#t" >> return (LogoBool True)) <|> (string "#f" >> return (LogoBool False))

--Fix spacing issue
parseList :: Parser LogoPrim
parseList = do
	char '['
	skipSpace
	elements <- parseExpr `sepBy` (char ',')
	skipSpace
	char ']'
	return $ LogoList elements

parseString :: Parser LogoPrim
parseString = do
	char '"'
	elements <- Data.Attoparsec.Char8.takeWhile (/='\"')
	char '"'
	return $ LogoList (map (LogoPrim . LogoChar) (B.unpack elements))

parsePrim :: Parser LogoExpr
parsePrim = do
	p <- (parseChar <|> parseList <|> parseString <|> parseBool <|> parseNumber)
	return $ LogoPrim p

--Fix spacing issue
parseFunc :: Parser LogoExpr
parseFunc = do
	func <- Data.Attoparsec.Char8.takeWhile (/=' ')
	space
	args <- parseExpr `sepBy` (space)
	return $ LogoFunc (B.unpack func) args

parseExpr :: Parser LogoExpr
parseExpr = parsePexpr <|> parsePrim <|> parseFunc

parsePexpr :: Parser LogoExpr
parsePexpr = do
	char '('
	e <- parseExpr
	char ')'
	return e

--- Statement Parsing

parseCommand :: Parser LogoCommand
parseCommand = 
	(do
	string "forward"
	space
	e <- parseExpr
	return $ Forward e) <|>
	(do
	string "backward"
	space
	e <- parseExpr
	return $ Backward e) <|>
	(do
	string "turn"
	space
	e <- parseExpr
	return $ Turn e) <|>
	(do
	string "pendown"
	return $ Pendown) <|>
	(do
	string "penup"
	return $ Penup) <|>
	(do
	string "print"
	space
	e <- parseExpr
	return $ Print e)

parseStmt :: Parser LogoStmt
parseStmt = (do 
	skipSpace
	c <- parseCommand
	return $ LogoCommand c) <|> 
	(do 
	e <- parseExpr
	skipSpace
	--char ';'
	return $ LogoExpr e)

parseProg :: Parser [LogoStmt]
parseProg = many $ parseStmt <* (char ';')

parseAH :: String -> [LogoStmt]
parseAH s = case (parseOnly parseProg (B.pack s)) of
	Right e -> e
	Left _ -> []
