module Evaluator where

import Parser

type Pos = (Int, Int)

data PenState = Down | Up
	deriving (Show, Eq)

data Ant = Ant {  turtlePos :: Pos
			, turtleAngle :: Int
			, penState :: PenState
			, maxX :: Int
			, maxY :: Int
			, minX :: Int
			, minY :: Int
			, canvas :: [(Pos,Pos)] }
				deriving (Show)

defaultAnt :: Ant
defaultAnt = Ant (120,120) 0 Up 0 0 0 0 [] 

--turnAnt :: LogoPrim -> Ant -> Ant

evalPrim :: LogoPrim -> LogoPrim
evalPrim p = case p of
	(LogoNum n) -> LogoNum n
	(LogoBool b) -> LogoBool b
	(LogoChar c) -> LogoChar c
	(LogoList l) -> LogoList (map evalExpr l)

moveForward :: Pos -> Int -> Int -> Pos
moveForward (x, y) angle mov = (newX, newY)
	where
		newX = round $ (fromIntegral x) + (fromIntegral mov) * sin ((pi/180) * (fromIntegral angle))
		newY = round $ (fromIntegral y) + (fromIntegral mov) * cos ((pi/180) * (fromIntegral angle))

evalExpr :: LogoExpr -> LogoExpr
evalExpr e = case e of
	(LogoPrim p) -> LogoPrim $ evalPrim p
	(LogoFunc f a) -> LogoFunc f (map evalExpr a)

evalMove :: Ant -> LogoCommand -> Ant
evalMove a@(Ant pos angle pens maxx maxy minx miny canv) c = case c of
	(Forward (LogoPrim (LogoNum n))) -> if ((penState a) == Down)
					then (Ant nextPos angle pens maxx maxy minx miny (canv ++ [(pos, nextPos)]))
					else a
						where
							nextPos = moveForward pos angle n
	(Backward e) -> if ((penState a) == Down)
					then (Ant nextPos angle pens maxx maxy minx miny (canv ++ [(pos, nextPos)]))
					else a
						where
							nextPos = (1,1)--moveForward pos angle n

turn :: LogoExpr -> Int -> Int
turn (LogoPrim (LogoNum n)) c = n + c

evalCommand :: Ant -> LogoCommand -> Ant
evalCommand a@(Ant pos an pen maxx maxy minx miny canv) s = case s of
	(Forward e) -> evalMove a (Forward (evalExpr e))
	(Backward e) -> evalMove a (Backward (evalExpr e))
	(Turn e) -> (Ant pos (turn (evalExpr e) an) pen maxx maxy minx miny canv)
	(Pendown) -> (Ant pos an Down maxx maxy minx miny canv)
	(Penup) -> (Ant pos an Up maxx maxy minx miny canv)

evalStmt :: Ant -> LogoStmt -> Ant
evalStmt a s = case s of
	(LogoExpr e) -> a --LogoExpr $ evalExpr e
	(LogoCommand c) -> (evalCommand a c)

stringify :: LogoExpr -> String
stringify e = "y"

evalProgram :: [LogoStmt] -> Ant
evalProgram p = foldl evalStmt defaultAnt p
