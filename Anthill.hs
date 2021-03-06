import System.Environment

import qualified Parser as P
import qualified Evaluator as E
import qualified Codegen as C

main = do
	args <- getArgs
	s <- readFile (head args)
	if (length args > 1)
	then C.genHtmlFile (E.evalProgram $ P.parseAH s) (args !! 1)
	else C.genHtmlFile (E.evalProgram $ P.parseAH s) "anthill.html"

parseFile :: String -> IO ()
parseFile f = do
	s <- readFile f
	print $ P.parseAH s

evalFile :: String -> IO ()
evalFile f = do
	s <- readFile f
	print $ E.evalProgram $ P.parseAH s

--scanl for each ant, print an HtmlFile with a number appended
--incrementalDraw

--repl = forever $ do
--	putStr "anthill> "
--	st <- getLine
--	putStrLn (E.eval $ P.parse st)
