module Codegen where

import Evaluator
import Parser

data HtmlNode = HtmlNode String String [HtmlNode] | String

canvStart = HtmlNode "<canvas id=\"anthill\" width=\"800\" height=\"600\">" "</canvas>" []

printHtml :: HtmlNode -> String
printHtml (HtmlNode n1 n2 l) = n1 ++ "\n" ++ (concat $ map printHtml l) ++ "\n" ++ n2

addNode :: HtmlNode -> HtmlNode -> HtmlNode
addNode (HtmlNode n1 n2 l1) h = (HtmlNode n1 n2 (l1 ++ [h]))

genCode :: Ant -> String
genCode a = printHtml canvStart ++ "\n<script>var c = document.getElementById(\"anthill\");\n var anthill = c.getContext(\"2d\");\n" ++ (concat $ map genLine (canvas a)) ++ "</script>"

genLine :: (Pos, Pos) -> String
genLine (p1,p2) = "anthill.moveTo" ++ (show p1) ++ ";\n" ++ "anthill.lineTo" ++ (show p2) ++ ";\n" ++ "anthill.stroke();\n"

genHtmlFile :: Ant -> IO ()
genHtmlFile a = writeFile "anthill.html" (genCode a)

testProg = evalProgram [
	 LogoCommand Pendown
	,LogoCommand (Turn (LogoPrim (LogoNum 90)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 90)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 90)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 90)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	]

triangleProg = evalProgram [
	 LogoCommand Pendown
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	,LogoCommand (Turn (LogoPrim (LogoNum 60)))
	,LogoCommand (Forward (LogoPrim (LogoNum 50)))
	]
