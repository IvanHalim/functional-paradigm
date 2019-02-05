module MiniLogo where

import Data.List

-- MiniLogo is toy version of the Logo language for programming simple 2D graphics.
-- A MiniLogo program describes a graphic by a sequence of move commands that move
-- a pen from one position to another on a Cartesian plane, drawing lines as it goes.

-- For example, here is a MiniLogo program that draws a 2x2 square with its bottom-left corner at the origin.

-- pen up; move (0,0);
-- pen down; move (2,0); move (2,2);
--           move (0,2); move (0,0);

-- Conceptually, the MiniLogo execution environment consists of two parts:
-- 1. a canvas rooted at position (0,0) and extending infinitely upward and to the right
-- 2. a pen which is always located at a certain position on the canvas, and which can be in one of two states, either up or down

-- The move command moves the position of the pen from one position to another:
-- 1. If the pen is down when it moves, it draws a straight line connecting the two positions.
-- 2. If the pen is up, it just moves to the new position but does not draw a line.

-- The state of the pen can be changed using the pen command as illustrated in the example program above.

-- In addition to basic pen and move commands, a MiniLogo program can define and invoke macros.
-- A macro is a procedure that takes some coordinate values as inputs and performs some commands.
-- Within the body of a macro, commands can refer to input values by name. For example,
-- here is the definition of a macro that draws a 2x2 square starting from an arbitrary origin.

-- define square (x,y) {
--   pen up; move (x,y);
--   pen down; move (x+2,y); move (x+2,y+2);
--             move (x,y+2); move (x,y);
-- }

-- Now, if I want to draw two squares in two different places, later in my program I can call the macro with two different sets of arguments.
-- The following will draw two 2x2 squares, one anchored at position (3,5), the other anchored at (13,42).

-- call square (3,5); call square (13,42);

-- Notice in the definition of the square macro, that we can also perform addition on coordinate values.



--------------------------
-- * Syntax of MiniLogo
--------------------------

-- Grammar for MiniLogo:
--
--      num ::= (any natural number)
--      var ::= (any variable name)
--    macro ::= (any macro name)
--
--     prog ::= cmd*
--     mode ::= `up` | `down`
--     expr ::= var
--            | num
--            | expr + expr
--      cmd ::= pen mode
--            | move (expr, expr)
--            | define macro (var*) {prog}
--            | call macro (expr*)

-- 1 Encode the above grammar as a set of Haskell data types

data Mode = Up | Down
    deriving (Eq,Show)

data Expr
    = Variable Var
    | Lit Int
    | Add Expr Expr
    deriving (Eq,Show)

data Cmd
    = Pen Mode
    | Move (Expr, Expr)
    | Define Macro [Var] Prog
    | Call Macro [Expr]
    deriving (Eq,Show)

type Prog  = [Cmd]
type Macro = String
type Var   = String


-- 2 Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on
-- the canvas) draws a line segment from (x1,y1) to (x2,y2).
--
-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
--     pen up;
-- }

line :: Cmd
line = Define "line" ["x1","y1","x2","y2"]
        [Pen Up,
         Move (Variable "x1", Variable "y1"),
         Pen Down,
         Move (Variable "x2", Variable "y2"),
         Pen Up]


-- 3 Use the line macro to define a new MiniLogo macro nix (x,y,w,h) that
-- draws a big “X” of width w and height h, starting from position (x,y).
--
-- define nix (x, y, w, h) {
--   line(x, y, x + w, y + h);
--   line(x, y + h, x + w, y);
-- }

nix :: Cmd
nix = Define "nix" ["x","y","w","h"] [
    Call "line" [Variable "x",
                 Variable "y",
                 Add (Variable "x") (Variable "w"),
                 Add (Variable "y") (Variable "h")],
    Call "line" [Variable "x",
                 Add (Variable "y") (Variable "h"),
                 Add (Variable "x") (Variable "w"),
                 Variable "y"]]


-- 4 Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo
-- program that draws a staircase of n steps starting from (0,0).

steps :: Int -> Prog
steps 0 = []
steps n =
    [Call "line" [Lit n, Lit n, Lit (n-1), Lit n],
     Call "line" [Lit (n-1), Lit n, Lit (n-1), Lit (n-1)]]
    ++ steps (n-1)


-- 5 Define a Haskell function macros :: Prog -> [Macro] that returns a list of
-- the names of all of the macros that are defined anywhere in a given MiniLogo
-- program.

macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
    Define m _ _ -> m : macros xs
    otherwise    -> macros xs


-- 6 Define a Haskell function pretty :: Prog -> String that pretty-prints a
-- MiniLogo program. That is, it transforms the abstract syntax (a Haskell
-- value) into nicely formatted concrete syntax (a string of characters).

prettyExpr :: Expr -> String
prettyExpr (Variable v) = v
prettyExpr (Lit x)      = show x
prettyExpr (Add l r)    = prettyExpr l ++ " + " ++ prettyExpr r

pretty :: Prog -> String
pretty [] = ""
pretty (x:xs) = case x of
    Pen Up       -> "pen up;\n" ++ pretty xs
    Pen Down     -> "pen down;\n" ++ pretty xs
    Move (a,b)   -> "move (" ++ prettyExpr a ++ ", " ++ prettyExpr b ++ ");\n" ++ pretty xs
    Define m v p -> "define " ++ m ++ " (" ++ (intercalate ", " v) ++ ") {\n" ++ pretty p ++ "}\n" ++ pretty xs
    Call m e     -> "call " ++ m ++ " (" ++ (intercalate ", " (map prettyExpr e)) ++ ");\n" ++ pretty xs


-- 7 Define a Haskell function optE :: Expr -> Expr that partially evaluates
-- expressions by replacing any additions of literals with the result. For
-- example, given the expression (2+3)+x, optE returns the expression 5+x.

optE :: Expr -> Expr
optE (Variable v) = Variable v
optE (Lit x)      = Lit x
optE (Add l r)    = case l of
    Lit m     -> case r of
        Lit n     -> Lit (m + n)
        otherwise -> Add (optE l) (optE r)
    otherwise -> Add (optE l) (optE r)


-- 8 Define a Haskell function optP :: Prog -> Prog that optimizes all of the
-- expressions contained in a given program using optE.

optP :: Prog -> Prog
optP [] = []
optP (x:xs) = case x of
    Pen _        -> x : optP xs
    Move (a,b)   -> Move (optE a, optE b) : optP xs
    Define m v p -> Define m v (optP p) : optP xs
    Call m e     -> Call m (map optE e) : optP xs