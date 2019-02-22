-- | A single register imperative language.
module While where


--
-- * Syntax
--

--  Before refactoring:
--
--    int  ::= (any integer)
--
--    expr ::= `R`                  -- load from register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--          |  expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` expr stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block
--
--  After refactoring:
--
--    int  ::= (any integer)
--
--    expr ::= `R`                  -- load from register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--
--    test ::= expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` test stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block

data Expr
   = Get
   | Lit Int
   | Add Expr Expr
  deriving (Eq,Show)

data Test
   = LTE Expr Expr
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | While Test Stmt
   | Begin [Stmt]
  deriving (Eq,Show)


-- Example program:
--   begin
--     R := 1
--     while R <= 100
--       R := R + R
--   end
p :: Stmt
p = Begin [
      Set (Lit 1),
      While (LTE Get (Lit 100))
        (Set (Add Get Get))
    ]


--
-- * Semantics
--

-- Semantic domains, pre-refactoring:
-- expr :: Expr -> Reg -> Maybe (Either Int Bool)
-- stmt :: Stmt -> Reg -> Maybe Reg

-- | The current value of the register.
type Reg = Int

-- | Valuation function for expressions.
expr :: Expr -> Reg -> Int
expr Get       = \s -> s
expr (Lit i)   = \s -> i
expr (Add l r) = \s -> expr l s + expr r s

-- | Valuation function for tests.
test :: Test -> Reg -> Bool
test (LTE l r) = \s -> expr l s <= expr r s

-- | Non-compositional valuation function for statements.
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     = \s -> expr e s
stmt (While c b) = \s -> if test c s
                         then stmt (While c b) (stmt b s)
                         else s
stmt (Begin l)   = \s -> stmts l s
-- stmt (Begin l)   = foldl (flip stmt) s l

stmts :: [Stmt] -> Reg -> Reg
stmts []     = \r -> r
stmts (s:ss) = \r -> stmts ss (stmt s r)



-- ** Regaining compositionality

-- | Compute least fix point. Defined in Data.Function.
fix f = let x = f x in x

-- | Compositional valuation function for statements using least fix point.
stmt' :: Stmt -> Reg -> Reg
stmt' (Set e)     = \s -> expr e s
stmt' (While c b) = fix (\f s -> if test c s
                                 then f (stmt' b s)
                                 else s)
stmt' (Begin l)   = \s -> foldl (flip stmt') s l