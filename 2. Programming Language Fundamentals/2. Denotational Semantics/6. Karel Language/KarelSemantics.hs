module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown       _ _ r = Done r
stmt PickBeeper     _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move           _ w r = let p = relativePos Front r
                        in if test (Clear Front) w r
                              then OK w (setPos p r)
                              else Error ("Blocked at: " ++ show p)
stmt PutBeeper      _ w r = let p = getPos r
                        in if test (Not Empty) w r
                              then OK (incBeeper p w) (decBag r)
                              else Error ("No beeper to put.")
stmt (Turn d)       _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block ss)     ds w r = case ss of
                                []    -> OK w r
                                (h:t) -> case stmt h ds w r of
                                            OK w' r'  -> stmt (Block t) ds w' r'
                                            otherwise -> stmt h ds w r
stmt (If t s1 s2)   ds w r = if test t w r
                                then stmt s1 ds w r
                                else stmt s2 ds w r
stmt (Call m)       ds w r = case lookup m ds of
                                Just s  -> stmt s ds w r
                                Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s)  ds w r = case i of
                                0 -> OK w r
                                _ -> case stmt s ds w r of
                                        OK w' r'  -> stmt (Iterate (i-1) s) ds w' r'
                                        otherwise -> stmt s ds w r
stmt (While t s)    ds w r = if test t w r
                                then case stmt s ds w r of
                                    OK w' r'  -> stmt (While t s) ds w' r'
                                    otherwise -> stmt s ds w r
                                else OK w r


-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
