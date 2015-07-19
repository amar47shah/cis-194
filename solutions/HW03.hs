module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Needed for testing
instance Show (a -> b) where
  show _ = "function"

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s var val var'
 | var' == var = val
 | otherwise   = s var'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE s (Var var)      = s var
evalE _ (Val val)      = val
evalE s (Op lhs b rhs) = let e = evalE s lhs in
  case b of
      Plus   -> (e +)
      Minus  -> (e -)
      Times  -> (e *)
      Divide -> (e `div`)
      Gt     -> boolToInt . (e >)
      Ge     -> boolToInt . (e >=)
      Lt     -> boolToInt . (e <)
      Le     -> boolToInt . (e <=)
      Eql    -> boolToInt . (e ==)
  $ evalE s rhs

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v e)    = DAssign v e
desugar (Incr v)        = DAssign v (Op (Var v) Plus (Val 1))
desugar (If e s s')     = DIf e (desugar s) (desugar s')
desugar (While e s)     = DWhile e (desugar s)
desugar (For i e u s)   =
  DSequence (desugar i) (DWhile e (DSequence (desugar s) (desugar u)))
desugar (Sequence s s') = DSequence (desugar s) (desugar s')
desugar  Skip           = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign v e)    = extend s v $ evalE s e
evalSimple s (DIf e d d')     = if trueE s e
                                then evalSimple s d
                                else evalSimple s d'
evalSimple s w@(DWhile e d)   = if trueE s e
                                then evalSimple s (DSequence d w)
                                else evalSimple s DSkip
evalSimple s (DSequence d d') = evalSimple (evalSimple s d) d'
evalSimple s  DSkip           = s

run :: State -> Statement -> State
run = (. desugar) . evalSimple

trueE :: State -> Expression -> Bool
trueE = ((0 /=) .) . evalE

-- Programs -------------------------------------------

exec :: Statement -> Int -> Int
exec statement input = finalState "Out"
    where finalState = run initialState statement
          initialState = extend empty "In" input

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   Out := 0;
   while (In >= Out * Out) {
     Out++
   };
   Out := Out - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
