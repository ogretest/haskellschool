
-- Exercise 1

type State = (String -> Int)

extend :: State -> String -> Int -> State
extend s n v = \ x -> if x == n then v else s x

empty :: State
empty = (\ _ -> 0)

-- Exercise 2

data Bops
  = Eql
  | Le
  | Plus
  | Minus
  | Times deriving Eq

data Expression = Val Int | Op Expression Bops Expression | Var String

evalE :: State -> Expression -> Int
evalE s (Val v) = v
evalE s (Var n) = s n
evalE s (Op l o r)
  | o == Eql = if lv == rv then 1 else 0
  | o == Le = if lv <= rv then 1 else 0
  | o == Plus = lv + rv
  | o == Minus = lv - rv
  | o == Times = lv * rv
  where
    lv = evalE s l
    rv = evalE s r

-- Exercise 3

data Statement
  = Skip
  | Assign String Expression
  | Incr String
  | Decr String
  | Sequence Statement Statement
  | If Expression Statement Statement
  | For Statement Expression Statement Statement

data DietStatement 
  = DSkip
  | DAssign String Expression
  | DSequence DietStatement DietStatement
  | DIf Expression DietStatement DietStatement
  | DLoop Expression DietStatement

desugar :: Statement -> DietStatement
desugar Skip = DSkip
desugar (Sequence a b) = DSequence (desugar a) (desugar b)
desugar (Assign n e) = DAssign n e
desugar (Incr n) = DAssign n (Op (Var n) Plus (Val 1))
desugar (Decr n) = DAssign n (Op (Var n) Minus (Val 1))
desugar (If c a b) = DIf c (desugar a) (desugar b)
desugar (For i c l st) = DSequence (desugar i) (DLoop c (DSequence (desugar st) (desugar l)))

-- Exercise 4

evalSimple :: State -> DietStatement -> State
evalSimple s DSkip = s
evalSimple s (DAssign n e) = extend s n (evalE s e)
evalSimple s (DSequence b n) = evalSimple (evalSimple s b) n
evalSimple s (DLoop c b) = if evalE s c == 1 then evalSimple s' (DLoop c b) else s
  where
    s' = evalSimple s b
evalSimple s (DIf c a b) = if evalE s c == 1 then evalSimple s a else evalSimple s b

run :: State -> Statement -> State
run s st = evalSimple s (desugar st)

-- Exercise 5

factorial :: Statement
factorial =
  (Sequence
    (Assign "I" (Val 1))
    (For
        (Assign "Out" (Val 1))
      (Op (Var "I") Le (Var "In" ))
      (Incr "I")
      (Assign "Out" (Op (Var "I") Times (Var "Out")))))

squareRoot :: Statement
squareRoot =
  (Sequence
    (For
      (Assign "Out" (Val 1))
      (Op (Op (Var "Out") Times (Var "Out")) Le (Var "In"))
      (Incr "Out")
      Skip)
    (Decr "Out"))

fibonacci :: Statement
fibonacci =
  (If (Op (Var "In") Le (Val 1))
    (Assign "Out" (Val 1))
    (For
      (Sequence
        (Assign "I" (Val 2))
        (Sequence
          (Assign "Out" (Val 1))        
          (Assign "Prev" (Val 1))))
      (Op (Var "I") Le (Var "In" ))
      (Incr "I")
      (Sequence
        (Assign "Temp" (Var "Out"))
        (Sequence
          (Assign "Out" (Op (Var "Out") Plus (Var "Prev")))
          (Assign "Prev" (Var "Temp"))))))
