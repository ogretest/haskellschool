
-- Exercise 1

type State = (String -> Int)

extend :: State -> String -> Int -> State
extend s n v = \ x -> if x == n then v else s x

empty :: State
empty = (\ _ -> 0)

-- Exercise 2

data Bops = Eql | Le | Plus | Mul deriving Eq

data Expression = Val Int | Op Expression Bops Expression | Var String

evalE :: State -> Expression -> Int
evalE s (Val v) = v
evalE s (Var n) = s n
evalE s (Op l o r)
  | o == Eql = if lv == rv then 1 else 0
  | o == Le = if lv <= rv then 1 else 0
  | o == Plus = lv + rv
  | o == Mul = lv * rv
  where
    lv = evalE s l
    rv = evalE s r

-- Exercise 3

data Statement
  = Assign String Expression
  | Incr String
  | Block Statement Statement
  | For Statement Expression Statement Statement

data DietStatement 
  = DAssign String Expression
  | DBlock DietStatement DietStatement
  | DLoop Expression DietStatement

desugar :: Statement -> DietStatement
desugar (Block a b) = DBlock (desugar a) (desugar b)
desugar (Assign n e) = DAssign n e
desugar (Incr n) = DAssign n (Op (Var n) Plus (Val 1))
desugar (For i c l st) = DBlock (desugar i) (DLoop c (DBlock (desugar st) (desugar l)))

-- Exercise 4

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign n e) = extend s n (evalE s e)
evalSimple s (DBlock b n) = evalSimple (evalSimple s b) n
evalSimple s (DLoop c b) = if evalE s c == 1 then evalSimple s' (DLoop c b) else s
  where
    s' = evalSimple s b

run :: State -> Statement -> State
run s st = evalSimple s (desugar st)

-- Exercise 5

factorial :: Statement
factorial = (Block (Assign "Out" (Val 1)) (For (Assign "I" (Val 1)) (Op (Var "I") Le (Var "In" )) (Incr "I") (Assign "Out" (Op (Var "I") Mul (Var "Out")))))
