import Prelude hiding (Num)
import OperationalSemantics

data Inst = Push Num | Add | Mult | Sub | True | False | Eq | Le | And | Neg | Fetch Var | Store Var |
            Noop | Branch Code Code | Loop Code Code
            deriving (Show)

type Code = [Inst]

data Expression = INT Z | BOOL T 
    deriving(Show)

type Stack = [Expression]

data Config = Config Code Stack State

am_step :: Config -> Config
am_step (Config [] e s) = (Config [] e s)