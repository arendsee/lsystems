module Lsystem.Grammar
(
    System(..)
  , Node(..)
  , Rule(..)
  , Distance
  , Yaw
  , Pitch
  , Roll
  , LeftContext
  , RightContext
  , Chance
) where

-- context-sensitive stochastic parameterized L-systems

type Distance = Double -- x > 0
type Yaw      = Double -- -180 < x < 180
type Pitch    = Double -- -180 < x < 180
type Roll     = Double -- -180 < x < 180

type LeftContext  = [Node]
type RightContext = [Node]

type Chance = Double -- 0 <= x <= 1

data System = System {
      systemBasis :: [Node]
    , systemRules :: [Rule]
    , systemSteps :: Int
  }

data Node
  = NodeDraw [Double] Double 
  | NodeRotate [Double] Yaw Pitch Roll
  | NodeBranch [[Node]]
  deriving(Eq, Ord, Show)

data Rule
  = DeterministicRule {
        ruleContext     :: LeftContext -> RightContext -> Bool
      , ruleCondition   :: LeftContext -> RightContext -> Node -> Bool
      , ruleMatch       :: Node -> Bool
      , ruleReplacement :: [Node]
    }
  | StochasticRule [(Chance, Rule)]

