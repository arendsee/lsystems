module Lsystem.Grammar
(
    System(..)
  , Node(..)
  , GeneralRule(..)
  , Rule
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
  | NodeDummy String
  deriving(Eq, Ord, Show)

type Rule = GeneralRule Node
data GeneralRule a
  = DeterministicRule {
        ruleContext     :: [a] -> [a] ->      ([a] -> Maybe [a])
      , ruleCondition   :: [a] -> [a] -> a -> ([a] -> Maybe [a])
      , ruleMatch       ::               a -> ([a] -> Maybe [a])
      , ruleReplacement :: [a] -> [a] -> a -> [a]
    }
  | StochasticRule [(Chance, Rule)]
