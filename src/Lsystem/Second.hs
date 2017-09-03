-- context-sensitive stochastic parameterized L-systems

type Distance = Double -- x > 0
type Yaw      = Double -- -180 < x < 180
type Pitch    = Double -- -180 < x < 180
type Roll     = Double -- -180 < x < 180

type LeftContext  = [N]
type RightContext = [N]

type Chance = Double -- 0 <= x <= 1

data Rule
  = DeterministicRule {
        ruleContext     :: LeftContext -> RightContext -> Bool
      , ruleCondition   :: LeftContext -> RightContext -> N -> Bool
      , ruleMatch       :: N -> Bool
      , ruleReplacement :: [N]
    }
  | StochasticRule [(Chance, Rule)]

data N
  = NDraw [Double] Double 
  | NRotate [Double] Yaw Pitch Roll
  deriving(Eq, Ord, Show)

data System = System {
      instr :: [N]
    , rules :: [Rule]
    , angle :: Double
    , steps :: Int
  }

ignoreContext :: LeftContext -> RightContext -> Bool
ignoreContext _ _ = True

unconditional :: LeftContext -> RightContext -> N -> Bool
unconditional _ _ _ = True

isF :: N -> Bool
isF (NDraw _ _) = True
isF _ = False

is90 :: N -> Bool
is90 (NRotate _ 90 0 0) = True
is90 _ = True

is270 :: N -> Bool
is270 (NRotate _ 270 0 0) = True
is270 _ = True

fromF :: [N] -> Rule
fromF repl =
  DeterministicRule {
      ruleContext     = ignoreContext
    , ruleCondition   = unconditional
    , ruleMatch       = isF
    , ruleReplacement = repl
  }

oF :: N
oF = NDraw [] 1

o90 :: N
o90 = NRotate [] 90  0 0

o270 :: N
o270 = NRotate [] 270 0 0

oSquare :: [N]
oSquare = [oF, o90, oF, o90, oF, o90, oF, o90, oF] ---  F-F-F-F



-- TODO replace with a real random choice function
choose :: [(Chance, a)] ->  Maybe a
choose ((_,x):_) = Just x
choose _ = Nothing

applyRules :: LeftContext -> RightContext -> [Rule] -> N -> [N]
applyRules lc rc []     n = [n]
applyRules lc rc (r:rs) n = case applyRule n lc rc r of
  Nothing -> applyRules lc rc rs n
  Just x  -> x
  where
    applyRule :: N -> LeftContext -> RightContext -> Rule -> Maybe [N]
    applyRule n lc rc (DeterministicRule cont cond match repl) =
      case (match n && cont lc rc && cond lc rc n) of
        True  -> Just repl
        False -> Nothing
    applyRule n lc rc (StochasticRule rs) = case (choose rs) of
      Just r -> applyRule n lc rc r
      Nothing -> Nothing

-- this will need to be refitted to allow branching later
next :: [N] -> [Rule] -> [N]
next [] rs = []
next (x:xs) rs = concat $ next' rs [] xs where
  next' :: [Rule] -> [N] -> [N] -> [[N]]
  next' _  _  []     = []
  next' rs lc [r]    = [applyRules lc [] rs r]
  next' rs lc (r:rc) = [applyRules lc rc rs r] ++ next' rs (r:lc) rc
