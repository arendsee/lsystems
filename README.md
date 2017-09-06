## L-systems

My goal is to implement all features of the L-system described in 'Algorithmic
Beauty of Plants' (ABOP). The input will be from files using the grammar
described in ABOP. The result will be either an SVG (for 2D models) or
something else for 3D (I haven't settled on a format yet, but should be
something that can be read by Blender).

## Examples

### DOL-systems

The data structures I used to represent the L-systems are very general. They
allow stochastic, context-sensitive, parametric L-systems (the algorithms and
graphics for these higher forms aren't implemented yet). But expressing systems
in this general format is extremely verbose, so I wrote the special handler
`transDolSys`.

As I implement the higher features, I will probably add similar convenience
functions. Ultimately, I would like to have a full parser for the L-system
grammar used in ABOP.

``` haskell
import Lsystem
import System.Random

render' :: String -> System -> IO ()
render' = renderSystem (mkStdGen 42) (400,400)

main :: IO ()
main = do
  render' "d1.svg" $ transDolSys 3 90 "F-F-F-F" "F-F+F-F+F"
  render' "d2.svg" $ transDolSys 2 90 "F-F-F-F" "F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F"
  render' "d3.svg" $ transDolSys 3 90 "F-F-F-F" "FF-F--F-F"
  render' "d4.svg" $ transDolSys 3 90 "F-F-F-F" "F-F-F+F+F-F-F+F+F"
  render' "d5.svg" $ transDolSys 2 90 "F-F-F-F" "FF+F+F-FFF-FFF-FFF-F+F+FF"
  render' "d6.svg" $ transDolSys 3 90 "F-F-F-F" "F+F+F-F-F+F-F-F+F-F-F+F+F"
  render' "d7.svg" $ transDolSys 4 90 "F-F-F-F" "FF-F+F+F-F"
  render' "d8.svg" $ transDolSys 4 90 "F-F-F-F" "FF-F-F+F+F-F+F+F-F+F+F-F-FF"
  render' "d9.svg" $ transDolSys 5 90 "F"       "F+F-F-F+F"
```

![d1](images/d1.png)
![d2](images/d2.png)
![d3](images/d3.png)
![d4](images/d4.png)
![d5](images/d5.png)
![d6](images/d6.png)
![d7](images/d7.png)
![d8](images/d8.png)
![d9](images/d9.png)


### Stochastic systems

L-systems can be made stochastic be adding probabilistic rules. A probabilistic
rule is a set of rules associated with probabilities, where the probabilities
must some to a number less than or equal to 1 (with the implicit default rule
of N -> N making up the difference).


``` haskell
import Lsystem
import System.Random

render' :: String -> System -> IO ()
render' = renderSystem (mkStdGen 42) (400,400)

s1 = transSolSys 12 45 "F" [(0.4, "FF"), (0.5, "-F"), (0.1, "FF-FF-FF-FF")]

main :: IO ()
main = do
  render' "s1.svg" s1
```

![s1](images/s1.png)


### Branching

I currently don't have any sugar for branching. So I will do this the hard way.

``` haskell
import Lsystem
import System.Random

render' :: String -> System -> IO ()
render' = renderSystem (mkStdGen 42) (400,400)

-- -- This example is taken from ABOP pp. 25
-- n=5 d=25.7
-- F -> F[+F]F[-F]F

b1 = System {
      systemBasis = [NodeDraw [] 1] -- F
    , systemRules = [               -- F[+F]F[-F]F
        DeterministicRule {
            ruleContext = ignoreContext
          , ruleCondition = unconditional
          , ruleMatch = matchF
          , ruleReplacement = [
                NodeDraw [] 1 ------------------ F
              , NodeBranch [[ ------------------ [+F]
                    NodeRotate [] (-25.7) 0 0
                  , NodeDraw [] 1
                ]]
              , NodeDraw [] 1 ------------------ F
              , NodeBranch [[ ------------------ [-F]
                    NodeRotate [] 25.7 0 0
                  , NodeDraw [] 1
                ]]
              , NodeDraw [] 1 ------------------ F
            ]
        }
      ]
    , systemSteps = 5
  }

main :: IO ()
main = do
  render' "b1.svg" b1
```

![b1](images/b1.png)


### Stochastic branching

Putting together the previous two examples, we can make more plant-like constructions

![sb1-1](images/sb1-1.png)
![sb1-2](images/sb1-2.png)
![sb1-3](images/sb1-3.png)
![sb1-4](images/sb1-4.png)
![sb1-5](images/sb1-5.png)


### Node rewriting and dummy variables


``` haskell
-- n=5 f=22.5
-- X
-- X -> F-[[X]+X]+F[+FX]-X
-- F -> FF

dummy = System {
      systemBasis = [x]
    , systemRules = [
        DeterministicRule {
            ruleContext = ignoreContext
          , ruleCondition = unconditional
          , ruleMatch = matchDummy "X"
          , ruleReplacement = [
                f, m                                   -- F-
              , NodeBranch [[NodeBranch [[x]], p, x]]  -- [[X]+X]
              , p, f                                   -- +F
              , NodeBranch [[p,f,x]]                   -- [+FX]
              , m, x                                   -- -X
            ]
        },
        DeterministicRule {
            ruleContext = ignoreContext
          , ruleCondition = unconditional
          , ruleMatch = matchF
          , ruleReplacement = [f,f]
        }
      ]
    , systemSteps = 5
  } where
  p = NodeRotate [] 22.5    0 0
  m = NodeRotate [] (-22.5) 0 0
  f = NodeDraw [] 1
  x = NodeDummy "X"
```

![dummy](images/dummy.png)

### Contextual L-systems

Currently something is broken and this produces no output. But it is late ...
I'll fix it later.

``` haskell
-- n=26 d=25.75
-- ignore  +-F
-- F1F1F1
--
-- 0 0 0 -> 0
-- 0 0 1 -> 1[+F1F1]
-- 0 1 0 -> 0
-- 0 1 1 -> 1
-- 1 0 0 -> 0
-- 1 0 1 -> 1F1
-- 1 1 0 -> 0
-- 1 1 1 -> 0
-- * - * -> +
-- * + * -> -

contextual = System {
      systemBasis = [f,b,f,b,f,b]
    , systemRules = [
          DeterministicRule c000          unconditional isA [a]
        , DeterministicRule c001          unconditional isA [a, NodeBranch [[ p,f,a,f,a ]]]
        , DeterministicRule c010          unconditional isA [a]
        , DeterministicRule c011          unconditional isA [b]

        , DeterministicRule c100          unconditional isA [a]
        , DeterministicRule c101          unconditional isA [a,f,a]
        , DeterministicRule c110          unconditional isA [a]
        , DeterministicRule c111          unconditional isA [a]

        , DeterministicRule ignoreContext unconditional isP [m]
        , DeterministicRule ignoreContext unconditional isM [p]
      ]
    , systemSteps = 26
  } where

  f = NodeDraw [] 1
  a = NodeDummy "0"
  b = NodeDummy "1"
  p = NodeRotate [] 25.75    0 0
  m = NodeRotate [] (-25.75) 0 0

  ss = [p,m,f]

  isA = matchDummy "0"
  isB = matchDummy "1"

  isP :: Node -> a -> Maybe a
  isP n x | n == p = Just x
          | otherwise = Nothing

  isM :: Node -> a -> Maybe a
  isM n x | n == m = Just x
          | otherwise = Nothing
  
  c000 = contextMatch ss [a] [a]
  c001 = contextMatch ss [a] [b]
  c010 = contextMatch ss [a] [a]
  c011 = contextMatch ss [a] [b]
  c100 = contextMatch ss [b] [a] 
  c101 = contextMatch ss [b] [b] 
  c110 = contextMatch ss [b] [a] 
  c111 = contextMatch ss [b] [b] 
```

## TODO

 - [x] deterministic
 - [x] stochastic
 - [x] branching
 - [x] dummy variables
 - [ ] context sensitive
 - [ ] parametric
 - [x] stochastic sugar
 - [x] deterministic sugar
 - [ ] branching sugar
 - [ ] parametric sugar
 - [ ] context sensitive sugar
 - [x] 2D visualization
 - [ ] 3D handling
 - [ ] 3D visualization
 - [ ] animation
 - [ ] polygons
 - [ ] colors and such
 - [ ] ABOP language parsing
