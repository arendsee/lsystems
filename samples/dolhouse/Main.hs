import Lsystem

main :: IO ()
main = renderSystem (400,400) "3_90_F-F+F-F+F.svg" s1

-- F-F+F-F+F
s1Repl :: [Node]
s1Repl = [
      oF    -- F
    , o90   -- -
    , oF    -- F
    , o270  -- +
    , oF    -- F
    , o90   -- -
    , oF    -- F
    , o270  -- +
    , oF    -- F
  ]

s1 :: System
s1 = System {
      systemBasis = oSquare
    , systemRules = [fromF s1Repl]
    , systemSteps = 3
  }


-- 2 90 F-F-F-F F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F
--
-- 3 90 F-F-F-F FF-F--F-F
--
-- 3 90 F-F-F-F F-F-F+F+F-F-F+F+F
--
-- 2 90 F-F-F-F FF+F+F-FFF-FFF-FFF-F+F+FF
--
-- 3 90 F-F-F-F F+F+F-F-F+F-F-F+F-F-F+F+F
--
-- 4 90 F-F-F-F FF-F+F+F-F
--
-- 4 90 F-F-F-F FF-F-F+F+F-F+F+F-F+F+F-F-FF
--
-- 5 90 F F+F-F-F+F
