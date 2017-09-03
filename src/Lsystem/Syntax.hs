module Lsystem.Syntax
(
    System(..)
  , TurtleParam(..)
  , Production(..)
  , ApicalCmd(..)
  , MetaCmd(..)
  , Context(..) 
  , LhsElement(..)
  , apicalMap
  , metaMap
) where

data System = System {
      systemDepth :: Integer
    , systemAngle :: Double
    , systemTable :: [(String, String)]
    , systemParam :: TurtleParam
    , systemBasis :: [Char]
    , systemRules :: [Production]
  }
  deriving(Show, Eq, Ord)

data ArithmeticToken
  = ArithmeticTokenCon Double
  | ArithmeticTokenVar String
  | ArithmeticTokenAdd
  | ArithmeticTokenSub
  | ArithmeticTokenExp
  | ArithmeticTokenDiv
  | ArithmeticTokenMul
  | ArithmeticTokenRParen
  | ArithmeticTokenRParen

data LeftElement
  = LeftElementFunction String [String]
  | LeftElementVariable String 
  deriving(Show, Eq, Ord)

data RightElement
  = RightElementFunction String [[ArithmeticToken]]
  | RightElementVariable String 
  deriving(Show, Eq, Ord)

data LHS
  = LHSContextFree LeftElement
  | LHSContextDependent (Maybe LeftElement) LeftElement (Maybe LeftElement)

data RHS
  = RHSApicalCmd 
  | RHSMetaCmd
  | RHSVariable
  | RHSBranch
  | RHSNest

-- Currently there is only one parameter, but this may eventually include
-- things like rendering details, output filenames and the like. `ignore` is
-- special, though, since it affects the productions and is not something the
-- user should ever set.
data TurtleParam = TurtleParam {
    turtleParamIgnore :: [Char]
  }
  deriving(Show, Eq, Ord)

data Production = Production {
      productionChance  :: Double
    , productionFrom    :: LHS
    , productionTo      :: String
  } deriving(Show, Eq, Ord)

data ParameterizedCmd
  = ParameterizedCmdDrawForward Double -- F(x) where x > 0
  | ParameterizedCmdMoveForward Double -- f(x) where x > 0
  | ParameterizedCmdRotateXY    Double -- +(x)
  | ParameterizedCmdRotateXZ    Double -- &(x)
  | ParameterizedCmdRotateYZ    Double -- /(x)

-- Legal on LHS or RHS of production
data ApicalCmd
  = CmdDrawForward         -- F
  | CmdMoveForward         -- f
  | CmdDrawForwardL        -- l in ABOP: F_l (used in edge writing)
  | CmdDrawForwardR        -- r in ABOP: F_r
  | CmdNodeReplaceLeft     -- L
  | CmdNodeReplaceRight    -- R
  | CmdMoveForwardNoVertex -- G
  deriving(Show, Eq, Ord)

-- Legal only on the RHS of production
data MetaCmd
  = CmdTurnLeft            -- +
  | CmdTurnRight           -- -
  | CmdPitchUp             -- ^
  | CmdPitchDown           -- &
  | CmdRollLeft            -- \
  | CmdRollRight           -- /
  | CmdTurnAround          -- |
  | CmdRotateVertical      -- $
  | CmdStartBranch         -- [
  | CmdCompleteBranch      -- ]
  | CmdStartPolygon        -- {
  | CmdCompletePolygon     -- }
  | CmdRecordVertex        -- .
  | CmdMerge               -- ~
  | CmdIncrementColor      -- '
  | CmdCutoffBranch        -- %
  deriving(Show, Eq, Ord)

data Context
  = ContextLhs String
  | ContextRhs String
  deriving(Show, Eq, Ord)

data LhsElement
  = LhsElementCmd Char
  | LhsElementIdentifier String
  deriving(Show, Eq, Ord)

apicalMap = [
      ('F'  , CmdDrawForward         )
    , ('f'  , CmdMoveForward         )
    , ('l'  , CmdDrawForwardL        )
    , ('r'  , CmdDrawForwardR        )
    , ('L'  , CmdNodeReplaceLeft     )
    , ('R'  , CmdNodeReplaceRight    )
    , ('G'  , CmdMoveForwardNoVertex )
  ]

metaMap = [
      ('+'  , CmdTurnLeft            )
    , ('-'  , CmdTurnRight           )
    , ('^'  , CmdPitchUp             )
    , ('&'  , CmdPitchDown           )
    , ('\\' , CmdRollLeft            )
    , ('/'  , CmdRollRight           )
    , ('|'  , CmdTurnAround          )
    , ('$'  , CmdRotateVertical      )
    , ('['  , CmdStartBranch         )
    , (']'  , CmdCompleteBranch      )
    , ('{'  , CmdStartPolygon        )
    , ('}'  , CmdCompletePolygon     )
    , ('.'  , CmdRecordVertex        )
    , ('~'  , CmdMerge               )
    , ('\'' , CmdIncrementColor      )
    , ('%'  , CmdCutoffBranch        )
  ]
