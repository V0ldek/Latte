module SemanticAnalysis.Analyser where

import           Syntax.Abs

data SemData = SemData Integer

analyse :: Program Pos -> Program SemData
analyse = undefined
