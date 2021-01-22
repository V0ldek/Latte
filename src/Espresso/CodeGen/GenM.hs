-- Generator monad used in Espresso codegen.
module Espresso.CodeGen.GenM (
      askClass,
      askSym,
      emit,
      freshLabel,
      freshLabelIdx,
      freshVal,
      getCode,
      localSyms,
      runGen,
      toVVal,
      valIdentFor,
      GenM,
      EspPtr(..),
      EspVal(..),
) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                  as Map
import           Espresso.Syntax.Abs       hiding (Metadata (..))
import           Identifiers               (indexedValIdent, labIdent, valIdent)
import           SemanticAnalysis.Class
import           SemanticAnalysis.TopLevel
import qualified Syntax.Abs                as Latte

-- State of the generator.
data Store = St {
    -- Supply for indexed label names.
    stLabelCnt :: Integer,
    -- Supply of names for temporary value names.
    stValCnt   :: Integer,
    -- Supply for indexed names of variable-based values.
    stSymCnt   :: Map.Map Latte.Ident Integer,
    -- Code generated thus far, reversed.
    stCode     :: [Instr ()]
}
-- Immutable environment mapping Latte variable names to generated values.
data Env = Env {
    envSymbols  :: Map.Map Latte.Ident EspVal,
    envMetadata :: Metadata ()
}

-- Espresso value.
data EspVal = EspVal {valName :: ValIdent, valType_ :: SType ()}
data EspPtr = EspPtr {ptrVal :: Ptr (), ptrType_ :: SType ()}

type GenM = StateT Store (Reader Env)

-- Get the class metadata under a given identifier.
askClass :: SymIdent -> GenM (Class ())
askClass (SymIdent i) = do
    mbcl <- asks ((\(Meta m) -> Map.lookup (Latte.Ident i) m) . envMetadata)
    case mbcl of
        Just cl -> return cl
        Nothing -> error $ "internal error. class not found: " ++ i

-- Get the scoped Espresso value for a given Latte variable identifier.
askSym :: Latte.Ident -> GenM EspVal
askSym i = do
    mbval <- asks (Map.lookup i . envSymbols)
    case mbval of
        Just val -> return val
        Nothing  -> error $ "internal error. symbol not found: " ++ Latte.showI i

-- Emit an instruction.
emit :: Instr () -> GenM ()
emit instr = modify (\s -> s {stCode = instr : stCode s})

-- Get a fresh, previously not generated label.
freshLabel :: GenM LabIdent
freshLabel = labIdent . show <$> freshLabelIdx

-- Retrieve and increment the label counter.
freshLabelIdx :: GenM Integer
freshLabelIdx = do
    n <- gets stLabelCnt
    modify (\s -> s {stLabelCnt = n + 1})
    return n

-- Get a fresh, temporary value not related to any particular symbol.
freshVal :: GenM ValIdent
freshVal = valIdent . show <$> freshValIdx

-- Get all the code emitted thus far.
getCode :: GenM [Instr ()]
getCode = gets (reverse . stCode)

-- Execute the given continuation with the supplied symbol-to-value declarations in scope.
localSyms :: [(Latte.Ident, EspVal)] -> GenM a -> GenM a
localSyms syms = local (\e -> e {envSymbols = Map.union (Map.fromList syms) (envSymbols e)})

-- Run the generator starting from an empty initial state.
runGen :: Metadata a -> GenM b -> b
runGen meta gen = runIdentity $ runReaderT (evalStateT gen $ St 0 0 Map.empty []) (Env Map.empty (() <$ meta))

-- Get a fresh value with name signifying the identifier of the associated symbol.
valIdentFor :: Latte.Ident -> GenM ValIdent
valIdentFor i = do
    idx <- cntSym i
    return $ indexedValIdent (Latte.showI i) idx

-- Retrieve and increment the counter for the given symbol.
-- Returns 0 and initialises the counter if the symbol was not counted before.
cntSym :: Latte.Ident -> GenM Integer
cntSym i = do
    mbidx <- gets (Map.lookup i . stSymCnt)
    case mbidx of
        Just n -> do
            modify (\s -> s { stSymCnt = Map.insert i (n + 1) (stSymCnt s)})
            return $ n + 1
        Nothing -> do
            modify (\s -> s { stSymCnt = Map.insert i 0 (stSymCnt s)})
            return 0

-- Retrieve and increment the temporary value counter.
freshValIdx :: GenM Integer
freshValIdx = do
    n <- gets stValCnt
    modify (\s -> s {stValCnt = n + 1})
    return n

toVVal :: EspVal -> Val ()
toVVal (EspVal i t) = VVal () t i
