-- Stack state during assembly generation.
module X86_64.CodeGen.Stack where

import           Data.Int
import           Data.List
import qualified Data.Map            as Map
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Loc
import           X86_64.Size

-- The stack contains a number of reserved slots for locals which
-- are preserved between basic blocks, and an overhead of spilled
-- variables and empty areas for alignment purposes.
data Stack = Stack {
    stackReservedSize  :: Int64,
    stackReservedSlots :: Map.Map ValIdent Slot,
    stackOverheadSize  :: Int64,
    stackOccupiedSlots :: Map.Map ValIdent [Slot]}

data Slot = Slot {
    -- Offset on the stack. The stack grows downwards,
    -- so the offset is usually negative.
    slotOffset :: Int64,
    -- Size of the value in this slot.
    slotSize   :: Size
} deriving Show

stackEmpty :: Stack
stackEmpty = Stack 0 Map.empty 0 Map.empty

-- Allocate slots for the given variables to be preserved
-- between basic blocks.
stackReserve :: [(ValIdent, Size)] -> Stack -> Stack
stackReserve vals sInit = foldl' reserveOne sInit vals
    where reserveOne s (vi, size) =
            let sizeBefore = stackReservedSize s
                bytes = sizeInBytes size
                slot = Slot (-sizeBefore - bytes) size
            in  s {stackReservedSize = sizeBefore + bytes,
                   stackReservedSlots = Map.insert vi slot (stackReservedSlots s),
                   stackOccupiedSlots = Map.insertWith (++) vi [slot] (stackOccupiedSlots s)}

-- Get all variables that have reserved locations along with these locations.
stackReservedLocs :: Stack -> [(ValIdent, Loc)]
stackReservedLocs s = Map.toList $ Map.map slotToLoc (stackReservedSlots s)

-- Remove all values that do not have reserved locations.
stackClearOverhead :: Stack -> (Int64, Stack)
stackClearOverhead s =
    let overhead = stackOverheadSize s
        reservedValues = Map.keysSet (stackReservedSlots s)
        s' = s {stackOverheadSize = 0,
                stackOccupiedSlots = Map.restrictKeys (stackOccupiedSlots s) reservedValues}
    in  (overhead, s')

-- Push the variable onto the stack, ignoring whether it has or does not have
-- a reserved slot.
stackPush :: ValIdent -> Size -> Stack -> (Loc, Stack)
stackPush vi size s =
    let sizeBefore = stackOverheadSize s
        bytes = sizeInBytes size
        slot = Slot (-sizeBefore - stackReservedSize s - bytes) size
        s' = s {stackOverheadSize = sizeBefore + bytes,
                stackOccupiedSlots = Map.insertWith (++) vi [slot] (stackOccupiedSlots s)}
    in (slotToLoc slot, s')

-- Increase the stack overhead with some unidentified value.
stackPushUnnamed :: Size -> Stack -> Stack
stackPushUnnamed size s = s {stackOverheadSize = stackOverheadSize s + sizeInBytes size}

-- Remove the given variable from the stack.
stackDelete :: ValIdent -> Stack -> Stack
stackDelete vi s = s {stackOccupiedSlots = Map.delete vi (stackOccupiedSlots s)}

-- Whether the given variable has a reserved slot.
stackIsValueReserved :: ValIdent -> Stack -> Bool
stackIsValueReserved vi = Map.member vi . stackReservedSlots

-- Insert a given variable into its reserved slot. Fails if the variable
-- has no reserved slot.
stackInsertReserved :: ValIdent -> Stack -> (Loc, Stack)
stackInsertReserved vi s = case Map.lookup vi (stackReservedSlots s) of
    Just slot -> (slotToLoc slot, s {stackOccupiedSlots = Map.insertWith (++) vi [slot] (stackOccupiedSlots s)})
    Nothing -> error $ "stackInsertReserved: value with not reserved slot " ++ toStr vi

-- Insert a given variable into the given slot.
stackInsert :: ValIdent -> Slot -> Stack -> Stack
stackInsert vi slot s = if (-slotOffset slot) > stackSize s
                          then error $ "stackInsert: stack size " ++ show (stackSize s) ++ " exceeded when inserting " ++ toStr vi ++ " into " ++ show slot
                          else s {stackOccupiedSlots = Map.insertWith (++) vi [slot] (stackOccupiedSlots s)}

-- Whether the given variable is saved on the stack.
stackContains :: ValIdent -> Stack -> Bool
stackContains vi s = Map.member vi (stackOccupiedSlots s)

-- Align the stack to take a multiple of 16 bytes. Returns the applied additional offset
-- and the aligned stack.
stackAlign16 :: Stack -> (Int64, Stack)
stackAlign16 s = let misalignment = 16 - stackSize s `mod` 16
                     x = if misalignment == 16 then 0 else misalignment
                 in (x, s {stackOverheadSize = stackOverheadSize s + x})

slotToLoc :: Slot -> Loc
slotToLoc slot = LocStack (slotOffset slot)

-- Size of the entire stack counting from 0 downwards.
stackSize :: Stack -> Int64
stackSize s = stackReservedSize s + stackOverheadSize s
