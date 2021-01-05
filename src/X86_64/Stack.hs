module X86_64.Stack where

import           Data.Int
import           Data.List
import qualified Data.Map            as Map
import           Espresso.Syntax.Abs
import           Identifiers
import           X86_64.Loc
import           X86_64.Size

data Stack = Stack {
    stackReservedSize  :: Int64,
    stackReservedSlots :: Map.Map ValIdent Slot,
    stackOverheadSize  :: Int64,
    stackOccupiedSlots :: Map.Map ValIdent Slot}

data Slot = Slot {slotOffset :: Int64, slotSize :: Size} deriving Show

stackEmpty :: Stack
stackEmpty = Stack 0 Map.empty 0 Map.empty

stackReserve :: [(ValIdent, Size)] -> Stack -> Stack
stackReserve vals sInit = foldl' reserveOne sInit vals
    where reserveOne s (vi, size) =
            let sizeBefore = stackReservedSize s
                bytes = sizeInBytes size
                slot = Slot (-sizeBefore - bytes) size
            in  s {stackReservedSize = sizeBefore + bytes,
                   stackReservedSlots = Map.insert vi slot (stackReservedSlots s),
                   stackOccupiedSlots = Map.insert vi slot (stackOccupiedSlots s)}

stackReservedLocs :: Stack -> [(ValIdent, Loc)]
stackReservedLocs s = Map.toList $ Map.map slotToLoc (stackReservedSlots s)

stackClearOverhead :: Stack -> (Int64, Stack)
stackClearOverhead s =
    let overhead = stackOverheadSize s
        reservedValues = Map.keysSet (stackReservedSlots s)
        s' = s {stackOverheadSize = 0,
                stackOccupiedSlots = Map.restrictKeys (stackOccupiedSlots s) reservedValues}
    in  (overhead, s')

stackPush :: ValIdent -> Size -> Stack -> (Loc, Stack)
stackPush vi size s =
    let sizeBefore = stackOverheadSize s
        bytes = sizeInBytes size
        slot = Slot (-sizeBefore - stackReservedSize s - bytes) size
        s' = s {stackOverheadSize = sizeBefore + bytes,
                stackOccupiedSlots = Map.insert vi slot (stackOccupiedSlots s)}
    in (slotToLoc slot, s')

stackPushUnnamed :: Size -> Stack -> Stack
stackPushUnnamed size s = s {stackOverheadSize = stackOverheadSize s + sizeInBytes size}

stackDelete :: ValIdent -> Stack -> Stack
stackDelete vi s = s {stackOccupiedSlots = Map.delete vi (stackOccupiedSlots s)}

stackIsValueReserved :: ValIdent -> Stack -> Bool
stackIsValueReserved vi = Map.member vi . stackReservedSlots

stackInsertReserved :: ValIdent -> Stack -> (Loc, Stack)
stackInsertReserved vi s = case Map.lookup vi (stackReservedSlots s) of
    Just slot -> (slotToLoc slot, s {stackOccupiedSlots = Map.insert vi slot (stackOccupiedSlots s)})
    Nothing -> error $ "stackInsertReserved: value with not reserved slot " ++ toStr vi

stackContains :: ValIdent -> Stack -> Bool
stackContains vi s = Map.member vi (stackOccupiedSlots s)

stackAlign16 :: Stack -> (Int64, Stack)
stackAlign16 s = let misalignment = 16 - (stackReservedSize s + stackOverheadSize s) `mod` 16
                     x = if misalignment == 16 then 0 else misalignment
                 in (x, s {stackOverheadSize = stackOverheadSize s + x})

slotToLoc :: Slot -> Loc
slotToLoc slot = LocStack (slotOffset slot)
