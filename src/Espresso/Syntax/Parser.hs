{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Espresso.Syntax.Parser where
import Espresso.Syntax.Abs
import Espresso.Syntax.Lexer
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((Maybe (Int, Int), Integer))
happyIn4 :: ((Maybe (Int, Int), Integer)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((Maybe (Int, Int), String))
happyIn5 :: ((Maybe (Int, Int), String)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((Maybe (Int, Int), SymIdent))
happyIn6 :: ((Maybe (Int, Int), SymIdent)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((Maybe (Int, Int), LabIdent))
happyIn7 :: ((Maybe (Int, Int), LabIdent)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((Maybe (Int, Int), ValIdent))
happyIn8 :: ((Maybe (Int, Int), ValIdent)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((Maybe (Int, Int), ArgIdent))
happyIn9 :: ((Maybe (Int, Int), ArgIdent)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((Maybe (Int, Int), QIdent (Maybe (Int, Int))))
happyIn10 :: ((Maybe (Int, Int), QIdent (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((Maybe (Int, Int), Program (Maybe (Int, Int))))
happyIn11 :: ((Maybe (Int, Int), Program (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ((Maybe (Int, Int), Metadata (Maybe (Int, Int))))
happyIn12 :: ((Maybe (Int, Int), Metadata (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ((Maybe (Int, Int), ClassDef (Maybe (Int, Int))))
happyIn13 :: ((Maybe (Int, Int), ClassDef (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ((Maybe (Int, Int), FieldDef (Maybe (Int, Int))))
happyIn14 :: ((Maybe (Int, Int), FieldDef (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((Maybe (Int, Int), MethodDef (Maybe (Int, Int))))
happyIn15 :: ((Maybe (Int, Int), MethodDef (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ((Maybe (Int, Int), [FieldDef (Maybe (Int, Int))]))
happyIn16 :: ((Maybe (Int, Int), [FieldDef (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ((Maybe (Int, Int), [MethodDef (Maybe (Int, Int))]))
happyIn17 :: ((Maybe (Int, Int), [MethodDef (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ((Maybe (Int, Int), [ClassDef (Maybe (Int, Int))]))
happyIn18 :: ((Maybe (Int, Int), [ClassDef (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ((Maybe (Int, Int), FType (Maybe (Int, Int))))
happyIn19 :: ((Maybe (Int, Int), FType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ((Maybe (Int, Int), SType (Maybe (Int, Int))))
happyIn20 :: ((Maybe (Int, Int), SType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ((Maybe (Int, Int), [SType (Maybe (Int, Int))]))
happyIn21 :: ((Maybe (Int, Int), [SType (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ((Maybe (Int, Int), Method (Maybe (Int, Int))))
happyIn22 :: ((Maybe (Int, Int), Method (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ((Maybe (Int, Int), [Method (Maybe (Int, Int))]))
happyIn23 :: ((Maybe (Int, Int), [Method (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ((Maybe (Int, Int), Instr (Maybe (Int, Int))))
happyIn24 :: ((Maybe (Int, Int), Instr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ((Maybe (Int, Int), PhiVariant (Maybe (Int, Int))))
happyIn25 :: ((Maybe (Int, Int), PhiVariant (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ((Maybe (Int, Int), Call (Maybe (Int, Int))))
happyIn26 :: ((Maybe (Int, Int), Call (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ((Maybe (Int, Int), [Instr (Maybe (Int, Int))]))
happyIn27 :: ((Maybe (Int, Int), [Instr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ((Maybe (Int, Int), [Val (Maybe (Int, Int))]))
happyIn28 :: ((Maybe (Int, Int), [Val (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ((Maybe (Int, Int), [PhiVariant (Maybe (Int, Int))]))
happyIn29 :: ((Maybe (Int, Int), [PhiVariant (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ((Maybe (Int, Int), Val (Maybe (Int, Int))))
happyIn30 :: ((Maybe (Int, Int), Val (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 ((Maybe (Int, Int), Op (Maybe (Int, Int))))
happyIn31 :: ((Maybe (Int, Int), Op (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x20\x0c\x30\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x08\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x10\x00\x00\x00\x00\x00\x00\x04\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x86\x43\x66\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x80\xb3\x80\xf8\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x02\x41\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x10\x90\x08\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x02\x00\x00\x08\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x10\x90\x08\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x06\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x10\x90\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x10\x90\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Integer","String","SymIdent","LabIdent","ValIdent","ArgIdent","QIdent","Program","Metadata","ClassDef","FieldDef","MethodDef","ListFieldDef","ListMethodDef","ListClassDef","FType","SType","ListSType","Method","ListMethod","Instr","PhiVariant","Call","ListInstr","ListVal","ListPhiVariant","Val","Op","'!='","'%'","'&'","'('","')'","'*'","'+'","','","'-'","'--'","'.'","'.classes'","'.fields'","'.metadata'","'.method'","'.methods'","'/'","':'","':='","';'","'<'","'<='","'=='","'>'","'>='","'['","'[]'","']'","'boolean'","'call'","'callvirt'","'chars'","'elemptr'","'else'","'false'","'fldptr'","'if'","'int'","'jump'","'lines'","'load'","'null'","'phi'","'return'","'store'","'string'","'then'","'true'","'void'","'|'","L_integ","L_quoted","L_SymIdent","L_LabIdent","L_ValIdent","L_ArgIdent","%eof"]
        bit_start = st * 88
        bit_end = (st + 1) * 88
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..87]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xff\xff\xdc\xff\x00\x00\xd9\xff\x00\x00\x0f\x00\x0a\x00\x08\x00\x00\x00\x09\x00\x2d\x00\x38\x00\x3a\x00\x3b\x00\x00\x00\x4d\x00\x34\x00\x54\x00\x00\x00\x00\x00\x00\x00\x04\x00\x0b\x00\x5d\x00\x00\x00\x73\x00\x81\x00\x82\x00\x00\x00\x83\x00\x00\x00\x5f\x00\x5f\x00\xdd\xff\xf5\xff\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x8d\x00\xb0\x00\xa9\x00\xaf\x00\x00\x00\x1e\x00\xaa\x00\x00\x00\x9f\x00\xa8\x00\x92\x00\xb3\x00\x05\x00\xb0\x00\xb0\x00\xb0\x00\xba\x00\xb0\x00\xb0\x00\x9e\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x8e\x00\xd0\x00\xce\x00\xd2\x00\xa7\x00\xc5\x00\xa6\x00\xc4\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\xcf\x00\xc8\x00\xdd\x00\xd7\x00\xb0\x00\xd8\x00\x00\x00\xdb\x00\xe6\x00\xea\x00\x00\x00\xb0\x00\x00\x00\xd1\x00\xbb\x00\x00\x00\xdc\x00\xbc\x00\xb0\x00\x00\x00\xd9\x00\x00\x00\xc1\x00\x00\x00\x95\x00\xd6\x00\xe3\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x00\x00\x00\x00\xc6\x00\x00\x00\xe7\x00\xfe\xff\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x00\xcb\x00\xe5\x00\x00\x00\x00\x00\x9a\x00\xed\x00\xcd\x00\x2b\x00\xe9\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x93\x00\xfb\x00\x00\x00\x9b\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xfd\xff\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\x00\x00\xf2\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\xf9\x00\x00\x00\xf1\x00\x5e\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x62\x00\x06\x01\x5b\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\x80\x00\x86\x00\x88\x00\x00\x00\x4b\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x64\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x01\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x0a\x01\x00\x00\x00\x00\xbf\x00\xab\x00\x00\x00\x00\x00\x00\x00\x0e\x01\x03\x01\x8f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x0f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x01\x00\x00\x07\x01\x00\x00\xda\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\xe0\xff\x00\x00\x00\x00\xf7\xff\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\xee\xff\xf8\xff\xcd\xff\x00\x00\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\xcc\xff\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xfa\xff\xc5\xff\xc4\xff\xc0\xff\xbf\xff\x00\x00\xc2\xff\xc1\xff\xc3\xff\xfd\xff\xf9\xff\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\xde\xff\xf6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\xcb\xff\x00\x00\xd7\xff\xdb\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xca\xff\x00\x00\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\xba\xff\xb3\xff\xbc\xff\xbe\xff\xbd\xff\xbb\xff\xb9\xff\xb8\xff\xb5\xff\xb7\xff\xb6\xff\xb2\xff\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\x00\x00\xc7\xff\x00\x00\xcf\xff\xcb\xff\xce\xff\x00\x00\x00\x00\xc9\xff\x00\x00\xc8\xff\x00\x00\xd3\xff\x00\x00\xda\xff\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\xd0\xff\xc6\xff\xd1\xff\x00\x00\xd6\xff\xd2\xff\x00\x00\xe6\xff\x00\x00\x00\x00\x00\x00\xe9\xff\xeb\xff\xea\xff\xe8\xff\x00\x00\xf4\xff\xe5\xff\xe7\xff\xf1\xff\x00\x00\x00\x00\x00\x00\xf0\xff\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xe4\xff\xf3\xff\xef\xff\xe3\xff\x00\x00\xec\xff\xe4\xff\xe2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x25\x00\x02\x00\x07\x00\x08\x00\x01\x00\x02\x00\x03\x00\x14\x00\x09\x00\x06\x00\x07\x00\x0e\x00\x09\x00\x33\x00\x00\x00\x01\x00\x39\x00\x36\x00\x04\x00\x05\x00\x11\x00\x0f\x00\x23\x00\x1b\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x2a\x00\x1c\x00\x12\x00\x1e\x00\x1f\x00\x1a\x00\x30\x00\x16\x00\x1c\x00\x33\x00\x34\x00\x1a\x00\x27\x00\x37\x00\x38\x00\x03\x00\x04\x00\x2c\x00\x2d\x00\x02\x00\x35\x00\x02\x00\x02\x00\x06\x00\x32\x00\x06\x00\x0c\x00\x36\x00\x37\x00\x1e\x00\x1f\x00\x35\x00\x21\x00\x35\x00\x23\x00\x24\x00\x10\x00\x11\x00\x0b\x00\x1b\x00\x29\x00\x2a\x00\x2b\x00\x12\x00\x00\x00\x01\x00\x12\x00\x30\x00\x04\x00\x05\x00\x33\x00\x34\x00\x00\x00\x01\x00\x37\x00\x38\x00\x04\x00\x05\x00\x00\x00\x01\x00\x00\x00\x01\x00\x04\x00\x05\x00\x04\x00\x05\x00\x03\x00\x04\x00\x18\x00\x02\x00\x1a\x00\x02\x00\x1a\x00\x06\x00\x35\x00\x06\x00\x18\x00\x02\x00\x1a\x00\x1a\x00\x12\x00\x06\x00\x18\x00\x14\x00\x1a\x00\x16\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x04\x00\x05\x00\x04\x00\x05\x00\x00\x00\x01\x00\x00\x00\x01\x00\x04\x00\x05\x00\x04\x00\x05\x00\x00\x00\x01\x00\x00\x00\x01\x00\x04\x00\x05\x00\x04\x00\x05\x00\x02\x00\x1c\x00\x1a\x00\x02\x00\x1a\x00\x12\x00\x35\x00\x13\x00\x03\x00\x14\x00\x1a\x00\x0a\x00\x1a\x00\x08\x00\x10\x00\x11\x00\x14\x00\x10\x00\x1a\x00\x14\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x01\x00\x04\x00\x05\x00\x04\x00\x05\x00\x00\x00\x01\x00\x04\x00\x1b\x00\x04\x00\x05\x00\x1c\x00\x1d\x00\x04\x00\x0a\x00\x0d\x00\x1c\x00\x1d\x00\x1d\x00\x1a\x00\x28\x00\x26\x00\x03\x00\x1a\x00\x04\x00\x1a\x00\x26\x00\x26\x00\x03\x00\x2e\x00\x36\x00\x1a\x00\x31\x00\x14\x00\x2e\x00\x2e\x00\x35\x00\x31\x00\x31\x00\x2f\x00\x15\x00\x35\x00\x35\x00\x14\x00\x19\x00\x23\x00\x15\x00\x05\x00\x08\x00\x05\x00\x19\x00\x14\x00\x2a\x00\x35\x00\x02\x00\x36\x00\x1a\x00\x33\x00\x30\x00\x12\x00\x1a\x00\x33\x00\x34\x00\x0b\x00\x09\x00\x37\x00\x38\x00\x0f\x00\x10\x00\x14\x00\x14\x00\x12\x00\x08\x00\x05\x00\x14\x00\x36\x00\x36\x00\x22\x00\x33\x00\x1c\x00\x20\x00\x14\x00\x14\x00\x33\x00\x12\x00\x14\x00\x10\x00\x09\x00\x33\x00\x1a\x00\x05\x00\x14\x00\x35\x00\x13\x00\x12\x00\x1c\x00\x02\x00\x0e\x00\x17\x00\x03\x00\x1b\x00\x03\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x8f\x00\x34\x00\x17\x00\x03\x00\x04\x00\x54\x00\x55\x00\x56\x00\x32\x00\x18\x00\x57\x00\x58\x00\x06\x00\x59\x00\x03\x00\x26\x00\x27\x00\xff\xff\x25\x00\x28\x00\x29\x00\x5a\x00\x0a\x00\x2c\x00\x90\x00\x5b\x00\x5c\x00\x5d\x00\x5e\x00\x5f\x00\x2d\x00\x1f\x00\x07\x00\x20\x00\x21\x00\x0b\x00\x2e\x00\x3d\x00\x1a\x00\x03\x00\x2f\x00\x3e\x00\x22\x00\x26\x00\x30\x00\x8f\x00\x9d\x00\x23\x00\x24\x00\x0c\x00\x0f\x00\x0c\x00\x84\x00\x0d\x00\x60\x00\x35\x00\x0c\x00\x25\x00\x26\x00\x20\x00\x21\x00\x0f\x00\x40\x00\x0f\x00\x2c\x00\x41\x00\x9f\x00\xa0\x00\x11\x00\x90\x00\x42\x00\x2d\x00\x43\x00\x12\x00\x26\x00\x27\x00\x10\x00\x2e\x00\x28\x00\x29\x00\x03\x00\x2f\x00\x26\x00\x27\x00\x26\x00\x30\x00\x28\x00\x29\x00\x26\x00\x27\x00\x26\x00\x27\x00\x28\x00\x29\x00\x28\x00\x29\x00\x1a\x00\x1b\x00\x4d\x00\x0c\x00\x4c\x00\x0c\x00\x15\x00\x34\x00\x0f\x00\x67\x00\x4b\x00\x0c\x00\x4c\x00\x13\x00\x3b\x00\x9d\x00\x71\x00\x1c\x00\x4c\x00\x1d\x00\x30\x00\x26\x00\x27\x00\x26\x00\x27\x00\x28\x00\x29\x00\x28\x00\x29\x00\x26\x00\x27\x00\x26\x00\x27\x00\x28\x00\x29\x00\x28\x00\x29\x00\x26\x00\x27\x00\x26\x00\x27\x00\x28\x00\x29\x00\x28\x00\x29\x00\x84\x00\x3a\x00\x2a\x00\x84\x00\x48\x00\x39\x00\x0f\x00\x38\x00\x8f\x00\x37\x00\x45\x00\x85\x00\x51\x00\xa3\x00\x9f\x00\xa3\x00\x48\x00\x86\x00\x50\x00\x47\x00\x4f\x00\x26\x00\x27\x00\x26\x00\x27\x00\x28\x00\x29\x00\x28\x00\x29\x00\x26\x00\x27\x00\x45\x00\x90\x00\x28\x00\x29\x00\x88\x00\x89\x00\x44\x00\x3d\x00\x63\x00\x9b\x00\x89\x00\x89\x00\x3c\x00\x62\x00\x8a\x00\x69\x00\x65\x00\x4f\x00\x76\x00\x8a\x00\x8a\x00\x69\x00\x8b\x00\x25\x00\x7d\x00\x8c\x00\x61\x00\x8b\x00\x8b\x00\x0f\x00\x8c\x00\x8c\x00\x4b\x00\x6a\x00\x0f\x00\x0f\x00\x4a\x00\x6b\x00\x2c\x00\x6a\x00\x6f\x00\x6e\x00\x6d\x00\x7e\x00\x69\x00\x2d\x00\x0f\x00\x84\x00\x25\x00\x67\x00\x03\x00\x2e\x00\x64\x00\x7a\x00\x03\x00\x2f\x00\x97\x00\x79\x00\x26\x00\x30\x00\x98\x00\x99\x00\x78\x00\x76\x00\x75\x00\x74\x00\x73\x00\x80\x00\x25\x00\x25\x00\x71\x00\x03\x00\x7d\x00\x84\x00\x83\x00\x82\x00\x03\x00\x94\x00\x91\x00\x8d\x00\x93\x00\x03\x00\x95\x00\xa2\x00\x9f\x00\x0f\x00\x07\x00\x08\x00\x9c\x00\x13\x00\x16\x00\x15\x00\x32\x00\x52\x00\x6f\x00\x64\x00\x80\x00\x7b\x00\x7a\x00\x91\x00\x8d\x00\x95\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 77) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77)
	]

happy_n_terms = 58 :: Int
happy_n_nonterms = 28 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((Just (tokenLineCol happy_var_1), SymIdent (prToken happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((Just (tokenLineCol happy_var_1), LabIdent (prToken happy_var_1))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((Just (tokenLineCol happy_var_1), ValIdent (prToken happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((Just (tokenLineCol happy_var_1), ArgIdent (prToken happy_var_1))
	)}

happyReduce_7 = happySpecReduce_3  6# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut6 happy_x_3 of { (HappyWrap6 happy_var_3) -> 
	happyIn10
		 ((fst happy_var_1, Espresso.Syntax.Abs.QIdent (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_8 = happySpecReduce_2  7# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	happyIn11
		 ((fst happy_var_1, Espresso.Syntax.Abs.Program (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_2)))
	)}}

happyReduce_9 = happyReduce 9# 8# happyReduction_9
happyReduction_9 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_7 of { (HappyWrap18 happy_var_7) -> 
	happyIn12
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Meta (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_7)))
	) `HappyStk` happyRest}}

happyReduce_10 = happyReduce 14# 9# happyReduction_10
happyReduction_10 (happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut16 happy_x_7 of { (HappyWrap16 happy_var_7) -> 
	case happyOut17 happy_x_12 of { (HappyWrap17 happy_var_12) -> 
	happyIn13
		 ((fst happy_var_1, Espresso.Syntax.Abs.ClDef (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_7)) (reverse (snd happy_var_12)))
	) `HappyStk` happyRest}}}

happyReduce_11 = happySpecReduce_2  10# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	happyIn14
		 ((fst happy_var_1, Espresso.Syntax.Abs.FldDef (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_12 = happySpecReduce_2  11# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn15
		 ((fst happy_var_1, Espresso.Syntax.Abs.MthdDef (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_13 = happySpecReduce_0  12# happyReduction_13
happyReduction_13  =  happyIn16
		 ((Nothing, [])
	)

happyReduce_14 = happySpecReduce_3  12# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	happyIn16
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_15 = happySpecReduce_0  13# happyReduction_15
happyReduction_15  =  happyIn17
		 ((Nothing, [])
	)

happyReduce_16 = happySpecReduce_3  13# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn17
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_17 = happySpecReduce_0  14# happyReduction_17
happyReduction_17  =  happyIn18
		 ((Nothing, [])
	)

happyReduce_18 = happySpecReduce_2  14# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	happyIn18
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_19 = happyReduce 4# 15# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn19
		 ((fst happy_var_1, Espresso.Syntax.Abs.FType (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_20 = happySpecReduce_1  16# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Int (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_21 = happySpecReduce_1  16# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Str (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_22 = happySpecReduce_1  16# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Bool (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_23 = happySpecReduce_1  16# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Void (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_24 = happySpecReduce_2  16# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, Espresso.Syntax.Abs.Arr (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_25 = happySpecReduce_1  16# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, Espresso.Syntax.Abs.Cl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_26 = happySpecReduce_2  16# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, Espresso.Syntax.Abs.Ref (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_27 = happySpecReduce_0  17# happyReduction_27
happyReduction_27  =  happyIn21
		 ((Nothing, [])
	)

happyReduce_28 = happySpecReduce_1  17# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn21
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_29 = happySpecReduce_3  17# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn21
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_30 = happyReduce 6# 18# happyReduction_30
happyReduction_30 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut27 happy_x_5 of { (HappyWrap27 happy_var_5) -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Mthd (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(reverse (snd happy_var_5)))
	) `HappyStk` happyRest}}}

happyReduce_31 = happySpecReduce_0  19# happyReduction_31
happyReduction_31  =  happyIn23
		 ((Nothing, [])
	)

happyReduce_32 = happySpecReduce_2  19# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	happyIn23
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_33 = happySpecReduce_2  20# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.ILabel (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_34 = happyReduce 11# 20# happyReduction_34
happyReduction_34 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut4 happy_x_5 of { (HappyWrap4 happy_var_5) -> 
	case happyOut4 happy_x_7 of { (HappyWrap4 happy_var_7) -> 
	case happyOut4 happy_x_9 of { (HappyWrap4 happy_var_9) -> 
	case happyOut4 happy_x_11 of { (HappyWrap4 happy_var_11) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.ILabelAnn (fst happy_var_1)(snd happy_var_1)(snd happy_var_5)(snd happy_var_7)(snd happy_var_9)(snd happy_var_11))
	) `HappyStk` happyRest}}}}}

happyReduce_35 = happySpecReduce_2  20# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.IVRet (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_36 = happySpecReduce_3  20# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { (HappyWrap30 happy_var_2) -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.IRet (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_37 = happyReduce 6# 20# happyReduction_37
happyReduction_37 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	case happyOut31 happy_x_4 of { (HappyWrap31 happy_var_4) -> 
	case happyOut30 happy_x_5 of { (HappyWrap30 happy_var_5) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.IOp (fst happy_var_1)(snd happy_var_1)(snd happy_var_3)(snd happy_var_4)(snd happy_var_5))
	) `HappyStk` happyRest}}}}

happyReduce_38 = happySpecReduce_2  20# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.IVCall (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_39 = happyReduce 4# 20# happyReduction_39
happyReduction_39 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.ICall (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_40 = happySpecReduce_3  20# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.IJmp (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_41 = happyReduce 8# 20# happyReduction_41
happyReduction_41 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	case happyOut7 happy_x_5 of { (HappyWrap7 happy_var_5) -> 
	case happyOut7 happy_x_7 of { (HappyWrap7 happy_var_7) -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.ICondJmp (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5)(snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_42 = happyReduce 5# 20# happyReduction_42
happyReduction_42 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.ILoad (fst happy_var_1)(snd happy_var_1)(snd happy_var_4))
	) `HappyStk` happyRest}}

happyReduce_43 = happyReduce 4# 20# happyReduction_43
happyReduction_43 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { (HappyWrap30 happy_var_2) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.IStore (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_44 = happyReduce 6# 20# happyReduction_44
happyReduction_44 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	case happyOut10 happy_x_5 of { (HappyWrap10 happy_var_5) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.IFld (fst happy_var_1)(snd happy_var_1)(snd happy_var_4)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_45 = happyReduce 8# 20# happyReduction_45
happyReduction_45 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	case happyOut30 happy_x_6 of { (HappyWrap30 happy_var_6) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.IArr (fst happy_var_1)(snd happy_var_1)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest}}}

happyReduce_46 = happyReduce 7# 20# happyReduction_46
happyReduction_46 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut29 happy_x_5 of { (HappyWrap29 happy_var_5) -> 
	happyIn24
		 ((fst happy_var_1, Espresso.Syntax.Abs.IPhi (fst happy_var_1)(snd happy_var_1)(snd happy_var_5))
	) `HappyStk` happyRest}}

happyReduce_47 = happySpecReduce_3  21# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn25
		 ((fst happy_var_1, Espresso.Syntax.Abs.PhiVar (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_48 = happyReduce 5# 22# happyReduction_48
happyReduction_48 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn26
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.Call (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_49 = happyReduce 5# 22# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn26
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.CallVirt (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_50 = happySpecReduce_0  23# happyReduction_50
happyReduction_50  =  happyIn27
		 ((Nothing, [])
	)

happyReduce_51 = happySpecReduce_2  23# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn27
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_52 = happySpecReduce_0  24# happyReduction_52
happyReduction_52  =  happyIn28
		 ((Nothing, [])
	)

happyReduce_53 = happySpecReduce_1  24# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn28
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_54 = happySpecReduce_3  24# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn28
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_55 = happySpecReduce_0  25# happyReduction_55
happyReduction_55  =  happyIn29
		 ((Nothing, [])
	)

happyReduce_56 = happySpecReduce_1  25# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn29
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_57 = happySpecReduce_3  25# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn29
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_58 = happySpecReduce_1  26# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn30
		 ((fst happy_var_1, Espresso.Syntax.Abs.VInt (fst happy_var_1)(snd happy_var_1))
	)}

-- This reduction is manually edited.
-- Without the `init $ tail $` calls a string literal "abc" is parsed with the enclosing quotes included.
-- That causes nasty problems with pretty printing later on.
happyReduce_59 = happySpecReduce_1  26# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn30
		 ((fst happy_var_1, Espresso.Syntax.Abs.VStr (fst happy_var_1)(init $ tail $ snd happy_var_1))
	)}

happyReduce_60 = happySpecReduce_1  26# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.VTrue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_61 = happySpecReduce_1  26# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.VFalse (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_62 = happySpecReduce_1  26# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.VNull (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_63 = happySpecReduce_1  26# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn30
		 ((fst happy_var_1, Espresso.Syntax.Abs.VVal (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_1  26# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn30
		 ((fst happy_var_1, Espresso.Syntax.Abs.VArg (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_65 = happySpecReduce_1  27# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpAdd (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_66 = happySpecReduce_1  27# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpSub (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_67 = happySpecReduce_1  27# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpMul (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_68 = happySpecReduce_1  27# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpDiv (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_69 = happySpecReduce_1  27# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpMod (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_70 = happySpecReduce_1  27# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpLTH (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_71 = happySpecReduce_1  27# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpLE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_72 = happySpecReduce_1  27# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpGTH (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_73 = happySpecReduce_1  27# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpGE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_74 = happySpecReduce_1  27# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpEQU (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_75 = happySpecReduce_1  27# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpNE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_76 = happySpecReduce_1  27# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpAnd (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_77 = happySpecReduce_1  27# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Espresso.Syntax.Abs.OpOr (Just (tokenLineCol happy_var_1)))
	)}

happyNewToken action sts stk [] =
	happyDoAction 57# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TI _) -> cont 51#;
	PT _ (TL _) -> cont 52#;
	PT _ (T_SymIdent _) -> cont 53#;
	PT _ (T_LabIdent _) -> cont 54#;
	PT _ (T_ValIdent _) -> cont 55#;
	PT _ (T_ArgIdent _) -> cont 56#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 57# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap11 x') = happyOut11 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8371_0/ghc_2.h" #-}
































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
