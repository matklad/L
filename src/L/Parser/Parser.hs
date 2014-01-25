{-# OPTIONS_GHC -w #-}
module L.Parser.Parser (parse) where

import qualified L.Types.Token as T
import L.Types.AST
import L.Parser.ParserInternals

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (T.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Statement)
	| HappyAbsSyn6 (Expression)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (T.Token)
	-> HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (T.Token) (HappyStk HappyAbsSyn -> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(T.Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (8) = happyShift action_3
action_0 (11) = happyShift action_4
action_0 (13) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (17) = happyShift action_8
action_0 (4) = happyGoto action_9
action_0 (5) = happyGoto action_2
action_0 _ = happyFail

action_1 (8) = happyShift action_3
action_1 (11) = happyShift action_4
action_1 (13) = happyShift action_5
action_1 (15) = happyShift action_6
action_1 (16) = happyShift action_7
action_1 (17) = happyShift action_8
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (12) = happyShift action_15
action_2 _ = happyReduce_1

action_3 (18) = happyShift action_14
action_3 _ = happyFail

action_4 _ = happyReduce_2

action_5 (9) = happyShift action_13
action_5 _ = happyFail

action_6 (9) = happyShift action_12
action_6 _ = happyFail

action_7 (9) = happyShift action_11
action_7 _ = happyFail

action_8 (9) = happyShift action_10
action_8 _ = happyFail

action_9 (31) = happyAccept
action_9 _ = happyFail

action_10 (7) = happyShift action_18
action_10 (8) = happyShift action_19
action_10 (6) = happyGoto action_23
action_10 _ = happyFail

action_11 (8) = happyShift action_22
action_11 _ = happyFail

action_12 (7) = happyShift action_18
action_12 (8) = happyShift action_19
action_12 (6) = happyGoto action_21
action_12 _ = happyFail

action_13 (7) = happyShift action_18
action_13 (8) = happyShift action_19
action_13 (6) = happyGoto action_20
action_13 _ = happyFail

action_14 (7) = happyShift action_18
action_14 (8) = happyShift action_19
action_14 (6) = happyGoto action_17
action_14 _ = happyFail

action_15 (8) = happyShift action_3
action_15 (11) = happyShift action_4
action_15 (13) = happyShift action_5
action_15 (15) = happyShift action_6
action_15 (16) = happyShift action_7
action_15 (17) = happyShift action_8
action_15 (5) = happyGoto action_16
action_15 _ = happyFail

action_16 _ = happyReduce_3

action_17 (21) = happyShift action_25
action_17 (22) = happyShift action_26
action_17 (23) = happyShift action_27
action_17 (24) = happyShift action_28
action_17 (25) = happyShift action_29
action_17 (26) = happyShift action_30
action_17 (27) = happyShift action_31
action_17 (28) = happyShift action_32
action_17 (29) = happyShift action_33
action_17 (30) = happyShift action_34
action_17 _ = happyReduce_4

action_18 _ = happyReduce_10

action_19 _ = happyReduce_9

action_20 (10) = happyShift action_37
action_20 (21) = happyShift action_25
action_20 (22) = happyShift action_26
action_20 (23) = happyShift action_27
action_20 (24) = happyShift action_28
action_20 (25) = happyShift action_29
action_20 (26) = happyShift action_30
action_20 (27) = happyShift action_31
action_20 (28) = happyShift action_32
action_20 (29) = happyShift action_33
action_20 (30) = happyShift action_34
action_20 _ = happyFail

action_21 (10) = happyShift action_36
action_21 (21) = happyShift action_25
action_21 (22) = happyShift action_26
action_21 (23) = happyShift action_27
action_21 (24) = happyShift action_28
action_21 (25) = happyShift action_29
action_21 (26) = happyShift action_30
action_21 (27) = happyShift action_31
action_21 (28) = happyShift action_32
action_21 (29) = happyShift action_33
action_21 (30) = happyShift action_34
action_21 _ = happyFail

action_22 (10) = happyShift action_35
action_22 _ = happyFail

action_23 (10) = happyShift action_24
action_23 (21) = happyShift action_25
action_23 (22) = happyShift action_26
action_23 (23) = happyShift action_27
action_23 (24) = happyShift action_28
action_23 (25) = happyShift action_29
action_23 (26) = happyShift action_30
action_23 (27) = happyShift action_31
action_23 (28) = happyShift action_32
action_23 (29) = happyShift action_33
action_23 (30) = happyShift action_34
action_23 _ = happyFail

action_24 _ = happyReduce_6

action_25 (7) = happyShift action_18
action_25 (8) = happyShift action_19
action_25 (6) = happyGoto action_49
action_25 _ = happyFail

action_26 (7) = happyShift action_18
action_26 (8) = happyShift action_19
action_26 (6) = happyGoto action_48
action_26 _ = happyFail

action_27 (7) = happyShift action_18
action_27 (8) = happyShift action_19
action_27 (6) = happyGoto action_47
action_27 _ = happyFail

action_28 (7) = happyShift action_18
action_28 (8) = happyShift action_19
action_28 (6) = happyGoto action_46
action_28 _ = happyFail

action_29 (7) = happyShift action_18
action_29 (8) = happyShift action_19
action_29 (6) = happyGoto action_45
action_29 _ = happyFail

action_30 (7) = happyShift action_18
action_30 (8) = happyShift action_19
action_30 (6) = happyGoto action_44
action_30 _ = happyFail

action_31 (7) = happyShift action_18
action_31 (8) = happyShift action_19
action_31 (6) = happyGoto action_43
action_31 _ = happyFail

action_32 (7) = happyShift action_18
action_32 (8) = happyShift action_19
action_32 (6) = happyGoto action_42
action_32 _ = happyFail

action_33 (7) = happyShift action_18
action_33 (8) = happyShift action_19
action_33 (6) = happyGoto action_41
action_33 _ = happyFail

action_34 (7) = happyShift action_18
action_34 (8) = happyShift action_19
action_34 (6) = happyGoto action_40
action_34 _ = happyFail

action_35 _ = happyReduce_5

action_36 (19) = happyShift action_39
action_36 _ = happyFail

action_37 (19) = happyShift action_38
action_37 _ = happyFail

action_38 (8) = happyShift action_3
action_38 (11) = happyShift action_4
action_38 (13) = happyShift action_5
action_38 (15) = happyShift action_6
action_38 (16) = happyShift action_7
action_38 (17) = happyShift action_8
action_38 (5) = happyGoto action_51
action_38 _ = happyFail

action_39 (8) = happyShift action_3
action_39 (11) = happyShift action_4
action_39 (13) = happyShift action_5
action_39 (15) = happyShift action_6
action_39 (16) = happyShift action_7
action_39 (17) = happyShift action_8
action_39 (5) = happyGoto action_50
action_39 _ = happyFail

action_40 (21) = happyShift action_25
action_40 (22) = happyShift action_26
action_40 (23) = happyShift action_27
action_40 (24) = happyShift action_28
action_40 (25) = happyShift action_29
action_40 (26) = happyShift action_30
action_40 (27) = happyShift action_31
action_40 _ = happyReduce_19

action_41 (21) = happyShift action_25
action_41 (22) = happyShift action_26
action_41 (23) = happyShift action_27
action_41 (24) = happyShift action_28
action_41 (25) = happyShift action_29
action_41 (26) = happyShift action_30
action_41 (27) = happyShift action_31
action_41 _ = happyReduce_18

action_42 (21) = happyShift action_25
action_42 (22) = happyShift action_26
action_42 (23) = happyShift action_27
action_42 (24) = happyShift action_28
action_42 (25) = happyShift action_29
action_42 (26) = happyShift action_30
action_42 (27) = happyShift action_31
action_42 _ = happyReduce_20

action_43 (21) = happyShift action_25
action_43 (22) = happyShift action_26
action_43 (23) = happyShift action_27
action_43 (24) = happyShift action_28
action_43 _ = happyReduce_17

action_44 (21) = happyShift action_25
action_44 (22) = happyShift action_26
action_44 (23) = happyShift action_27
action_44 (24) = happyShift action_28
action_44 _ = happyReduce_16

action_45 (21) = happyShift action_25
action_45 (22) = happyShift action_26
action_45 (23) = happyShift action_27
action_45 (24) = happyShift action_28
action_45 _ = happyReduce_15

action_46 _ = happyReduce_14

action_47 _ = happyReduce_13

action_48 (23) = happyShift action_27
action_48 (24) = happyShift action_28
action_48 _ = happyReduce_12

action_49 (23) = happyShift action_27
action_49 (24) = happyShift action_28
action_49 _ = happyReduce_11

action_50 (12) = happyShift action_15
action_50 (20) = happyShift action_53
action_50 _ = happyFail

action_51 (12) = happyShift action_15
action_51 (20) = happyShift action_52
action_51 _ = happyFail

action_52 (14) = happyShift action_54
action_52 _ = happyFail

action_53 _ = happyReduce_8

action_54 (19) = happyShift action_55
action_54 _ = happyFail

action_55 (8) = happyShift action_3
action_55 (11) = happyShift action_4
action_55 (13) = happyShift action_5
action_55 (15) = happyShift action_6
action_55 (16) = happyShift action_7
action_55 (17) = happyShift action_8
action_55 (5) = happyGoto action_56
action_55 _ = happyFail

action_56 (12) = happyShift action_15
action_56 (20) = happyShift action_57
action_56 _ = happyFail

action_57 _ = happyReduce_7

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn5
		 (Skip
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (T.Name happy_var_1))
	 =  HappyAbsSyn5
		 (Assign (Var happy_var_1) happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyTerminal (T.Name happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Read (Var happy_var_3)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Write happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 11 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_3 happy_var_6 happy_var_10
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 7 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (While happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyTerminal (T.Name happy_var_1))
	 =  HappyAbsSyn6
		 (Variable (Var happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyTerminal (T.Number happy_var_1))
	 =  HappyAbsSyn6
		 (Constant (read happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Operation (makeOp happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Number happy_dollar_dollar -> cont 7;
	T.Name happy_dollar_dollar -> cont 8;
	T.LParen -> cont 9;
	T.RParen -> cont 10;
	T.Skip -> cont 11;
	T.Semicolon -> cont 12;
	T.If -> cont 13;
	T.Else -> cont 14;
	T.While -> cont 15;
	T.Read -> cont 16;
	T.Write -> cont 17;
	T.Assign -> cont 18;
	T.LBrace -> cont 19;
	T.RBrace -> cont 20;
	T.Operator T.Plus -> cont 21;
	T.Operator T.Minus -> cont 22;
	T.Operator T.Multiply -> cont 23;
	T.Operator T.Divide -> cont 24;
	T.Operator T.And -> cont 25;
	T.Operator T.Or -> cont 26;
	T.Operator T.Xor -> cont 27;
	T.Operator T.Eq -> cont 28;
	T.Operator T.Lt -> cont 29;
	T.Operator T.Gt -> cont 30;
	_ -> happyError' (tk:tks)
	}

happyError_ 31 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(T.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [T.Token] -> a
parseError t = error "You shall not parse"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4























{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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