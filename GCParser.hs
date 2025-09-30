{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module GCParser where
import SyntacticGlobalChoreographies
import ErlanGC
import Data.Set as S (empty, insert, union, fromList, toList, member, singleton, Set)
import qualified Data.Set as S (map)
import Data.List as L
import qualified Data.Map as M
import Misc
import CFSM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (((GC, Set Ptp), QDec))
	| HappyAbsSyn5 (GCEnv -> (GC, Set Ptp))
	| HappyAbsSyn6 (GCEnv)
	| HappyAbsSyn7 (GCEnv -> GCEnv)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn9 (GCEnv -> GCCtx)
	| HappyAbsSyn10 ([GCEnv -> GCCtx])
	| HappyAbsSyn11 (QDec)
	| HappyAbsSyn13 (String)
	| HappyAbsSyn14 (Ptp)
	| HappyAbsSyn15 (GCEnv -> [((GC, Set Ptp), M.Map String String  {- TODO: implement guards -} )])
	| HappyAbsSyn16 (GCEnv -> ((GC, Set Ptp), M.Map String String))
	| HappyAbsSyn18 (GCEnv -> (GCConst, (GC, Set Ptp), [String]))
	| HappyAbsSyn22 (M.Map String String)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,227) ([0,0,0,4,0,0,512,0,128,0,0,32768,33856,574,0,64,0,0,0,0,0,0,0,8200,0,0,16,0,0,1088,0,0,0,16,0,0,0,0,0,768,0,0,0,0,64,0,0,8192,0,0,0,0,0,8,0,0,1024,0,0,0,0,0,0,256,0,0,32768,1024,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,8192,0,0,16,0,0,0,256,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,8,0,0,2,0,0,256,0,0,32768,33856,574,0,0,16384,0,8192,41232,143,0,128,0,0,2048,0,0,0,4,0,0,0,0,0,0,512,0,0,0,0,0,16384,0,0,0,4128,36833,0,0,16,0,0,0,0,0,0,4,0,0,64,0,0,0,32768,0,0,512,0,0,0,1,0,8192,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,2,0,0,0,0,0,32768,0,16384,0,0,0,0,0,0,4096,0,0,0,0,2,0,0,64,0,0,0,0,0,0,0,0,32768,0,0,0,64,0,0,0,0,0,0,16,0,0,2048,59460,35,0,1088,0,0,0,16,0,0,6,0,0,16512,16260,2,16384,49696,287,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,4098,0,0,256,0,0,0,0,0,0,512,0,0,8192,57616,143,0,0,0,0,0,64,0,0,8708,4604,0,0,0,0,0,4368,0,0,34816,1,0,16384,0,0,0,32,0,0,4096,61576,71,0,17416,9208,0,1024,64546,17,0,8192,0,0,8192,0,0,0,32,0,0,0,2,0,0,256,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,32,0,0,4096,53384,71,0,0,0,0,16384,4,0,0,32,0,0,12288,1,0,0,2048,0,0,0,1,0,0,128,0,0,16,0,0,0,0,0,0,1088,512,0,512,65041,8,0,272,0,0,34816,16384,0,0,1024,0,0,0,0,0,4096,0,0,0,8,0,0,0,0,0,0,4354,2302,0,0,0,0,0,0,64,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,16,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_gcgrammar","G","GE","E","A","R","Ctx","Brxs","Q","B","qos","choiceop","Bs","Br","S","call","fparams","aparams","strs","guard","constr","pptps","qstr","qatt","str","'->'","'=>'","'='","'|'","'+'","'%'","'*'","';'","'@'","':'","'{'","'}'","'['","']'","'&'","'(o)'","'[]'","'sel'","'branch'","'break'","'accept'","'repeat'","'unless'","'let'","'in'","'do'","'with'","%eof"]
        bit_start = st Prelude.* 55
        bit_end = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyActOffsets = Happy_Data_Array.listArray (0,143) ([-22,-22,54,21,27,31,11,87,60,103,0,43,92,92,0,98,115,0,117,1,118,0,0,0,-14,109,95,0,121,0,0,111,113,124,125,21,105,21,123,127,128,0,122,0,130,-2,126,0,126,131,2,129,132,133,0,0,120,116,0,116,135,0,135,134,137,0,0,138,141,0,143,21,91,136,88,-2,-2,0,0,0,144,23,145,0,147,-2,0,140,-2,0,-4,57,146,152,-2,-2,-2,142,148,149,150,150,0,139,0,0,0,158,0,157,21,0,91,154,89,151,155,159,160,0,-1,-2,91,-1,156,0,162,164,0,-2,0,153,161,0,0,0,163,163,0,165,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyGotoOffsets = Happy_Data_Array.listArray (0,143) ([32,102,166,28,167,0,0,170,0,0,0,0,104,106,0,0,0,0,168,0,169,0,0,0,171,0,108,0,172,0,0,0,0,0,173,53,110,62,0,174,0,0,177,0,178,4,0,0,0,0,176,0,0,0,0,0,0,112,0,114,179,0,180,0,0,0,0,0,0,0,0,41,0,0,0,74,75,0,0,0,181,0,184,0,0,77,0,0,78,0,0,0,0,182,68,81,84,0,0,0,175,190,0,187,0,0,0,0,0,0,47,0,0,0,0,0,0,0,0,0,186,85,0,188,0,0,0,0,0,71,0,189,0,0,0,0,191,196,0,0,0,0,0,0
	])

happyAdjustOffset :: Prelude.Int -> Prelude.Int
happyAdjustOffset = Prelude.id

happyDefActions :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyDefActions = Happy_Data_Array.listArray (0,143) ([-6,0,-29,0,0,0,0,-10,-3,0,-30,0,-6,-6,-41,-36,-37,-42,-43,0,0,-2,-28,-47,-58,0,-6,-44,-65,-35,-34,0,0,0,0,0,-6,0,0,0,0,-5,-10,-9,-55,0,-51,-4,0,0,-62,0,0,0,-52,-66,0,-6,-53,-6,0,-57,-59,0,0,-50,-48,0,0,-40,0,0,-7,0,0,0,0,-12,-11,-13,-14,0,0,-56,0,0,-16,0,0,-15,0,0,0,0,0,0,0,0,-38,0,-32,-32,-49,-58,-60,-54,-45,0,-46,0,0,-31,-20,-21,-26,0,0,0,0,-25,-62,0,-8,-62,0,-22,0,0,-19,0,-39,-62,0,-33,-61,-27,-32,-32,-24,0,-23,-17,-18
	])

happyCheck :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyCheck = Happy_Data_Array.listArray (0,227) ([-1,3,16,7,3,27,7,11,10,5,11,15,14,11,10,14,30,19,20,21,22,23,24,25,3,26,3,29,26,18,3,10,0,1,2,14,8,14,10,28,19,13,21,22,23,24,25,4,5,8,29,10,11,12,13,8,2,10,11,12,13,8,31,10,7,12,13,7,11,12,8,11,10,5,6,13,5,6,10,5,5,10,5,5,10,10,5,10,10,5,5,10,4,5,10,10,7,8,7,12,11,3,11,1,2,1,2,1,2,1,2,1,2,1,2,1,2,14,3,27,3,3,27,14,3,12,15,3,3,6,3,3,27,3,12,15,3,11,3,8,-1,3,13,27,3,13,3,3,3,3,14,17,15,6,14,3,8,15,9,1,3,7,-1,3,14,3,15,3,13,30,3,15,13,7,4,-1,15,14,-1,26,15,4,-1,14,9,-1,-1,16,20,15,-1,-1,20,20,18,17,17,17,14,9,9,20,20,16,18,9,18,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyTable = Happy_Data_Array.listArray (0,227) ([0,75,60,96,26,5,96,97,76,72,97,120,77,-30,73,27,61,78,79,16,17,80,81,82,12,71,88,83,71,41,8,13,5,2,3,14,8,89,9,42,15,10,16,17,18,19,20,34,35,48,21,9,97,98,50,48,23,9,130,98,50,48,-1,9,96,49,50,37,97,119,46,38,9,114,115,10,114,135,73,91,90,73,122,120,73,73,113,73,73,112,123,73,93,94,73,73,96,130,96,40,97,31,97,2,3,32,3,31,3,56,3,47,3,64,3,63,3,36,30,5,29,25,5,58,29,54,55,53,29,46,45,43,5,63,40,66,67,38,63,72,0,102,69,5,101,68,100,29,25,118,95,104,103,86,122,29,111,112,110,133,132,96,0,126,108,138,129,137,128,61,6,139,127,21,38,0,134,108,0,71,141,84,0,23,108,0,0,58,27,43,0,0,55,51,69,83,61,104,86,106,142,89,116,105,124,141,139,134,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 65) [
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
	(65 , happyReduce_65)
	]

happy_n_terms = 32 :: Prelude.Int
happy_n_nonterms = 21 :: Prelude.Int

happyReduce_1 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_1 = happySpecReduce_2  0 happyReduction_1
happyReduction_1 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1 emptyEnv, happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_2 = happySpecReduce_2  1 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (\env ->
         let
           join = compEnv env happy_var_1
           (g, ptps) = happy_var_2 join
         in
            (g, ptps)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_3 = happyReduce 4 1 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\env ->
         let
           join = compEnv env happy_var_1
           (g, ptps) = happy_var_2 join
           (g', ptps') = happy_var_4 join
         in
           (Par (checkToken TokenPar (Par [g, g'])),
             S.union ptps ptps'
           )
	) `HappyStk` happyRest

happyReduce_4 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_4 = happySpecReduce_3  2 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2 emptyEnv
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_5 = happySpecReduce_0  2 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 (emptyEnv
	)

happyReduce_6 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_6 = happyReduce 4 3 happyReduction_6
happyReduction_6 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\env -> (insEnv happy_var_1 happy_var_2 happy_var_4 env)
	) `HappyStk` happyRest

happyReduce_7 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_7 = happyReduce 6 3 happyReduction_7
happyReduction_7 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\env -> insEnv happy_var_3 happy_var_4 happy_var_6 (compEnv env (happy_var_1 emptyEnv))
	) `HappyStk` happyRest

happyReduce_8 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_9 = happySpecReduce_0  4 happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (\_ -> Hole
	)

happyReduce_11 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (\_ -> Empx
	)

happyReduce_12 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (\_ -> Brkx
	)

happyReduce_13 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (\_ -> Acpx []
	)

happyReduce_14 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (\_ -> Acpx happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_15 = happySpecReduce_2  5 happyReduction_15
happyReduction_15 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (\env ->
          let
            (const, (g, _), params) = happy_var_2 env
            (ctx, formals) = env M.! const 
            m = M.fromList (L.zip formals params)
            g' = applyEnvGC env g
          in
            case (checkParameters formals params, M.member const env) of
              (Just err, _) -> myErr err
              (_, True) -> substx m $ fillx env g' ctx
              _ -> myErr ("Use of undefined constant: " ++ const)
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_16 = happyReduce 6 5 happyReduction_16
happyReduction_16 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\_ ->
          case ((isPtp happy_var_1), (isPtp happy_var_3), (happy_var_1 == happy_var_3)) of
            (True, True, False) ->
              Actx (happy_var_1 , happy_var_3) (happy_var_5 ++ happy_var_6)
            (False, False, _) ->
              myErr ("Malformed participant names " ++ happy_var_1 ++ " and " ++ happy_var_3)
            (False, True, True) ->
              myErr ("Malformed participant name " ++ happy_var_1 ++ " and sender and receiver must be different")
            (False, True, False) ->
              myErr ("Malformed participant name " ++ happy_var_1)
            (True, False, False) ->
              myErr ("Malformed participant name " ++ happy_var_3)
            (_, _, True)         ->
              myErr ("Participant " ++ happy_var_1 ++ " cannot be both sender and receiver of a same interaction")
	) `HappyStk` happyRest

happyReduce_17 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_17 = happyReduce 6 5 happyReduction_17
happyReduction_17 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\_ ->
          case (happy_var_1 € happy_var_3, isPtp happy_var_1, happy_var_3) of
            (True, _, _) ->
              myErr (happy_var_1 ++ " must NOT be one of the receivers " ++ (mkSep happy_var_3 ", "))
            (False, False, _) ->
              myErr ("Malformed participant name " ++ happy_var_1)
            (False, True, []) ->
              myErr ("No receiver for " ++ happy_var_1)
            (False, True, s:[]) ->
              Actx (happy_var_1 , s) (happy_var_5 ++ happy_var_6)
            _ ->
              Parx (L.map (\s -> (Actx (happy_var_1 , s) happy_var_5)) happy_var_3)
	) `HappyStk` happyRest

happyReduce_18 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_18 = happyReduce 4 5 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\env ->
          let
            range = [1 .. length happy_var_3]
            brcs = M.fromList $ L.zip range (L.map (\x -> x env) happy_var_3)
          in
            Brax happy_var_1 (M.fromList $ L.zip range (checkTokenx TokenBra (Brax happy_var_1 brcs)))
	) `HappyStk` happyRest

happyReduce_19 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (\env ->
          Seqx (checkTokenx TokenSeq (Seqx [happy_var_1 env, happy_var_3 env]))
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (\env ->
          Parx (checkTokenx TokenPar (Parx [happy_var_1 env, happy_var_3 env]))
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_21 = happyReduce 4 5 happyReduction_21
happyReduction_21 ((HappyTerminal (TokenStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\env ->
          if (isPtp happy_var_4)
          then Repx happy_var_4 (happy_var_2 env)
          else myErr ("Malformed participant name: " ++ happy_var_4)
	) `HappyStk` happyRest

happyReduce_22 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_22 = happyReduce 6 5 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\env ->
          if (isPtp happy_var_2)
          then (Repx happy_var_2 (happy_var_4 env))
          else myErr ("Malformed participant name: " ++ happy_var_2)
	) `HappyStk` happyRest

happyReduce_23 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_23 = happyReduce 5 5 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (\env -> Repx "" (happy_var_3 env)
	) `HappyStk` happyRest

happyReduce_24 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_24 = happySpecReduce_3  5 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1:happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_27 = happySpecReduce_1  7 happyReduction_27
happyReduction_27 (HappyTerminal (TokenQAtt happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_28 = happySpecReduce_0  7 happyReduction_28
happyReduction_28  =  HappyAbsSyn11
		 (""
	)

happyReduce_29 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_29 = happySpecReduce_1  8 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_30 = happyReduce 6 8 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\env ->
        (let
            branches = L.map fst ([happy_var_3 env] ++ (happy_var_5 env))
            aux g l = (checkToken TokenBra (fst g)) ++ l
            tmp = L.foldr aux [] branches
            gcs = M.fromList $ L.zip [1 .. length tmp] tmp
          in
            Bra happy_var_1 gcs,
          ptpsBranches ([happy_var_3 env] ++ (happy_var_5 env))
        )
	) `HappyStk` happyRest

happyReduce_31 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_31 = happySpecReduce_0  9 happyReduction_31
happyReduction_31  =  HappyAbsSyn13
		 (""
	)

happyReduce_32 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_32 = happySpecReduce_3  9 happyReduction_32
happyReduction_32 _
	(HappyTerminal (TokenQStr happy_var_2))
	_
	 =  HappyAbsSyn13
		 ("{ " ++ happy_var_2 ++ " }"
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_33 = happySpecReduce_2  10 happyReduction_33
happyReduction_33 (HappyTerminal (TokenStr happy_var_2))
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_34 = happySpecReduce_2  10 happyReduction_34
happyReduction_34 (HappyTerminal (TokenStr happy_var_2))
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn14
		 (""
	)

happyReduce_36 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn14
		 (""
	)

happyReduce_37 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_37 = happySpecReduce_1  11 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (\env -> [happy_var_1 env]
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_38 = happySpecReduce_3  11 happyReduction_38
happyReduction_38 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (\env -> [happy_var_1 env] ++ (happy_var_3 env)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_39 = happySpecReduce_2  12 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (\env -> checkGuard (happy_var_1 env) happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_40 = happySpecReduce_1  13 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn5
		 (\_ -> (Emp, S.empty)
	)

happyReduce_41 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn5
		 (\_ -> (Brk, S.empty)
	)

happyReduce_42 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn5
		 (\_ -> (Acp [], S.empty)
	)

happyReduce_43 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_43 = happySpecReduce_2  13 happyReduction_43
happyReduction_43 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (\_ -> (Acp happy_var_2, S.fromList happy_var_2)
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_44 = happyReduce 6 13 happyReduction_44
happyReduction_44 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\_ ->
        case ((isPtp happy_var_1), (isPtp happy_var_3), (happy_var_1 == happy_var_3)) of
          (True, True, False) ->
            ((Act (happy_var_1 , happy_var_3) (happy_var_5 ++ happy_var_6)), S.fromList [happy_var_1,happy_var_3])
          (False, False, _) ->
            myErr ("Malformed participant names: " ++ happy_var_1 ++ " and " ++ happy_var_3)
          (False, True, True) ->
            myErr ("Malformed participant name: " ++ happy_var_1 ++ " and sender and receiver must be different")
          (False, True, False) ->
            myErr ("Malformed participant name: " ++ happy_var_1)
          (True, False, False) ->
            myErr ("Malformed participant name: " ++ happy_var_3)
          (_, _, True) ->
            myErr ("Participant " ++ happy_var_1 ++ " cannot be both sender and receiver of a same interaction")
	) `HappyStk` happyRest

happyReduce_45 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_45 = happyReduce 6 13 happyReduction_45
happyReduction_45 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyTerminal (TokenStr happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\_ ->
        case (happy_var_1 € happy_var_3, isPtp happy_var_1, happy_var_3) of
          (True, _, _) ->
            myErr (happy_var_1 ++ " must NOT be one of the receivers " ++ (mkSep happy_var_3 ", "))
          (False, False, _) ->
            myErr ("Malformed participant name: " ++ happy_var_1)
          (False, True, []) ->
            myErr (happy_var_1 ++ " cannot be empty")
          (False, True, s:[]) ->
            ((Act (happy_var_1 , s) (happy_var_5 ++ happy_var_6)), S.fromList([happy_var_1,s]))
          _ ->
            (Par (L.map (\s -> (Act (happy_var_1 , s) happy_var_5)) happy_var_3), S.fromList(happy_var_1:happy_var_3))
	) `HappyStk` happyRest

happyReduce_46 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_46 = happySpecReduce_2  13 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (\env ->
        let
          (const, (g, ptps), params) = happy_var_2 env
          (ctx, formals) = (env M.! const)
          m = M.fromList (L.zip formals params)
          g' = subst m (fill env (applyEnvGC env g) ctx)
          ptps' = S.map (rename m) $ S.union ptps (ctxptps ctx)
        in
          case (checkParameters formals params, M.member const env) of
            (Just err, _) -> myErr err
            (_, True) -> (g', ptps')
            _ -> myErr ("Use of undefined constant: " ++ const)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_47 = happyReduce 4 13 happyReduction_47
happyReduction_47 ((HappyTerminal (TokenStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (-- Note the difference with Ctx on the checks
      \env ->
        let
          (g, ptps) = happy_var_2 env
        in
          case ((isPtp happy_var_4), ( happy_var_4 == "" || S.member happy_var_4 ptps)) of
            (True, True) ->
              (Rep happy_var_4 g , S.insert happy_var_4 ptps)
            (False, _) ->
              myErr ("Malformed participant name: " ++ happy_var_4)
            (True, False) ->
              myErr $ "Participant " ++ happy_var_4 ++
              " is not among the loop's participants: " ++
              (show $ toList ptps)
	) `HappyStk` happyRest

happyReduce_48 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_48 = happyReduce 5 13 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\env ->
        let
          (g, ptps) = happy_var_4 env
        in
          case ((isPtp happy_var_2), (S.member happy_var_2 ptps)) of
            (True, True) ->
              (Rep happy_var_2 g , S.insert happy_var_2 ptps)
            (False, _) ->
              myErr ("Malformed participant name: " ++ happy_var_2)
            (True, False) ->
              myErr $ "Participant " ++ happy_var_2 ++
              " is not among the loop's participants: " ++
              (show $ toList ptps)
	) `HappyStk` happyRest

happyReduce_49 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_49 = happyReduce 4 13 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (\env ->
        let
          (g, ptps) = happy_var_3 env
        in
          (Rep "" g , ptps)
	) `HappyStk` happyRest

happyReduce_50 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_50 = happySpecReduce_3  13 happyReduction_50
happyReduction_50 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (\env ->
        let
          (b1, ptps1) = (happy_var_1 env)
          (b2, ptps2) = (happy_var_3 env)
        in
          (Seq (checkToken TokenSeq (Seq [b1, b2])),
           S.union ptps1 ptps2
          )
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_51 = happySpecReduce_3  13 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_52 = happySpecReduce_2  14 happyReduction_52
happyReduction_52 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn18
		 (\_ -> (happy_var_1, (Emp, S.empty), happy_var_2)
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_53 = happyReduce 5 14 happyReduction_53
happyReduction_53 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\env -> 
        let 
          (g, ptps) = happy_var_3 env
        in
          (happy_var_1, (g, ptps), happy_var_5)
	) `HappyStk` happyRest

happyReduce_54 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_54 = happySpecReduce_1  15 happyReduction_54
happyReduction_54 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_55 = happySpecReduce_2  15 happyReduction_55
happyReduction_55 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_56 = happySpecReduce_2  16 happyReduction_56
happyReduction_56 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_57 = happySpecReduce_0  16 happyReduction_57
happyReduction_57  =  HappyAbsSyn8
		 ([]
	)

happyReduce_58 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_58 = happySpecReduce_1  17 happyReduction_58
happyReduction_58 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_59 = happySpecReduce_2  17 happyReduction_59
happyReduction_59 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_60 = happyReduce 5 18 happyReduction_60
happyReduction_60 ((HappyAbsSyn22  happy_var_5) `HappyStk`
	(HappyTerminal (TokenStr happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (M.insert happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_61 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_61 = happySpecReduce_0  18 happyReduction_61
happyReduction_61  =  HappyAbsSyn22
		 (M.empty
	)

happyReduce_62 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_62 = happySpecReduce_3  19 happyReduction_62
happyReduction_62 (HappyTerminal (TokenStr happy_var_3))
	_
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn22
		 (M.insert happy_var_1 happy_var_3 M.empty
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_63 = happyReduce 4 19 happyReduction_63
happyReduction_63 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	(HappyTerminal (TokenStr happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenStr happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (M.insert happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_64 = happySpecReduce_1  20 happyReduction_64
happyReduction_64 (HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 (if (isPtp happy_var_1)
         then [happy_var_1]
         else myErr ("Malformed participant name: " ++ happy_var_1)
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )
happyReduce_65 = happySpecReduce_2  20 happyReduction_65
happyReduction_65 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenStr happy_var_1))
	 =  HappyAbsSyn8
		 (if (isPtp happy_var_1)
         then
           case happy_var_2 of
             [] ->  [happy_var_1]
             s:l -> (happy_var_1:s:l)
         else myErr ("Malformed participant name: " ++ happy_var_1)
	)
happyReduction_65 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TokenEof -> happyDoAction 31 tk action sts stk;
	TokenQStr happy_dollar_dollar -> cont 1;
	TokenQAtt happy_dollar_dollar -> cont 2;
	TokenStr happy_dollar_dollar -> cont 3;
	TokenArr -> cont 4;
	TokenMAr -> cont 5;
	TokenEq -> cont 6;
	TokenPar -> cont 7;
	TokenBra -> cont 8;
	TokenGrd -> cont 9;
	TokenSta -> cont 10;
	TokenSeq -> cont 11;
	TokenUnt -> cont 12;
	TokenSec -> cont 13;
	TokenCurlyo -> cont 14;
	TokenCurlyc -> cont 15;
	TokenCtxo -> cont 16;
	TokenCtxc -> cont 17;
	TokenAnd -> cont 18;
	TokenEmp -> cont 19;
	TokenHole -> cont 20;
	TokenSel 3 -> cont 21;
	TokenSel 6 -> cont 22;
	TokenBrk -> cont 23;
	TokenAcp -> cont 24;
	TokenRep -> cont 25;
	TokenUnl -> cont 26;
	TokenLet -> cont 27;
	TokenIn -> cont 28;
	TokenDo -> cont 29;
	TokenWith -> cont 30;
	_ -> happyError' (tk, [])
	})

happyError_ explist 31 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Ptype a -> (a -> Ptype b) -> Ptype b
happyThen = (thenPtype)
happyReturn :: () => a -> Ptype a
happyReturn = (returnPtype)
happyParse :: () => Prelude.Int -> Ptype (HappyAbsSyn )

happyNewToken :: () => Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )

happyDoAction :: () => Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn )

happyReduceArr :: () => Happy_Data_Array.Array Prelude.Int (Prelude.Int -> Token -> Prelude.Int -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> Ptype (HappyAbsSyn ))

happyThen1 :: () => Ptype a -> (a -> Ptype b) -> Ptype b
happyThen1 = happyThen
happyReturn1 :: () => a -> Ptype a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Ptype a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
gcgrammar = happySomeParser where
 happySomeParser = happyThen (happyParse 0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token =
  TokenStr String
  | TokenQStr String
  | TokenQAtt String
  | TokenEmp
  | TokenBrk
  | TokenArr
  | TokenPar
  | TokenBra
  | TokenSel Int
  | TokenGrd
  | TokenSeq
  | TokenRep
  | TokenSta
  | TokenUnt
  | TokenSec
  | TokenCom
  | TokenMAr
  | TokenUnl
  | TokenCurlyo
  | TokenCurlyc
  | TokenCtxo
  | TokenCtxc
  | TokenLet
  | TokenAnd
  | TokenIn
  | TokenDo
  | TokenWith
  | TokenAcp
  | TokenEq
  | TokenHole
  | TokenEof
  deriving (Show)


lexer :: (Token -> Ptype a) -> Ptype a
lexer cont s (l, c) (l',c') =
  -- (l,c) is the currently reached position in the parsing
  -- (l',c') is the position of the last accepted token
  case s of
    'b':'r':'e':'a':'k':r ->
      cont TokenBrk r' (l, c + (length s')) (l, c)
    'a':'c':'c':'e':'p':'t':x:r ->
      case x of
        ' '  -> cont TokenAcp r (l, (c+6)) (l, c)
        '\t' -> cont TokenAcp r (l, (c+6)) (l, c)
        '\n' -> cont TokenAcp r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'b':'r':'a':'n':'c':'h':x:r ->
      case x of
        ' '  -> cont (TokenSel 6) r (l, (c+7)) (l, c)
        '\t' -> cont (TokenSel 6) r (l, (c+7)) (l, c)
        '{'  -> cont (TokenSel 6) ('{':r) (l, (c+6)) (l, c)
        '\n' -> cont (TokenSel 6) r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'r':'e':'p':'e':'a':'t':x:r ->
      case x of
        ' '  -> cont TokenRep r (l, (c+7)) (l, c)
        '\t' -> cont TokenRep r (l, (c+7)) (l, c)
        '{'  -> cont TokenRep ('{':r) (l, (c+6)) (l, c)
        '\n' -> cont TokenRep r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    -- 'u':'n':'l':'e':'s':'s':x:r ->
    --   case x of
    --     ' '  -> cont TokenUnl r (l, (c+7)) (l, c)
    --     '\t' -> cont TokenUnl r (l, (c+7)) (l, c)
    --     '{'  -> cont TokenUnl ('{':r) (l, (c+6)) (l, c)
    --     '\n' -> cont TokenUnl r ((l+1), 0) (l, c)
    --     _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'l':'e':'t':x:r ->
      case x of
        ' '  -> cont TokenLet r (l, (c+4)) (l, c)
        '\t' -> cont TokenLet r (l, (c+4)) (l, c)
        '{'  -> cont TokenLet ('{':r) (l, (c+3)) (l, c)
        '\n' -> cont TokenLet r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    's':'e':'l':x:r ->
      case x of
        ' '  -> cont (TokenSel 3) r (l, (c+4)) (l, c)
        '\t' -> cont (TokenSel 3) r (l, (c+4)) (l, c)
        '{'  -> cont (TokenSel 3) ('{':r) (l, (c+3)) (l, c)
        '\n' -> cont (TokenSel 3) r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'i':'n':x:r ->
      case x of
        ' '  -> cont TokenIn r (l, (c+3)) (l, c)
        '\t' -> cont TokenIn r (l, (c+3)) (l, c)
        '{'  -> cont TokenIn ('{':r) (l, (c+2)) (l, c)
        '\n' -> cont TokenIn r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'd':'o':x:r ->
      case x of
        ' '  -> cont TokenDo r (l, (c+3)) (l, c)
        '\t' -> cont TokenDo r (l, (c+3)) (l, c)
        '{'  -> cont TokenDo ('{':r) (l, (c+2)) (l, c)
        '\n' -> cont TokenDo r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    'w':'i':'t':'h':x:r ->
      case x of
        ' '  -> cont TokenWith r (l, (c+5)) (l, c)
        '\t' -> cont TokenWith r (l, (c+5)) (l, c)
        '{'  -> cont TokenWith ('{':r) (l, (c+4)) (l, c)
        '\n' -> cont TokenWith r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    's':'q':'o':'s':r ->
      let
        (q,r') = L.span (\x -> x /= '}') r
      in
        (cont (TokenQStr $ "sqos" ++ q)) r' (l, c + (length q) + (length "sqos")) (l, c)
    'r':'q':'o':'s':r ->
      let
        (q,r') = L.span (\x -> x /= '}') r
      in
        (cont (TokenQStr $ "rqos" ++  q)) r' (l, c + (length q) + (length "rqos")) (l, c)
    'q':'o':'s':r ->
      let
        (q,r') = L.span (\x -> x /= '}') r
        x:q' = dropWhile (\x -> x == ' ') q
      in
        if x == '{'
          then (cont (TokenQAtt q') (tail r') (l, c + (length q) + 1) (l, c))
          else Er ("Syntax error at <" ++ (show $ l) ++ "," ++ (show $ c) ++ ">: " ++ "expected '{' after 'qos'.")
    '(':'o':')':r ->
      cont TokenEmp r (l, c+3) (l, c)
    '.':'.':r ->
      (lexer cont) (dropWhile (\c->c/='\n') r) (l, 0) (l',c')
    '-':'>':r ->
      cont TokenArr r (l, (c+2)) (l, c)
    '=':'>':r ->
      cont TokenMAr r (l, (c+2)) (l, c)
    '[':']':r -> cont TokenHole r (l, (c+2)) (l, c)
    '[':'[':r ->
      let
        takeComment acc s =
          case s of
            ']':']':_ -> acc
            _ -> takeComment (acc ++ [head s]) (tail s)
        tmp = takeComment "" r
        lskip = l + L.length (L.filter (\c -> c == '\n') tmp)
        cskip = 0 -- c + if lskip==0 then (length tmp) else 0
      in
        if tmp == r
        then Er ("Syntax error at <" ++ (show $ l+1) ++ "," ++ (show $ c) ++ ">: " ++ "multiline comment not closed")
        else lexer cont (tail $ tail (r \\ tmp)) (lskip, cskip) (l', c')
    x:r ->
      case x of
        '&' -> cont TokenAnd r (l, (c+1)) (l, c)
        '*' -> cont TokenSta r (l, (c+1)) (l, c)
--         '%' -> cont TokenGrd r (l, (c+1)) (l, c)
        '@' -> cont TokenUnt r (l, (c+1)) (l, c)
        ':' -> cont TokenSec r (l, (c+1)) (l, c)
        ';' -> cont TokenSeq r (l, (c+1)) (l, c)
        '|' -> cont TokenPar r (l, (c+1)) (l, c)
        '+' -> cont TokenBra r (l, (c+1)) (l, c)
        '=' -> cont TokenEq r (l, (c+1)) (l, c)
        '{' -> cont TokenCurlyo r (l, (c+1)) (l, c)
        '}' -> cont TokenCurlyc r (l, (c+1)) (l, c)
        '[' -> cont TokenCtxo r (l, (c+1)) (l, c)
        ']' -> cont TokenCtxc r (l, (c+1)) (l, c)
        ' ' -> (lexer cont) r (l, (c+1)) (l', c')
        '\t' -> (lexer cont) r (l, (c+1)) (l', c')
        '\n' -> (lexer cont) r ((l+1), 0) (l', c')
        _ -> (cont (TokenStr s')) r' (l, c + (length s')) (l, c)
    [] ->
      (cont TokenEof) "" (l, c) (l',c')
  where
    (s',r') = span (\c -> not (c € separators)) s
  

data ParseResult a =
  Ok a
  | Er String
  deriving (Show)


type Ptype a = String -> (Int, Int) -> (Int, Int) -> ParseResult a


parseError token =
  \ _ _ (l, c) ->
    Er (synErr l c token)


thenPtype :: Ptype a -> (a -> Ptype b) -> Ptype b
m `thenPtype` k = \s (l, c) (l', c') ->
  case m s (l, c) (l', c') of
    Ok v -> k v s (l, c) (l',c')
    Er e -> Er e


returnPtype :: a -> Ptype a
returnPtype a = \s _ _ -> Ok a


failPtype :: String -> Ptype a
failPtype err = \_ _ _ -> Er err


-- GC specific stuff

-- G-choreographies constants and contexts

type GCConst = String

type GCEnv = M.Map GCConst (GCCtx, [String])

type QDec = String

emptyEnv :: GCEnv 
emptyEnv = M.empty


applyEnvGC :: GCEnv -> GC -> GC
applyEnvGC env g =
  {- PRE: for all x in M.keys env, env ! x is a constant-free context

     POST: substitute the uses in g of constants in M.keys env with
           the corresponding definition in env and passes parameters
  -}
  (L.foldr (\const -> aux const) g (M.keys env))
    where
      aux :: GCConst -> GC -> GC
      aux const = \c ->
        case c of
          Par gs -> Par (checkToken TokenPar (Par (L.map (applyEnvGC env) gs)))
          Bra p brc ->
            let
              tmp = L.foldr (++) [] (L.map (checkToken TokenBra) (M.elems brc))
              gcs = M.fromList $ L.zip [1 .. length tmp] tmp
            in
              Bra p gcs
          Seq gs -> Seq (checkToken TokenSeq (Seq (L.map (applyEnvGC env) gs)))
          Rep ptp g' -> Rep ptp (applyEnvGC env g')
          _ -> g


data GCCtx = Hole
  | Dox GCConst ([String]) GCCtx
  | Parx [GCCtx]
  | Brax Ptp (M.Map Label GCCtx)
  | Seqx [GCCtx]
  | Repx Ptp GCCtx
  | Actx Channel Message
  | Empx
  | Brkx
  | Acpx [Ptp]
           deriving (Show)


compEnv :: GCEnv -> GCEnv -> GCEnv
compEnv env env' =
  let
    common = M.intersection env env'
  in
    if (M.null common)
    then M.union env env'
    else myErr ("Double definition of constants: " ++ (mkSep (M.keys common) ", "))


compCtx :: (GCCtx, [String]) -> ([String]) -> GCCtx -> GCCtx
compCtx (ctx, formals) params ctx' =
-- replace the holes in ctx with ctx' and formal parameters with params
  case checkParameters formals params of
    Just err -> myErr err
    _ ->
      let
         m = M.fromList (L.zip formals params)
       in
         case ctx of
           Hole -> ctx'
           Parx ctxs -> Parx (L.map (\c -> compCtx (c, formals) params ctx') ctxs)
           Brax p ctxs -> Brax p (M.map (\c -> compCtx (c, formals) params ctx') ctxs)
           Seqx ctxs -> Seqx (L.map (\c -> compCtx (c, formals) params ctx') ctxs)
           Repx ptp ctx'' -> Repx ptp (compCtx (ctx'', formals) params ctx')
           Actx (s,r) msg -> Actx (rename m s, rename m r) (rename m msg)
           Dox const params' ctx'' -> Dox const (L.map (rename m) params') ctx''


checkParameters :: [String] -> [String] -> Maybe String
checkParameters formals params =
  case (formals, params) of
    (p:formals', p':params') ->
      if (isPtp p) && (isPtp p')
      then checkParameters formals' params'
      else Just ("Participants cannot be messages and viceversa: " ++ (show p) ++ (show p'))
    ([], _:_) -> Just ("Not enough formal parameters: " ++ (mkSep params ", ") ++ " cannot replace " ++ (mkSep formals ", "))
    (_:_, []) -> Just ("Not enough actual parameters: " ++ (mkSep params ", ") ++ " cannot replace " ++ (mkSep formals ", "))
    _ -> Nothing


applyEnvCtx :: GCEnv -> GCCtx -> GCCtx
applyEnvCtx env ctx =
  {- PRE: for all x in M.keys env, env ! x is a constant-free context

     POST: substitute the uses in ctx of constants in M.keys env with
           the corresponding definition in env
  -}
  L.foldr (\const -> aux const) ctx (M.keys env)
    where
      aux :: GCConst -> GCCtx -> GCCtx
      aux const = \c ->
        case c of
          Dox const params ctx' ->
            aux const (compCtx (env M.! const) params ctx')
          Dox _ _ _ -> myErr ("Use of undefined constant: " ++ const)
          Parx ctxs -> Parx (L.map (applyEnvCtx env) ctxs)
          Brax p brc -> Brax p (M.map (applyEnvCtx env) brc)
          Seqx ctxs -> Seqx (L.map (applyEnvCtx env) ctxs)
          Repx ptp ctx' -> Repx ptp (applyEnvCtx env ctx')
          _ -> c


substx :: M.Map String String -> GCCtx -> GCCtx
substx m ctx =
  case ctx of
    Dox const params ctx' -> Dox const (L.map (rename m) params) (substx m ctx')
    Parx ctxs -> Parx (L.map (substx m) ctxs)
    Brax p brc -> Brax p (M.map (substx m) brc)
    Seqx ctxs -> Seqx (L.map (substx m) ctxs)
    Repx ptp ctx' -> Repx ptp (substx m ctx')
    Actx (s,r) msg -> Actx (rename m s, rename m r) (rename m msg)
    _ -> ctx


subst :: M.Map String String -> GC -> GC
subst m g =
  case g of
    Par gcs -> Par (L.map (subst m) gcs)
    Bra p brc -> Bra p (M.map (subst m) brc)
    Seq gcs -> Seq (L.map (subst m) gcs)
    Rep ptp gc' -> Rep ptp (subst m gc')
    Act (s,r) msg -> Act (rename m s, rename m r) (rename m msg)
    Emp -> Emp


insEnv :: GCConst -> ([String]) -> (GCEnv -> GCCtx) -> GCEnv -> GCEnv
insEnv const params absCtx env =
  {- PRE: for all x in M.keys env, env ! x is a constant-free context

     POST: insert in env the constant-free version of (absCtx env)
           obtained by replacing each x in M.keys env with its
           corresponding definition in env;
  -}
  if (M.member const env)
  then myErr ("Double definition of constant " ++ const)
  else
    let
      ctx = absCtx env
      ctx' = applyEnvCtx env ctx
    in
      case checkDuplicates params of
        Just err -> myErr err
        _ -> M.insert const (ctx', params) env


ctxptps :: GCCtx -> Set Ptp
ctxptps = \c ->
  case c of
    Parx ctxs -> L.foldr S.union S.empty (L.map ctxptps ctxs)
    Brax p brc -> L.foldr S.union S.empty (L.map ctxptps (M.elems brc))
    Seqx ctxs -> L.foldr S.union S.empty (L.map ctxptps ctxs)
    Repx ptp ctx -> S.insert ptp (ctxptps ctx)
    Actx (s,r) m -> S.fromList [s,r]
    _ -> S.empty


fill :: GCEnv -> GC -> GCCtx -> GC
fill env g ctx =
{- 
  PRE:  g and ctx are constant-free

  POST: return the g-choreography obtained by replacing the holes in ctx with g
-}
-- replace the holes in ctx with gc
  case ctx of
    Dox const _ _ -> myErr ("???" ++ "impossible invocation of " ++ const)
    Parx ctxs -> Par (L.map (fill env g) ctxs)
    Brax p ctxs -> Bra p (M.map (fill env g) ctxs)
    Seqx ctxs -> Seq (L.map (fill env g) ctxs)
    Repx ptp ctx' -> Rep ptp (fill env g ctx')
    Actx c m -> Act c m
    _ -> g


fillx :: GCEnv -> GC -> GCCtx -> GCCtx
fillx env g ctx =
  case ctx of
    Hole -> contexify g
    Dox const _ _ -> myErr ("???" ++ "impossible invocation of " ++ const)
    Parx ctxs -> Parx (L.map (fillx env g) ctxs)
    Brax p ctxs -> Brax p (M.map (fillx env g) ctxs)
    Seqx ctxs -> Seqx (L.map (fillx env g) ctxs)
    Repx ptp ctx' -> Repx ptp (fillx env g ctx')
    _ -> contexify g


contexify :: GC -> GCCtx
contexify g =
  case g of
    Par gcs -> Parx (L.map contexify gcs)
    Bra p brc -> Brax p (M.map contexify brc)
    Seq gcs -> Seqx (L.map contexify gcs)
    Rep ptp gc' -> Repx ptp (contexify gc')
    Act c m -> Actx c m
    Emp -> Empx
    Brk -> Brkx
    Acp ptps -> Acpx ptps


synErr :: Int -> Int -> Token -> String
synErr l c token =
  "Syntax error at <" ++
	(show (l+1)) ++ "," ++
	(show $ c+1) ++ ">: " ++
	err
  where
    err =
      case token of
        TokenStr s  ->  "unexpected or malformed string: \'" ++ s ++ "\'\n\t characters in " ++ (show separators) ++ " are forbidden"
        TokenQStr s ->  "unexpected or malformed qos string: \'" ++ s ++ "\'"
        TokenQAtt s ->  "unexpected or malformed qos string: \'" ++ s ++ "\'"
        TokenEmp    ->  "unexpected \'(o)\'"
        TokenBrk    ->  "unexpected \'break\'"
        TokenAcp    ->  "unexpected \'accept\'"
        TokenArr    ->  "unexpected \'->\'"
        TokenPar    ->  "unexpected \'|\'"
        TokenBra    ->  "unexpected \'+\'"
        TokenSel o  ->  "unexpected " ++ (if o == 6 then "branch" else "sel")
        TokenGrd    ->  "unexpected \'unless\'"
        TokenSeq    ->  "unexpected \';\'"
        TokenRep    ->  "unexpected loop \'repeat\'"
        TokenSta    ->  "unexpected loop \'*\'"
        TokenUnt    ->  "unexpected \'@\'"
        TokenSec    ->  "unexpected \':\'"
        TokenCom    ->  "unexpected \',\'"
        TokenMAr    ->  "unexpected =>"
        TokenUnl    ->  "unexpected \'unless\' clause"
        TokenCurlyo ->  "unexpected \'{\'"
        TokenCurlyc ->  "unexpected \'}\'"
        TokenCtxo   ->  "unexpected \'[\'"
        TokenCtxc   ->  "unexpected \']\'"
        TokenLet    ->  "unexpected \'let\'"
        TokenAnd    ->  "unexpected \'&\'"
        TokenIn     ->  "unexpected \'in\'"
        TokenDo     ->  "unexpected \'do\'"
        TokenWith   ->  "unexpected \'with\'"
        TokenHole   ->  "unexpected \'[]\'"
        TokenEq     ->  "unexpected \'=\'"
        TokenEof    ->  "Perhaps an unexpected trailing symbol"


myErr :: String -> a
myErr err = error ("gcparser: ERROR - " ++ err)


checkDuplicates :: [String] -> Maybe String
checkDuplicates l =
  case l of
    [] -> Nothing
    p:l' ->
      if (p € l')
      then Just ("Duplicated parameter: " ++ (show p))
      else checkDuplicates l'


rename :: M.Map String String -> String -> String
rename m x =
  if x € (M.keys m)
  then m M.! x
  else x


ptpsBranches :: [((GC, Set Ptp), ReversionGuard)] -> Set Ptp
-- to be revised: also participants in constants to be taken
ptpsBranches =
  \l -> L.foldr S.union S.empty (L.map (snd . fst) l)


checkGuard :: (GC, Set Ptp) -> ReversionGuard -> ((GC, Set Ptp), ReversionGuard)
checkGuard gc@(g, ptps) m =
  let
    tmp = [ x | x <- M.keys m, not (S.member x ptps) ]
  in
    if L.null tmp
    then (gc, m)
    else myErr ("Unknown participant" ++ (if L.length tmp > 1 then "(s): " else "") ++ (mkSep tmp ", "))


checkToken :: Token -> GC -> [GC]
checkToken t g =
-- flatten parallel and sequential composition
  case t of
    TokenPar -> case g of
      Par l -> L.foldr (++) [] (L.map (checkToken t) l)
      _ -> [g]
    TokenBra -> case g of
      Bra _ l -> L.foldr (++) [] (L.map (checkToken t) (M.elems l))
      _ -> [g]
    TokenSeq -> case g of
      Seq l -> L.foldr (++) [] (L.map (checkToken t) l)
      _ -> [g]
    _        -> [g]


checkTokenx :: Token -> GCCtx -> [GCCtx]
checkTokenx t ctx =
  case t of
    TokenPar -> case ctx of
      Parx l -> L.foldr (++) [] (L.map (checkTokenx t) l)
      _ -> [ctx]
    TokenBra -> case ctx of
      Brax _ l -> L.foldr (++) [] (L.map (checkTokenx t) (M.elems l))
      _ -> [ctx]
    TokenSeq -> case ctx of
      Seqx l -> L.foldr (++) [] (L.map (checkTokenx t) l)
      _ -> [ctx]
    _        -> [ctx]
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                (0)           -> {- nothing -}
                                     happyFail (happyExpListPerState ((st) :: Prelude.Int)) i tk st
                (-1)          -> {- nothing -}
                                     happyAccept i tk st
                n | (n Prelude.< ((0) :: Prelude.Int)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((Prelude.negate ((n Prelude.+ ((1) :: Prelude.Int)))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Prelude.- ((1) :: Prelude.Int))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Prelude.+ i)
         check  = if (off_i Prelude.>= ((0) :: Prelude.Int))
                  then (indexShortOffAddr happyCheck off_i Prelude.== i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st












indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Prelude.+ nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Prelude.+ nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
