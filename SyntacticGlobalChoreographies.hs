--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--

{-
	Project a syntactic g-choreography, calculate
	its semantics, and output it in dot format.
-}

{-# LANGUAGE InstanceSigs #-}

module SyntacticGlobalChoreographies where

import Data.Set as S
import Data.List as L
import Data.Map.Strict as M        -- TODO: consider using Data.IntMap.Strict
import Misc
import CFSM (CFSM,
             State,
             Ptp,
             Dir(Receive, Tau, Send),
             Channel,
             P,
             LTrans,
             replaceState,
             replaceStates,
             addTrans,
             cfsmUnion,
             initialOf,
             emptyCFSM,
             transitionsOf,
             acceptingTrx,
             isAccepting,
             acceptingOf,
             actionsOf,
             msgOf,
             eventOf,
             statesOf,
             stateProd,
             cfsmProd,
             finalStates,
             messageOf,
             isReceive
            )
import DotStuff (gcsizenode,
                 DotNode,
                 breakV,
                 sourceV,
                 forkV,
                 branchV,
                 mergeV,
                 joinV,
                 sinkV,
                 emptyI
                )
import Data.String.Utils (replace)
import Data.String.Utils as Str
import QoS.CFSM ( QoSSpec, QoSCFSM , QosAnnotation)
import Data.Hashable ( Hashable(hashWithSalt) )
import QoS.CFSM.Lexer (Token(QoSSpecs))
import Language.SMTLIB as SMTLIB ( parseTerm )

-- Simple internal representation of labels of branches
type Label = Int

{-
   A syntactic global graph is a set of nodes, a source, a sink, and a
   set of edges We assume that cp's will be automatically generated
   (uniquely) during parsing
-}
data GC = Emp
        | Brk
        | Acp [Ptp]
        | Act Channel Message
        | Par [GC]
        | Bra Ptp (Map Label GC)
        | Seq [GC]
        | Rep Ptp GC
    deriving (Eq, Ord, Show)

instance Hashable GC where
    hashWithSalt :: Int -> GC -> Int
    hashWithSalt s gc = case gc of 
                          Emp         -> hashWithSalt s (0::Int)
                          (Act c m)   -> hashWithSalt s (1::Int, c, m)
                          (Par gs)    -> hashWithSalt s (2::Int, gs)
                          (Bra p lg)   -> hashWithSalt s (3::Int, p, lg)
                          (Seq gs)    -> hashWithSalt s (4::Int, gs)
                          (Rep p g)   -> hashWithSalt s (5::Int, p, g)



-- Basic functions to check the nature of a g-choreography
isAct :: GC -> Bool
isAct (Act _ _) = True
isAct _ = False

isBra :: GC -> Bool
isBra (Bra _ _) = True
isBra _ = False

isEmp :: GC -> Bool
isEmp Emp = True
isEmp _ = False

isBrk :: GC -> Bool
isBrk Brk = True
isBrk _ = False

-- Other auxiliary operations

gcptps :: GC -> Set Ptp
gcptps gc =
-- return the set of participants of gc
  case gc of
    Emp -> S.empty
    Brk -> S.empty
    Act (s,r) _  -> S.fromList [s,r]
    Par gs -> L.foldr S.union S.empty (L.map gcptps gs)
    Bra p gs -> L.foldr S.union S.empty (L.map gcptps (M.elems gs))
    Seq gs -> L.foldr S.union S.empty (L.map gcptps gs)
    Rep p g' -> S.insert p (gcptps g')

loopBack :: State -> State
loopBack q = "__l__" ++ q

loopExit :: State -> State
loopExit q = "__e__" ++ q

brkSymb = '#'

brkMsg :: State -> State
brkMsg q = "__" ++ [brkSymb] ++ "__" ++ q

brkStates :: CFSM -> Set State
brkStates cfsm =
  S.map gtarget (S.filter helper (transitionsOf cfsm))
  where
    helper (s,l,t) = brkSymb € (msgOf l)

simplifyGC :: GC -> GC
simplifyGC gc =
{-
  simplifies gc by flattening nested | and + according to the
  following structural congruence rules are:

	(o) + gc = gc
	( GC, _|_, (o) ) abelian monoid
	( GC, _;_, (o) ) monoid
-}
  case gc of
    Seq gcs ->
      let
        gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Seq gcs'
    Par gcs ->
      let
        gcs' = [g | g <- (L.map simplifyGC gcs), g /= Emp]
      in
        case gcs' of
          [] -> Emp
          [gc'] -> gc'
          _ -> Par gcs'
    Bra sel gcs ->
      let
        shift k m =
          M.fromList [(i+k,v) | (i,v) <- M.toList m]
        (bra, oth) =
          L.partition isBra (M.elems $ M.map simplifyGC gcs)
        ms =
          [m | Bra sel m <- bra]
        aux =
          M.fromList $ L.zip ([1 .. (L.length oth)]) oth
        gcs' =
          L.foldl (\x y -> M.union x (shift (M.size x) y)) aux ms
      in
        if (S.size $ S.fromList $ M.elems gcs') == 1 
        then L.head $ M.elems gcs'
        else Bra sel gcs'
    Rep p gc' ->
      let body = simplifyGC gc'
      in
        case body of
          Emp -> Emp
          _ -> Rep p body
    _ -> gc

removeTopLvlBreak :: GC -> GC
removeTopLvlBreak gc =
  case gc of
    Brk -> Emp
    Par gs -> Par (L.map removeTopLvlBreak gs)
    Bra p gs -> Bra p (M.map removeTopLvlBreak gs)
    Seq gs -> Seq (L.map removeTopLvlBreak gs)
    _ -> gc

removeTopLvlBrkBranch :: GC -> GC
removeTopLvlBrkBranch gc =
  case gc of
    Brk -> error "removeTopLvlBrkBranch: there was a Break in an unexpected place."
    Par gs -> Par (L.map removeTopLvlBrkBranch gs)
    Seq gs -> Seq (L.map removeTopLvlBrkBranch gs)
    Bra p gs -> Bra p (M.map removeTopLvlBrkBranch (M.filter (not . endsInBrk) gs))
    _ -> gc
  where 
    endsInBrk g = case g of
      Seq gs -> isBrk (L.last gs) 
      _ -> False

unfoldRepBody :: Int -> GC -> GC
unfoldRepBody n gc =
  -- PRE: gc is well-formed and it is the body of a Rep
  -- POST: returns the n-unfold of gc
  case n' of 
    0 -> Emp
    1 -> removeTopLvlBreak gc
    _ -> Seq $ (L.replicate (n' - 1) (removeTopLvlBrkBranch gc)) ++ [removeTopLvlBreak gc]
  where n' = abs n

unfoldGCUpTo :: Int -> GC -> Set GC
unfoldGCUpTo n gc =
{-
   PRE: n >= 0
   POST: each loop in gc is unfolded from 0 to n times
-}
  case gc of 
    Par gcs -> S.map Par $ cartesianProd (L.map (unfoldGCUpTo n) gcs)
    Bra p gcsmap -> S.map (Bra p) (cartesianProdMap (M.map (unfoldGCUpTo n) gcsmap))
    Seq gcs -> S.map Seq $ cartesianProd (L.map (unfoldGCUpTo n) gcs)
    Rep _ g -> S.unions [ S.map (unfoldRepBody n') (unfoldGCUpTo n g) | n' <- [0 .. n] ]
    _ -> S.singleton gc

    where cartesianProd :: (Ord a) => [Set a] -> Set [a]
          cartesianProd = S.fromList . sequence . (L.map S.toList)

          cartesianProdMap :: Map Label (Set GC) -> Set (Map Label GC)
          cartesianProdMap m = 
            let
              ss = M.elems m
              keys = M.keys m
              ss' = sequence $ (L.map S.toList) ss
            in
              S.fromList [ M.fromList (L.zip keys ss'') | ss'' <- ss' ]


unfoldGC :: Int -> GC -> GC
unfoldGC n gc =
{-
   PRE: n >= 0
   POST: each loop in gc is unfolded n times
-}
    case gc of
      Par gcs -> Par (L.map (unfoldGC n) gcs)
      Bra p gcs -> simplifyGC (Bra p (M.map (unfoldGC n) gcs))
      Seq gcs -> Seq (L.map (unfoldGC n) gcs)
      Rep _ g -> unfoldGC n (unfoldRepBody n g)
      Brk -> error "unfoldGC: \'break\' not allowed here"
      _ -> gc

getQoSCFSM :: CFSM -> QoSCFSM
getQoSCFSM cfsm = (removeQosFromCFSM cfsm, getQosMap $ transitionsOf cfsm, S.empty)
  where getQosMap :: Set LTrans -> Map State QosAnnotation
        getQosMap ts = M.unionsWith f (S.map getQosMap' ts)

        f :: QosAnnotation -> QosAnnotation -> QosAnnotation
        f (Just q1) Nothing = Just q1
        f Nothing (Just q2) = Just q2
        f (Just q1) (Just q2) = 
          if q1 == q2 then Just q1
                      else error $ "getQosMap: two different qos annotations for the same state: " ++ show q1 ++ ", " ++ show q2 ++ "."
        f Nothing Nothing = Nothing

        getQosMap' :: LTrans -> Map State QosAnnotation
        getQosMap' (s1, act, s2) = 
          let qosmap = snd $ parseMessage $ messageOf act
          in M.fromList $ if isReceive act then [(f' s1, M.lookup "rqos" qosmap), (f' s2, M.lookup "rqos'" qosmap)]
                                           else [(f' s1, M.lookup "sqos" qosmap), (f' s2, M.lookup "sqos'" qosmap)]

        qs = statesOf cfsm
        f' s = show $ S.findIndex s qs                               

removeQosFromCFSM :: CFSM -> CFSM
removeQosFromCFSM (sts, q0, acts, trxs) = 
  let acts' = S.map (\(c,d,m) -> (c,d,fst $ parseMessage m)) acts
      trxs' = S.map (\(s1, (c,d,m), s2) -> (s1, (c,d,fst $ parseMessage m), s2)) trxs
  in (sts, q0, acts', trxs')

parseMessage :: String -> (Message, Map String QoSSpec)
parseMessage str =
  let trimmedStr = dropWhile (== ' ') str
      message = takeWhile (/= '{') trimmedStr
      body = takeWhile (/= '}') $ dropWhile (== '{') (dropWhile (/= '{') trimmedStr)
      components = L.map strip $ Str.split "," body
      qosmap = M.fromList [ (k, SMTLIB.parseTerm v) | [k,v] <- L.map (L.map strip . Str.split ":") components]
  in (message , qosmap)

proj :: GC -> State -> (Set(Ptp), Set(Ptp)) -> State -> Ptp -> P -> (CFSM, State)
proj gc q0 inLoop qe p pmap =
  {- 
     PRE:
	- actions are well formed (wffActions) ^ q0 /= qe
        - inLoop is the set of participant in a loop body starting at q0
	- p is a participant of gc
	- the participants of gc are all and only those in M.elems pmap
     POST:
	the non-minimised projection of gc wrt p and a unique
        interface state (it must always exist!)  q0 must be the
        initial state of the resulting cfsm and qe is the interface
        state, respectively
  -}
  let
    onetr q1 q2 l =
      (((S.fromList [q1, q2]), q1, S.singleton l, S.singleton (q1, l, q2)), q2)
    inverse x =
      show $ (inv pmap)!x
    pTau =
      ((inverse p, inverse p), Tau, "")
    ctrLoop (p, ptps) m =
      let
        tmp = L.map (\r -> Act (S.elemAt 0 p,r) m) (S.toList $ S.difference ptps p)
      in
        if (L.length tmp) > 1
        then Seq tmp
        else head tmp
    danglingBrk m q =
      S.member q (brkStates m) && S.null (goutgoing m q)
  in
    case gc of
      Emp ->
        (
          (S.fromList [q0,qe], q0, S.singleton pTau, transitionsOf (fst $ onetr q0 qe pTau)),
          qe
        )
      Acp ptps ->
        {-
           For the moment we use a block-policy: accepting states are only those
           of participants in gc; using pmap we can make this uniform on all participants.
        -}
        if L.null ptps || p € ptps
        then
          (
            (S.singleton q0, q0, S.singleton pTau, S.singleton $ acceptingTrx p q0),
            q0
          )
        else
          (
            (S.fromList [q0,qe], q0, S.singleton pTau, transitionsOf (fst $ onetr q0 qe pTau)),
            qe
          )
      Act (s,r) m ->
        case ( p==s, p==r ) of
          (False, False) -> onetr q0 qe pTau
          _ -> onetr q0 qe label
            where
              label =
                if p==s
                then ((inverse p, inverse r), Send, m)
                else ((inverse s, inverse p), Receive, m)
      Par gcs ->
        (replaceState (initialOf m) q0 m, qe)
        where
          m   = replaceState qe' qe (cfsmProd $ L.map fst mps)
          qe' = L.foldr stateProd "" (L.map snd mps)
          mps = L.map (\g -> proj g q0 inLoop qe p pmap) gcs
      Bra _ gcs ->
        (replaceStates final qe cfsm, qe)
        where
          final q =
            S.member q (S.difference (finalStates cfsm) (S.filter (danglingBrk cfsm) (statesOf cfsm)))
          cfsm =
            replaceStates aux q0 (states, q0, acts, trxs)
          aux =
            \q_ -> q_ € [q0 ++ (show i) | i <- [1 .. (M.size gcs)]]
          (states, acts, trxs) =
            L.foldl
              (\(x,y,z) m -> (S.union x (statesOf m),
                              S.union y (actionsOf m),
                              S.union z (transitionsOf m)
                             )
              )
            (S.singleton qe, S.empty, S.empty)
            mps
          mps =
            L.map (\(i,g) -> fst (proj g q0 inLoop (qe ++ (show i)) p pmap)) (M.toList gcs)
      Seq gcs ->
        let
          len =
            length gcs
          gcs' =
            L.zip [1 .. len] gcs
          aux (i, g) =
            (i, proj g (q0 ++ (show i)) inLoop (qe ++ (show i)) p pmap)
          tmp =
            M.fromList $ L.map aux gcs'
          helper i =
            let
              (mi, qei) = tmp!i
            in
              if i == 1
              then replaceState (initialOf mi) q0 mi
              else
                if i == len
                then
                  replaceStates (\q -> q == qei) qe (replaceState (initialOf mi) (snd $ tmp!(i-1)) mi)
                else replaceState (initialOf mi) (snd $ tmp!(i-1)) mi
        in
          (cfsmUnion q0 (L.map helper [1 .. len]), qe)
      Brk ->
        let
          (p', ptpsLoop) =
            if (S.null $ fst inLoop)
            then ("", [])
            else (S.elemAt 0 (fst inLoop), S.toList $ snd inLoop)
        in
          case (q0 == qe, p € ptpsLoop, p == p') of
            (True, _, _) ->
              error $ q0 ++ "Self-loop on break!"
            (False, _, True) ->
                proj (ctrLoop inLoop (brkMsg q0)) q0 inLoop qe p pmap
            (False, True, False) ->
              onetr q0 qe ((inverse p', inverse p), Receive, (brkMsg q0))
            (False, False, False) ->
              onetr q0 qe pTau
      Rep p' g ->
        if S.member p (snd ptpsloop)
        then m
        else onetr q0 qe pTau
        where
          ptpsloop =
            (S.singleton p', gcptps g)
          brks =
            S.filter (\t -> brkSymb € (msgOf $ eventOf t)) (transitionsOf (fst m))
          brkPt q =
              S.member q (S.map gtarget brks)
          m =
            let
              (q0', qe') = ('_':q0,'_':qe)
              (loop, qel) =
                if p == p'
                then proj (ctrLoop ptpsloop (loopBack q0)) q0 ptpsloop q0' p pmap
                else onetr q0 q0' ((inverse p', inverse p), Receive, (loopBack q0))
              (exit, qee) =
                if p == p'
                then proj (ctrLoop ptpsloop (loopExit q0)) q0 ptpsloop qe p pmap
                else onetr q0 qe ((inverse p', inverse p), Receive, (loopExit q0))
              (body, qeBody) =
                let
                  (b,qb) =
                    proj g q0' ptpsloop q0 p pmap
                in
                  (replaceStates (danglingBrk b) qe b, qb)
              cfsm =
                cfsmUnion q0 [loop, body, exit]
            in
              (cfsm, qe)


-- Representing GCs in DOT format, a pair made of a list of nodes and
-- a list of edges
type PD = ([(DotNode, DotString)], [(DotNode, DotNode)])

node2dot :: DotNode -> DotString
node2dot n =
-- DOT representation of GC nodes
  (if n < 0 then "_" else "") ++ (show $ abs n)
      
gc2dot :: GC -> String -> Map String String -> DotString
gc2dot gc name flines =
{- 
   transform a GC in dot format giving it name 'name'
   and setting the size of nodes to 'nodeSize'
-}
  let
    nodeSize = flines!gcsizenode
    maxIdx vs = aux vs 0
      where aux [] v = v + 1
            aux ((v', _):vs') v = aux vs' (max v v')
    dummyGC n = ([(n, branchV), (-n, mergeV)], [(n, -n)])
    helper vs as gc_  =
      let
        (sink, i) = (L.last vs, 1 + (maxIdx vs))
        attach idx idx' =
          [(s, t)   | (s, t) <- as, t /= fst sink] ++
          [(s, idx) | (s, t) <- as, t == fst sink] ++
          [(idx', fst sink)]
        notgate = \v -> v /= i && v /= (-i)
      in
        case gc_ of
          Emp ->
            if flines!"-e" == ""
            then (vs, as)
            else ((L.init vs) ++ [(i, dotLabelOf gc_ )] ++ [sink], attach i i)
          ------------------------------------------------------------------
          Brk -> -----------------------------------------------------------
            error "foobar: gc2dot line 449" ------------- fix this ---------
          ------------------------------------------------------------------
          Act _ _ ->
            ((L.init vs) ++ [(i, dotLabelOf gc_ )] ++ [sink], attach i i)
          Par gcs ->
            ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
            where
              (vs', as') = unionsPD forkV joinV notgate i (rename (\v -> not (notgate v)) i graphs)
              graphs     = (L.map (helper [(i,forkV),(-i,joinV)] [(i,-i)]) gcs)
          Bra _ gcs  -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
            where
              (vs', as') = unionsPD branchV mergeV notgate i (rename (\v -> not (notgate v)) i graphs)
              (evs, eas) = dummyGC i
              -- graphs     = S.toList (S.map (helper evs eas) gcs)
              graphs     = M.elems (M.map (helper evs eas) gcs)
          Seq gcs -> graphy gcs vs as
            where
              graphy gcs_ vs_ as_ =
                case gcs_ of
                  []       -> (vs_,as_)
                  ------------------------------------------------------------------
                  Brk:_ -> error "foobar: gc2dot line 470" --------- fix this ------
                  ------------------------------------------------------------------
                  Emp:gcs' -> graphy gcs' vs_ as_
                  gc':gcs' -> graphy gcs' vs'' as''
                    where
                      (vs0,as0)   = helper [(idx,""),(-idx,"")] [(idx,-idx)] gc'
                      (vs',as')   = renameVertex notgate (vs0,as0) (1 + maxIdx vs0)
                      (vs'',as'') = catPD (\(_,l) -> l /= "") (vs_,as_) (vs',as')
                      idx         = 1 + maxIdx vs_
          Rep _ gc' -> ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ ((-i,i):as'))
            where
              (evs, eas) = dummyGC i
              (vs', as') = helper evs eas gc'
        where
          rename excluded offset pds =
            case pds of
              (vs1, as1):pds' -> ([(newNode excluded v offset,l) | (v,l) <- vs1],
                                  [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as1]) :
                                 (rename excluded (1 + offset + maxIdx vs1) pds')
              []             -> []
          unionsPD gl gl' included idx pds =
            ([(idx,gl)] ++ [(v,l) | (v,l) <- L.concat  $ L.map fst pds, (included v)] ++ [((-idx),gl')],
             L.concatMap snd pds)
    dotnodes vs =
      L.concatMap (\(s, l) -> "\tnode" ++ (node2dot s) ++ l) vs
    dotedges as =
      L.concatMap (\(s, t) -> "\tnode" ++ (node2dot s) ++ " -> node" ++ (node2dot t) ++ "\n") as
    (evs_, eas_) =
      ([(1, sourceV), (-1, sinkV)], [(1, -1)])
    (vertexes, edges) =
      helper evs_ eas_ gc
    header =
      "digraph " ++ name ++
      " {\n   node [width=" ++ nodeSize ++
      ", height=" ++ nodeSize ++ "]\n\n"
  in
    header ++ (dotnodes vertexes) ++ (dotedges edges) ++ "\n}\n"

dotLabelOf :: GC -> DotString
dotLabelOf gc = case gc of
              Emp ->
                emptyI
              Brk ->
                breakV
              Act (s,r) m ->
                " [label = \"" ++ s ++
                " &rarr; " ++ r ++
                " : " ++ m ++
                "\", shape=rectangle,\
                \ fontname=helvetica,\
                \ fontcolor=MidnightBlue]\n"
              Par _
                -> forkV
              Bra _ _  ->
                branchV
              Seq _ ->
                ""
              Rep _ _ ->
                ""

catPD :: ((DotNode, DotString) -> Bool) -> PD -> PD -> PD
catPD included (vs, as) (vs', as') = (vs'', as'')
{-
   catPD (vs,as) (vs',as') appends (vs', as') attaching its
   source to the nodes of (vs,as) entering the sink of (vs,as)
  
   PRE : vs and vs' start and end with the source and target vertex of
         the corresponding graph
  
   POST: the result is the sequential composition of the graphs
         embedding the second just before the sink of (vs,as); the
         source of the resulting graph is the source of (vs,as)
-}  
  where vs'' = (L.init vs) ++ (L.filter included vs') ++ [L.last vs]        
        as'' = [ (n, m) | (n, m) <- as, not((n, m) € maxs) ] ++
               [ (n, m) | (n, _) <- maxs, (_, m) <- min_   ] ++
               [ (n, m) | (n, m) <- as', not((n, m) € (min_ ++ max_)) ] ++
               [ (n, fst $ L.last vs) | (n, _) <- max_  ]
        maxs = maxR as
        min_ = minR as'
        max_ = maxR as'

renameVertex :: (DotNode -> Bool) -> PD -> DotNode -> PD
renameVertex excluded (vs, as) offset =
  ([(newNode excluded s offset, l) | (s,l) <- vs],
   [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as]
  )


gc2txt :: Int -> GC -> String
gc2txt level gc =
{-
   PRE: 
   POST: pretty printing of g-choreographies
-}
  let
    indent = "  "
    tab l s = "\n" ++ (concat $ replicate l indent) ++ s
  in
    case gc of
      Emp ->
        tab level "(o)"
      Brk ->
        tab level "break" 
      Act (s,r) m ->
        tab level (s ++ " -> " ++ r ++ ": " ++ m)
      Par gcs ->
        (tab level ("{")) ++
        intercalate (tab level "|") (L.map (gc2txt (level + 1)) gcs) ++
        (tab level "}")
      Bra p gcs ->
        (tab level ("sel " ++ p ++ " {")) ++
        intercalate (tab (level+1) "+") (L.map (gc2txt (level+1)) (M.elems gcs)) ++
        (tab level "}")
      Seq gcs ->
        intercalate (tab level ";") (L.map (gc2txt level) gcs)
      Rep p g ->
        (tab level ("repeat {")) ++
        p ++ (gc2txt (level + 1) g) ++
        (tab level "}")

--
-- Stuff to generate global graphs in the gml format
--
data GMLTAGS = Source | Sink | Fork | Branch | Join | Merge | Loop
  deriving (Eq, Ord, Show)

gmlstyle :: GMLTAGS -> String
-- using the yed style
gmlstyle tag = ""

gmldata :: String -> String -> String
gmldata k v = "      <data key=\"" ++ k ++ "\">" ++ v ++ "</data>\n"

gmlLabelOf :: GC -> String
gmlLabelOf gc = case gc of
              Act (s,r) m -> (gmldata "sender" s ++ gmldata "receiver" r ++ gmldata "payload" m)
              _       -> ""

gmlNode :: String -> Int -> String
gmlNode d idn  = "    <node id=\"" ++ (show idn) ++ "\">\n" ++ d ++ "    </node>\n"

gmlEdge :: Int -> Int -> String
gmlEdge source target = "    <edge source=\"" ++ (show source) ++ "\" target=\"" ++ (show target) ++ "\"></edge>\n"

gmlOpenGate :: Int -> GMLTAGS -> String
gmlOpenGate idn gate = gmlNode (gmldata "open" (show gate)) idn

gmlCloseGate :: Int -> GMLTAGS -> String
gmlCloseGate idn gate = gmlNode (gmldata "close" (show gate)) (-idn)

gmlrename :: (Int -> Bool) -> Int -> Int -> String -> String
gmlrename excluded n j s =
  if (excluded n)
  then s
  else
    let idn = (if n > 0 then n + j else n - j)
    in replace ("<node id=\"" ++ (show n)) ("<node id=\"" ++ (show idn)) s

gc2graphml :: GC -> String
gc2graphml gc =
--
-- gc2graphml gc transforms a GC in graphml format
--
  let
    header = --"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:java=\"http://www.yworks.com/xml/yfiles-common/1.0/java\" xmlns:sys=\"http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0\" xmlns:x=\"http://www.yworks.com/xml/yfiles-common/markup/2.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">\n"
      "<?xml version=\"1.0\"encoding=\"utf-8\"? standalone=\"no\">\n\
      \<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\
      \ xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\
      \ xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\
      \ http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
    ogate =
      "  <key attr.name=\"open\" attr.type=\"string\" for=\"node\" id=\"open\" />\n"
    cgate =
      "  <key attr.name=\"close\" attr.type=\"string\" for=\"node\" id=\"close\" />\n"
    sender =
      "  <key attr.name=\"sender\" attr.type=\"string\" for=\"node\" id=\"sender\" />\n"
    receiver =
      "  <key attr.name=\"receive`<r\" attr.type=\"string\" for=\"node\" id=\"receiver\" />\n"
    payload =
      "  <key attr.name=\"payload\" attr.type=\"string\" for=\"node\" id=\"payload\" />\n"
-- TODO: check that cc works as before commenting the next 3 lines
--      source   = ""--  <key attr.name=\"source\" attr.type=\"string\" for=\"node\" id=\"source\" />\n"
--      sink     = ""--"  <key attr.name=\"sink\" attr.type=\"string\" for=\"node\" id=\"sink\" />\n"
--      yattr    = ""--  <key for=\"node\" id=\"ylabel\" yfiles.type=\"nodegraphics\"/>"
    edir =
      "  <graph edgedefault=\"directed\">\n"
    footer =
      "  </graph>\n</graphml>\n"
    maxIdx vs =
      aux vs 0
      where
        aux [] v = v + 1
        aux ((v', _):vs') v = aux vs' (max v v')
    dummyGC n =
      ([(n, gmlOpenGate n Loop),
        (-n, gmlCloseGate n Loop)], [(n, -n)]
      )
    helper vs as gc_  =
      let
        (sink, i) =
          (L.last vs, 1 + (maxIdx vs))
        attach idx idx' =
          [(s, t)   | (s, t) <- as, t /= fst sink] ++
          [(s, idx) | (s, t) <- as, t == fst sink] ++
          [(idx', fst sink)]
        notgate =
          \v -> v /= i && v /= (-i)
      in
        case gc_ of
          Emp ->
            (vs, as)
          --------------------------------------------------------------
          Brk -> error "foobar: gc2graphml line 683" ----- fix this ----
          --------------------------------------------------------------
          Act _ _ ->
            ((L.init vs) ++
               [(i, gmlNode (gmlLabelOf gc_) i)] ++
               [sink],
             attach i i
            )
          Par gcs ->
            let
              (vs', as') =
                gather (gmlOpenGate i Fork)
                (gmlCloseGate i Join)
                notgate
                i
                (rename (not.notgate) i graphs)
              graphs =
                (L.map (helper [(i, (gmlOpenGate i Fork)), ((-i), (gmlCloseGate i Join))] [(i, (-i))]) gcs)
            in
              ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
          Bra _ gcs ->
            let
              gmlOpen =
                gmlOpenGate i Branch
              gmlClose =
                gmlCloseGate i Merge
              (vs', as') =
                gather gmlOpen gmlClose notgate i (rename (not.notgate) i graphs)
                --graphs = S.toList (S.map (helper [(i, (gmlOpenGate i Branch)), ((-i), (gmlCloseGate i Merge))] [(i, (-i))]) gcs)
              graphs =
                M.elems $
                  M.map (helper [(i, gmlOpen), ((-i), gmlClose)] [(i, (-i))]) gcs
            in
              ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ as')
          Seq gcs ->
            let
              graphy gcs_ vs_ as_ =
                case gcs_ of
                  [] -> (vs_,as_)
                  Emp:gcs' -> graphy gcs' vs_ as_
                  ---------------------------------------------------------------------
                  Brk:_ -> error "foobar: gc2graphml line 723 " ----- fix this --------
                  ---------------------------------------------------------------------
                  gc':gcs' ->
                    let
                      (vs0,as0) =
                        helper [(idx,""),(-idx,"")] [(idx,-idx)] gc'
                      (vs',as') =
                        renameVertex notgate (vs0,as0) (1 + maxIdx vs0)
                      (vs'',as'') =
                        catPD (\(_,l) -> l /= "") (vs_, as_) (vs', as')
                      idx = 1 + maxIdx vs_
                    in
                      graphy gcs' vs'' as''
            in
              graphy gcs vs as
          Rep _ gc' ->
            let
              (evs, eas) = dummyGC i
              (vs', as') = helper evs eas gc'
            in
              ((L.init vs) ++ vs' ++ [sink], (attach i (-i)) ++ ((-i,i):as'))
          where
            rename excluded offset pds =
              case pds of
                [] -> []
                (vs1, as1):pds' ->
                  ([(newNode excluded v offset, gmlrename excluded v offset l) | (v,l) <- vs1],
                   [(newNode excluded s offset, newNode excluded t offset) | (s,t) <- as1]
                  ) : (rename excluded (1 + offset + maxIdx vs1) pds')
            gather gl gl' included idx pds =
              ([(idx,gl)] ++
                 [(v,l) | (v,l) <- L.concat  $ L.map fst pds, (included v)] ++
                 [((-idx),gl')],
               L.concatMap snd pds
              )
    gmlNodes vs =
      L.concatMap (\(_, s) -> s) vs
    gmlEdges as =
      L.concatMap (\(s, t) -> gmlEdge s t) as
    (evs_, eas_) =
      ([(1, (gmlOpenGate 1 Source)), (-1, gmlCloseGate 1 Sink)], [(1, -1)])
    (vertexes, edges) =
      helper evs_ eas_ gc
  in header ++
     ogate ++
     cgate ++
     sender ++
     receiver ++
     payload ++
--     source ++
--     sink ++
--     yattr ++
     edir ++
     (gmlNodes vertexes) ++
     (gmlEdges edges) ++
     footer
