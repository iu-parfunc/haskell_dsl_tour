{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L20UncoverLocals where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Tail
  = LetT [(UVar,Value)] Tail
  | IfT Pred Tail Tail
  | BeginT [Effect] Tail
  | AllocT Value
  | MrefT Value Value
  | AppT1 Binop Value Value
  | AppT2 Value [Value]
  | TrivT Triv
data Pred
  = LetP [(UVar,Value)] Pred
  | TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP Relop Value Value
data Effect
  = LetE [(UVar,Value)] Effect
  | Nop
  | Mset Value Value Value
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
  | AppE Value [Value]
data Value
  = LetV [(UVar,Value)] Value
  | IfV Pred Value Value
  | BeginV [Effect] Value
  | AllocV Value
  | MrefV Value Value
  | AppV1 Binop Value Value
  | AppV2 Value [Value]
  | TrivV Triv
data Triv
  = UVar UVar
  | Integer Integer
  | Label Label
data Prog
  = Letrec [(Label,[UVar],Body)] Body
data Body
  = Locals [UVar] Tail

instance PP Tail where
  pp (LetT l t) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp t)])
  pp (IfT p t t2) = (ppSexp [fromByteString "if",(pp p),(pp t),(pp t2)])
  pp (BeginT l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (AllocT v) = (ppSexp [fromByteString "alloc",(pp v)])
  pp (MrefT v v2) = (ppSexp [fromByteString "mref",(pp v),(pp v2)])
  pp (AppT1 b v v2) = (ppSexp [(pp b),(pp v),(pp v2)])
  pp (AppT2 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (TrivT t) = (pp t)
  ppp (LetT l t) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp t)])
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (AllocT v) = (pppSexp [text "alloc",(ppp v)])
  ppp (MrefT v v2) = (pppSexp [text "mref",(ppp v),(ppp v2)])
  ppp (AppT1 b v v2) = (pppSexp [(ppp b),(ppp v),(ppp v2)])
  ppp (AppT2 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (TrivT t) = (ppp t)
instance PP Pred where
  pp (LetP l p) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp p)])
  pp (TrueP) = (ppSexp [fromByteString "true"])
  pp (FalseP) = (ppSexp [fromByteString "false"])
  pp (IfP p p2 p3) = (ppSexp [fromByteString "if",(pp p),(pp p2),(pp p3)])
  pp (BeginP l p) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp p)])))
  pp (AppP r v v2) = (ppSexp [(pp r),(pp v),(pp v2)])
  ppp (LetP l p) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp p)])
  ppp (TrueP) = (pppSexp [text "true"])
  ppp (FalseP) = (pppSexp [text "false"])
  ppp (IfP p p2 p3) = (pppSexp [text "if",(ppp p),(ppp p2),(ppp p3)])
  ppp (BeginP l p) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp p)])))
  ppp (AppP r v v2) = (pppSexp [(ppp r),(ppp v),(ppp v2)])
instance PP Effect where
  pp (LetE l e) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp e)])
  pp (Nop) = (ppSexp [fromByteString "nop"])
  pp (Mset v v2 v3) = (ppSexp [fromByteString "mset!",(pp v),(pp v2),(pp v3)])
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (AppE v l) = (ppSexp ((pp v) : (map pp l)))
  ppp (LetE l e) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp e)])
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (Mset v v2 v3) = (pppSexp [text "mset!",(ppp v),(ppp v2),(ppp v3)])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (AppE v l) = (pppSexp ((ppp v) : (map ppp l)))
instance PP Value where
  pp (LetV l v) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp v)])
  pp (IfV p v v2) = (ppSexp [fromByteString "if",(pp p),(pp v),(pp v2)])
  pp (BeginV l v) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp v)])))
  pp (AllocV v) = (ppSexp [fromByteString "alloc",(pp v)])
  pp (MrefV v v2) = (ppSexp [fromByteString "mref",(pp v),(pp v2)])
  pp (AppV1 b v v2) = (ppSexp [(pp b),(pp v),(pp v2)])
  pp (AppV2 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (TrivV t) = (pp t)
  ppp (LetV l v) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp v)])
  ppp (IfV p v v2) = (pppSexp [text "if",(ppp p),(ppp v),(ppp v2)])
  ppp (BeginV l v) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp v)])))
  ppp (AllocV v) = (pppSexp [text "alloc",(ppp v)])
  ppp (MrefV v v2) = (pppSexp [text "mref",(ppp v),(ppp v2)])
  ppp (AppV1 b v v2) = (pppSexp [(ppp b),(ppp v),(ppp v2)])
  ppp (AppV2 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (TrivV t) = (ppp t)
instance PP Triv where
  pp (UVar u) = (pp u)
  pp (Integer i) = (pp i)
  pp (Label l) = (pp l)
  ppp (UVar u) = (ppp u)
  ppp (Integer i) = (ppp i)
  ppp (Label l) = (ppp l)
instance PP Prog where
  pp (Letrec l b) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,l2,b) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp (map pp l2)),(pp b)])])) l)),(pp b)])
  ppp (Letrec l b) = (pppSexp [text "letrec",(pppSexp (map (\(l,l2,b) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp (map ppp l2)),(ppp b)])])) l)),(ppp b)])
instance PP Body where
  pp (Locals l t) = (ppSexp [fromByteString "locals",(ppSexp (map pp l)),(pp t)])
  ppp (Locals l t) = (pppSexp [text "locals",(pppSexp (map ppp l)),(ppp t)])

deriving instance Eq Tail
deriving instance Read Tail
deriving instance Show Tail
deriving instance Ord Tail
deriving instance Eq Pred
deriving instance Read Pred
deriving instance Show Pred
deriving instance Ord Pred
deriving instance Eq Effect
deriving instance Read Effect
deriving instance Show Effect
deriving instance Ord Effect
deriving instance Eq Value
deriving instance Read Value
deriving instance Show Value
deriving instance Ord Value
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv
deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Body
deriving instance Read Body
deriving instance Show Body
deriving instance Ord Body

