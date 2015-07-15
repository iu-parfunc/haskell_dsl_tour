{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L23RemoveComplexOpera where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Tail
  = IfT Pred Tail Tail
  | BeginT [Effect] Tail
  | TrivT Triv
  | AllocT Triv
  | MrefT Triv Triv
  | AppT1 Binop Triv Triv
  | AppT2 Triv [Triv]
data Pred
  = TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP Relop Triv Triv
data Effect
  = Nop
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
  | Set UVar Value
  | Mset Triv Triv Triv
  | AppE Triv [Triv]
data Value
  = IfV Pred Value Value
  | BeginV [Effect] Value
  | TrivV Triv
  | AllocV Triv
  | MrefV Triv Triv
  | AppV1 Binop Triv Triv
  | AppV2 Triv [Triv]
data Triv
  = UVar UVar
  | Integer Integer
  | Label Label
data Prog
  = Letrec [(Label,[UVar],Body)] Body
data Body
  = Locals [UVar] Tail

instance PP Tail where
  pp (IfT p t t2) = (ppSexp [fromByteString "if",(pp p),(pp t),(pp t2)])
  pp (BeginT l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (TrivT t) = (pp t)
  pp (AllocT t) = (ppSexp [fromByteString "alloc",(pp t)])
  pp (MrefT t t2) = (ppSexp [fromByteString "mref",(pp t),(pp t2)])
  pp (AppT1 b t t2) = (ppSexp [(pp b),(pp t),(pp t2)])
  pp (AppT2 t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (TrivT t) = (ppp t)
  ppp (AllocT t) = (pppSexp [text "alloc",(ppp t)])
  ppp (MrefT t t2) = (pppSexp [text "mref",(ppp t),(ppp t2)])
  ppp (AppT1 b t t2) = (pppSexp [(ppp b),(ppp t),(ppp t2)])
  ppp (AppT2 t l) = (pppSexp ((ppp t) : (map ppp l)))
instance PP Pred where
  pp (TrueP) = (ppSexp [fromByteString "true"])
  pp (FalseP) = (ppSexp [fromByteString "false"])
  pp (IfP p p2 p3) = (ppSexp [fromByteString "if",(pp p),(pp p2),(pp p3)])
  pp (BeginP l p) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp p)])))
  pp (AppP r t t2) = (ppSexp [(pp r),(pp t),(pp t2)])
  ppp (TrueP) = (pppSexp [text "true"])
  ppp (FalseP) = (pppSexp [text "false"])
  ppp (IfP p p2 p3) = (pppSexp [text "if",(ppp p),(ppp p2),(ppp p3)])
  ppp (BeginP l p) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp p)])))
  ppp (AppP r t t2) = (pppSexp [(ppp r),(ppp t),(ppp t2)])
instance PP Effect where
  pp (Nop) = (ppSexp [fromByteString "nop"])
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (Set u v) = (ppSexp [fromByteString "set!",(pp u),(pp v)])
  pp (Mset t t2 t3) = (ppSexp [fromByteString "mset!",(pp t),(pp t2),(pp t3)])
  pp (AppE t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (Set u v) = (pppSexp [text "set!",(ppp u),(ppp v)])
  ppp (Mset t t2 t3) = (pppSexp [text "mset!",(ppp t),(ppp t2),(ppp t3)])
  ppp (AppE t l) = (pppSexp ((ppp t) : (map ppp l)))
instance PP Value where
  pp (IfV p v v2) = (ppSexp [fromByteString "if",(pp p),(pp v),(pp v2)])
  pp (BeginV l v) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp v)])))
  pp (TrivV t) = (pp t)
  pp (AllocV t) = (ppSexp [fromByteString "alloc",(pp t)])
  pp (MrefV t t2) = (ppSexp [fromByteString "mref",(pp t),(pp t2)])
  pp (AppV1 b t t2) = (ppSexp [(pp b),(pp t),(pp t2)])
  pp (AppV2 t l) = (ppSexp ((pp t) : (map pp l)))
  ppp (IfV p v v2) = (pppSexp [text "if",(ppp p),(ppp v),(ppp v2)])
  ppp (BeginV l v) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp v)])))
  ppp (TrivV t) = (ppp t)
  ppp (AllocV t) = (pppSexp [text "alloc",(ppp t)])
  ppp (MrefV t t2) = (pppSexp [text "mref",(ppp t),(ppp t2)])
  ppp (AppV1 b t t2) = (pppSexp [(ppp b),(ppp t),(ppp t2)])
  ppp (AppV2 t l) = (pppSexp ((ppp t) : (map ppp l)))
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

