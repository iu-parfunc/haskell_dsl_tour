{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L36FinalizeLocations where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Tail
  = IfT Pred Tail Tail
  | BeginT [Effect] Tail
  | AppT Triv
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
  | Mset Triv Triv Triv
  | ReturnPoint Label Tail
  | Set1 Loc Triv
  | Set2 Loc Binop Triv Triv
  | Set3 Loc Triv Triv
data Triv
  = Integer Integer
  | Label Label
  | Loc Loc
data Prog
  = Letrec [(Label,Tail)] Tail
data Loc
  = Reg Reg
  | FVar FVar

instance PP Tail where
  pp (IfT p t t2) = (ppSexp [fromByteString "if",(pp p),(pp t),(pp t2)])
  pp (BeginT l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (AppT t) = (ppSexp [(pp t)])
  ppp (IfT p t t2) = (pppSexp [text "if",(ppp p),(ppp t),(ppp t2)])
  ppp (BeginT l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (AppT t) = (pppSexp [(ppp t)])
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
  pp (Mset t t2 t3) = (ppSexp [fromByteString "mset!",(pp t),(pp t2),(pp t3)])
  pp (ReturnPoint l t) = (ppSexp [fromByteString "return-point",(pp l),(pp t)])
  pp (Set1 l t) = (ppSexp [fromByteString "set!",(pp l),(pp t)])
  pp (Set2 l b t t2) = (ppSexp [fromByteString "set!",(pp l),(ppSexp [(pp b),(pp t),(pp t2)])])
  pp (Set3 l t t2) = (ppSexp [fromByteString "set!",(pp l),(ppSexp [fromByteString "mref",(pp t),(pp t2)])])
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (Mset t t2 t3) = (pppSexp [text "mset!",(ppp t),(ppp t2),(ppp t3)])
  ppp (ReturnPoint l t) = (pppSexp [text "return-point",(ppp l),(ppp t)])
  ppp (Set1 l t) = (pppSexp [text "set!",(ppp l),(ppp t)])
  ppp (Set2 l b t t2) = (pppSexp [text "set!",(ppp l),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
  ppp (Set3 l t t2) = (pppSexp [text "set!",(ppp l),(pppSexp [text "mref",(ppp t),(ppp t2)])])
instance PP Triv where
  pp (Integer i) = (pp i)
  pp (Label l) = (pp l)
  pp (Loc l) = (pp l)
  ppp (Integer i) = (ppp i)
  ppp (Label l) = (ppp l)
  ppp (Loc l) = (ppp l)
instance PP Prog where
  pp (Letrec l t) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,t) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp []),(pp t)])])) l)),(pp t)])
  ppp (Letrec l t) = (pppSexp [text "letrec",(pppSexp (map (\(l,t) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp []),(ppp t)])])) l)),(ppp t)])
instance PP Loc where
  pp (Reg r) = (pp r)
  pp (FVar f) = (pp f)
  ppp (Reg r) = (ppp r)
  ppp (FVar f) = (ppp f)

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
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv
deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Loc
deriving instance Read Loc
deriving instance Show Loc
deriving instance Ord Loc

