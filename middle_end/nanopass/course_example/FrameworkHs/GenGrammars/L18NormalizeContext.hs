{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L18NormalizeContext where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Letrec [(Label,[UVar],Value)] Value
data Pred
  = LetP [(UVar,Value)] Pred
  | TrueP
  | FalseP
  | IfP Pred Pred Pred
  | BeginP [Effect] Pred
  | AppP PredPrim [Value]
data Effect
  = LetE [(UVar,Value)] Effect
  | Nop
  | IfE Pred Effect Effect
  | BeginE [Effect] Effect
  | AppE1 EffectPrim [Value]
  | AppE2 Value [Value]
data Value
  = Quote Immediate
  | LetV [(UVar,Value)] Value
  | IfV Pred Value Value
  | BeginV [Effect] Value
  | AppV1 ValPrim [Value]
  | AppV2 Value [Value]
  | UVar UVar
  | Label Label

instance PP Prog where
  pp (Letrec l v) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,l2,v) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp (map pp l2)),(pp v)])])) l)),(pp v)])
  ppp (Letrec l v) = (pppSexp [text "letrec",(pppSexp (map (\(l,l2,v) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp (map ppp l2)),(ppp v)])])) l)),(ppp v)])
instance PP Pred where
  pp (LetP l p) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp p)])
  pp (TrueP) = (ppSexp [fromByteString "true"])
  pp (FalseP) = (ppSexp [fromByteString "false"])
  pp (IfP p p2 p3) = (ppSexp [fromByteString "if",(pp p),(pp p2),(pp p3)])
  pp (BeginP l p) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp p)])))
  pp (AppP p l) = (ppSexp ((pp p) : (map pp l)))
  ppp (LetP l p) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp p)])
  ppp (TrueP) = (pppSexp [text "true"])
  ppp (FalseP) = (pppSexp [text "false"])
  ppp (IfP p p2 p3) = (pppSexp [text "if",(ppp p),(ppp p2),(ppp p3)])
  ppp (BeginP l p) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp p)])))
  ppp (AppP p l) = (pppSexp ((ppp p) : (map ppp l)))
instance PP Effect where
  pp (LetE l e) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp e)])
  pp (Nop) = (ppSexp [fromByteString "nop"])
  pp (IfE p e e2) = (ppSexp [fromByteString "if",(pp p),(pp e),(pp e2)])
  pp (BeginE l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (AppE1 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (AppE2 v l) = (ppSexp ((pp v) : (map pp l)))
  ppp (LetE l e) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp e)])
  ppp (Nop) = (pppSexp [text "nop"])
  ppp (IfE p e e2) = (pppSexp [text "if",(ppp p),(ppp e),(ppp e2)])
  ppp (BeginE l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (AppE1 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (AppE2 v l) = (pppSexp ((ppp v) : (map ppp l)))
instance PP Value where
  pp (Quote i) = (ppSexp [fromByteString "quote",(pp i)])
  pp (LetV l v) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,v) -> (ppSexp [(pp u),(pp v)])) l)),(pp v)])
  pp (IfV p v v2) = (ppSexp [fromByteString "if",(pp p),(pp v),(pp v2)])
  pp (BeginV l v) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp v)])))
  pp (AppV1 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (AppV2 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (UVar u) = (pp u)
  pp (Label l) = (pp l)
  ppp (Quote i) = (pppSexp [text "quote",(ppp i)])
  ppp (LetV l v) = (pppSexp [text "let",(pppSexp (map (\(u,v) -> (pppSexp [(ppp u),(ppp v)])) l)),(ppp v)])
  ppp (IfV p v v2) = (pppSexp [text "if",(ppp p),(ppp v),(ppp v2)])
  ppp (BeginV l v) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp v)])))
  ppp (AppV1 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (AppV2 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (UVar u) = (ppp u)
  ppp (Label l) = (ppp l)

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
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

