{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L03UncoverAssigned where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Expr Expr
data Expr
  = Let [(UVar,Expr)] Body
  | Letrec [(UVar,Expr)] Body
  | Lambda [UVar] Body
  | If Expr Expr Expr
  | Begin [Expr] Expr
  | Set UVar Expr
  | App1 ValPrim [Expr]
  | App2 EffectPrim [Expr]
  | App3 PredPrim [Expr]
  | App4 Expr [Expr]
  | UVar UVar
  | Quote Immediate
data Body
  = Assigned [UVar] Expr

instance PP Prog where
  pp (Expr e) = (pp e)
  ppp (Expr e) = (ppp e)
instance PP Expr where
  pp (Let l b) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,e) -> (ppSexp [(pp u),(pp e)])) l)),(pp b)])
  pp (Letrec l b) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(u,e) -> (ppSexp [(pp u),(pp e)])) l)),(pp b)])
  pp (Lambda l b) = (ppSexp [fromByteString "lambda",(ppSexp (map pp l)),(pp b)])
  pp (If e e2 e3) = (ppSexp [fromByteString "if",(pp e),(pp e2),(pp e3)])
  pp (Begin l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (Set u e) = (ppSexp [fromByteString "set!",(pp u),(pp e)])
  pp (App1 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (App2 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (App3 p l) = (ppSexp ((pp p) : (map pp l)))
  pp (App4 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (UVar u) = (pp u)
  pp (Quote i) = (ppSexp [fromByteString "quote",(pp i)])
  ppp (Let l b) = (pppSexp [text "let",(pppSexp (map (\(u,e) -> (pppSexp [(ppp u),(ppp e)])) l)),(ppp b)])
  ppp (Letrec l b) = (pppSexp [text "letrec",(pppSexp (map (\(u,e) -> (pppSexp [(ppp u),(ppp e)])) l)),(ppp b)])
  ppp (Lambda l b) = (pppSexp [text "lambda",(pppSexp (map ppp l)),(ppp b)])
  ppp (If e e2 e3) = (pppSexp [text "if",(ppp e),(ppp e2),(ppp e3)])
  ppp (Begin l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (Set u e) = (pppSexp [text "set!",(ppp u),(ppp e)])
  ppp (App1 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (App2 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (App3 p l) = (pppSexp ((ppp p) : (map ppp l)))
  ppp (App4 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (UVar u) = (ppp u)
  ppp (Quote i) = (pppSexp [text "quote",(ppp i)])
instance PP Body where
  pp (Assigned l e) = (ppSexp [fromByteString "assigned",(ppSexp (map pp l)),(pp e)])
  ppp (Assigned l e) = (pppSexp [text "assigned",(pppSexp (map ppp l)),(ppp e)])

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Expr
deriving instance Read Expr
deriving instance Show Expr
deriving instance Ord Expr
deriving instance Eq Body
deriving instance Read Body
deriving instance Show Body
deriving instance Ord Body

