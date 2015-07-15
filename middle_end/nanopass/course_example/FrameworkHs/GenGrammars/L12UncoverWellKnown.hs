{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L12UncoverWellKnown where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = Expr Expr
data Expr
  = If Expr Expr Expr
  | Begin [Expr] Expr
  | App1 ValPrim [Expr]
  | App2 EffectPrim [Expr]
  | App3 PredPrim [Expr]
  | App4 Expr [Expr]
  | UVar UVar
  | Quote Immediate
  | Let [(UVar,Expr)] Expr
  | Label Label
  | Letrec [(Label,[UVar],[UVar],Expr)] [(UVar,Label,[UVar])] [UVar] Expr

instance PP Prog where
  pp (Expr e) = (pp e)
  ppp (Expr e) = (ppp e)
instance PP Expr where
  pp (If e e2 e3) = (ppSexp [fromByteString "if",(pp e),(pp e2),(pp e3)])
  pp (Begin l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (App1 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (App2 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (App3 p l) = (ppSexp ((pp p) : (map pp l)))
  pp (App4 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (UVar u) = (pp u)
  pp (Quote i) = (ppSexp [fromByteString "quote",(pp i)])
  pp (Let l e) = (ppSexp [fromByteString "let",(ppSexp (map (\(u,e) -> (ppSexp [(pp u),(pp e)])) l)),(pp e)])
  pp (Label l) = (pp l)
  pp (Letrec l l2 l3 e) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(l,l2,l3,e) -> (ppSexp [(pp l),(ppSexp [fromByteString "lambda",(ppSexp (map pp l2)),(ppSexp [fromByteString "bind-free",(ppSexp (map pp l3)),(pp e)])])])) l)),(ppSexp [fromByteString "closures",(ppSexp (map (\(u,l,l2) -> (ppSexp ((pp u) : ((pp l) : (map pp l2))))) l2)),(ppSexp [fromByteString "well-known",(ppSexp (map pp l3)),(pp e)])])])
  ppp (If e e2 e3) = (pppSexp [text "if",(ppp e),(ppp e2),(ppp e3)])
  ppp (Begin l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (App1 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (App2 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (App3 p l) = (pppSexp ((ppp p) : (map ppp l)))
  ppp (App4 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (UVar u) = (ppp u)
  ppp (Quote i) = (pppSexp [text "quote",(ppp i)])
  ppp (Let l e) = (pppSexp [text "let",(pppSexp (map (\(u,e) -> (pppSexp [(ppp u),(ppp e)])) l)),(ppp e)])
  ppp (Label l) = (ppp l)
  ppp (Letrec l l2 l3 e) = (pppSexp [text "letrec",(pppSexp (map (\(l,l2,l3,e) -> (pppSexp [(ppp l),(pppSexp [text "lambda",(pppSexp (map ppp l2)),(pppSexp [text "bind-free",(pppSexp (map ppp l3)),(ppp e)])])])) l)),(pppSexp [text "closures",(pppSexp (map (\(u,l,l2) -> (pppSexp ((ppp u) : ((ppp l) : (map ppp l2))))) l2)),(pppSexp [text "well-known",(pppSexp (map ppp l3)),(ppp e)])])])

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Expr
deriving instance Read Expr
deriving instance Show Expr
deriving instance Ord Expr

