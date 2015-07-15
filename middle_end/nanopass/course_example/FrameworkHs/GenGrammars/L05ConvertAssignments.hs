{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L05ConvertAssignments where

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
  | Letrec [(UVar,Lamb)] Expr
data Lamb
  = Lambda [UVar] Expr

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
  pp (Letrec l e) = (ppSexp [fromByteString "letrec",(ppSexp (map (\(u,l) -> (ppSexp [(pp u),(pp l)])) l)),(pp e)])
  ppp (If e e2 e3) = (pppSexp [text "if",(ppp e),(ppp e2),(ppp e3)])
  ppp (Begin l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (App1 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (App2 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (App3 p l) = (pppSexp ((ppp p) : (map ppp l)))
  ppp (App4 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (UVar u) = (ppp u)
  ppp (Quote i) = (pppSexp [text "quote",(ppp i)])
  ppp (Let l e) = (pppSexp [text "let",(pppSexp (map (\(u,e) -> (pppSexp [(ppp u),(ppp e)])) l)),(ppp e)])
  ppp (Letrec l e) = (pppSexp [text "letrec",(pppSexp (map (\(u,l) -> (pppSexp [(ppp u),(ppp l)])) l)),(ppp e)])
instance PP Lamb where
  pp (Lambda l e) = (ppSexp [fromByteString "lambda",(ppSexp (map pp l)),(pp e)])
  ppp (Lambda l e) = (pppSexp [text "lambda",(pppSexp (map ppp l)),(ppp e)])

deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog
deriving instance Eq Expr
deriving instance Read Expr
deriving instance Show Expr
deriving instance Ord Expr
deriving instance Eq Lamb
deriving instance Read Lamb
deriving instance Show Lamb
deriving instance Ord Lamb

