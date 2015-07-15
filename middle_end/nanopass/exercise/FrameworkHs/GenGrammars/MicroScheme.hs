{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.MicroScheme where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Prog
  = ExprP Expr
data Expr
  = Immediate Immediate
  | Quote Datum
  | Let [(UVar,Expr)] [Body]
  | Lambda [UVar] [Body]
  | If Expr Expr Expr
  | Begin [Expr] Expr
  | Set UVar Expr
  | App1 ValPrim [Expr]
  | App2 EffectPrim [Expr]
  | App3 PredPrim [Expr]
  | App4 Expr [Expr]
  | UVar UVar
data Body
  = ExprB Expr

instance PP Prog where
  pp (ExprP e) = (pp e)
  ppp (ExprP e) = (ppp e)
instance PP Expr where
  pp (Immediate i) = (pp i)
  pp (Quote d) = (ppSexp [fromByteString "quote",(pp d)])
  pp (Let l l2) = (ppSexp (fromByteString "let" : ((ppSexp (map (\(u,e) -> (ppSexp [(pp u),(pp e)])) l)) : (map pp l2))))
  pp (Lambda l l2) = (ppSexp (fromByteString "lambda" : ((ppSexp (map pp l)) : (map pp l2))))
  pp (If e e2 e3) = (ppSexp [fromByteString "if",(pp e),(pp e2),(pp e3)])
  pp (Begin l e) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp e)])))
  pp (Set u e) = (ppSexp [fromByteString "set!",(pp u),(pp e)])
  pp (App1 v l) = (ppSexp ((pp v) : (map pp l)))
  pp (App2 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (App3 p l) = (ppSexp ((pp p) : (map pp l)))
  pp (App4 e l) = (ppSexp ((pp e) : (map pp l)))
  pp (UVar u) = (pp u)
  ppp (Immediate i) = (ppp i)
  ppp (Quote d) = (pppSexp [text "quote",(ppp d)])
  ppp (Let l l2) = (pppSexp (text "let" : ((pppSexp (map (\(u,e) -> (pppSexp [(ppp u),(ppp e)])) l)) : (map ppp l2))))
  ppp (Lambda l l2) = (pppSexp (text "lambda" : ((pppSexp (map ppp l)) : (map ppp l2))))
  ppp (If e e2 e3) = (pppSexp [text "if",(ppp e),(ppp e2),(ppp e3)])
  ppp (Begin l e) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp e)])))
  ppp (Set u e) = (pppSexp [text "set!",(ppp u),(ppp e)])
  ppp (App1 v l) = (pppSexp ((ppp v) : (map ppp l)))
  ppp (App2 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (App3 p l) = (pppSexp ((ppp p) : (map ppp l)))
  ppp (App4 e l) = (pppSexp ((ppp e) : (map ppp l)))
  ppp (UVar u) = (ppp u)
instance PP Body where
  pp (ExprB e) = (pp e)
  ppp (ExprB e) = (ppp e)

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

