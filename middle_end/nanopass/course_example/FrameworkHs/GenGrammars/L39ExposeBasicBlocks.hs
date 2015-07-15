{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L39ExposeBasicBlocks where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Tail
  = Begin [Effect] Tail
  | App Triv
  | If Relop Triv Triv Label Label
data Effect
  = Set1 Loc Triv
  | Set2 Loc Binop Triv Triv
data Triv
  = Integer Integer
  | Label Label
  | Loc Loc
data Prog
  = Letrec [(Label,Tail)] Tail
data Loc
  = Reg Reg
  | Disp Disp
  | Ind Ind

instance PP Tail where
  pp (Begin l t) = (ppSexp (fromByteString "begin" : ((map pp l) ++ [(pp t)])))
  pp (App t) = (ppSexp [(pp t)])
  pp (If r t t2 l l2) = (ppSexp [fromByteString "if",(ppSexp [(pp r),(pp t),(pp t2)]),(ppSexp [(pp l)]),(ppSexp [(pp l2)])])
  ppp (Begin l t) = (pppSexp (text "begin" : ((map ppp l) ++ [(ppp t)])))
  ppp (App t) = (pppSexp [(ppp t)])
  ppp (If r t t2 l l2) = (pppSexp [text "if",(pppSexp [(ppp r),(ppp t),(ppp t2)]),(pppSexp [(ppp l)]),(pppSexp [(ppp l2)])])
instance PP Effect where
  pp (Set1 l t) = (ppSexp [fromByteString "set!",(pp l),(pp t)])
  pp (Set2 l b t t2) = (ppSexp [fromByteString "set!",(pp l),(ppSexp [(pp b),(pp t),(pp t2)])])
  ppp (Set1 l t) = (pppSexp [text "set!",(ppp l),(ppp t)])
  ppp (Set2 l b t t2) = (pppSexp [text "set!",(ppp l),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
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
  pp (Disp d) = (pp d)
  pp (Ind i) = (pp i)
  ppp (Reg r) = (ppp r)
  ppp (Disp d) = (ppp d)
  ppp (Ind i) = (ppp i)

deriving instance Eq Tail
deriving instance Read Tail
deriving instance Show Tail
deriving instance Ord Tail
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

