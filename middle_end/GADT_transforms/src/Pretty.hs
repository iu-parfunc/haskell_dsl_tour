{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Pretty printing
--
module Pretty where

import AST
import Array.Sugar
import Text.PrettyPrint.Leijen

prettyAcc
    :: forall aenv a.
       Int
    -> (Doc -> Doc)
    -> OpenAcc aenv a
    -> Doc
prettyAcc alvl wrap = pp
  where
    name .$ docs = wrap $ hang 2 (sep (text name : docs))

    ppE :: OpenExp env aenv t -> Doc
    ppE = prettyExp 0 alvl parens

    ppF :: OpenFun env aenv f -> Doc
    ppF = parens . prettyFun 0 alvl

    ppA :: OpenAcc aenv arrs -> Doc
    ppA = prettyAcc alvl parens

    pp :: OpenAcc aenv arrs -> Doc
    pp (Use arr)        = wrap (text (show arr))
    pp (Avar ix)        = char 'a' <> int (alvl - idxToInt ix - 1)
    pp (Alet a b)       =
      let x  = char 'a' <> int alvl
          a' = prettyAcc alvl     id a
          b' = prettyAcc (alvl+1) id a

          isLet Alet{} = True
          isLet _      = False
      in
      if not (isLet a) && isLet b
         then wrap $ vcat [ text "let" <+> x <+> equals <+> a' <+> text "in", b'               ]
         else wrap $ vcat [ text "let" <+> x <+> equals <+> a',               text "in" <+> b' ]

    pp (Map f a)        = "map"      .$ [ ppF f,  ppA a ]
    pp (Generate sh f)  = "generate" .$ [ ppE sh, ppF f ]


prettyFun
    :: forall env aenv f.
       Int
    -> Int
    -> OpenFun env aenv f
    -> Doc
prettyFun lvl alvl fun =
  let (n, bodyDoc) = count n fun
  in
  char '\\' <> hsep [text $ 'x' : show idx | idx <- [lvl..n]] <+>
  text "->" <+> bodyDoc
  where
     count :: Int -> OpenFun env' aenv' fun' -> (Int, Doc)
     count l (Body body) = (lvl-1, prettyExp (l + 1) alvl id body)
     count l (Lam  fun') = let (n, body) = count l fun' in (1 + n, body)


prettyExp
    :: forall env aenv t.
       Int
    -> Int
    -> (Doc -> Doc)
    -> OpenExp env aenv t
    -> Doc
prettyExp lvl alvl wrap = pp
  where
    ppE :: OpenExp env aenv s -> Doc
    ppE = prettyExp lvl alvl parens

    pp :: OpenExp env aenv t -> Doc
    pp (Var ix)         = char 'x' <> int (lvl - idxToInt ix - 1)
    pp (Const c)        = text (show (toElt c :: t))
    pp (Prj ix tup)     = wrap (char '#' <> int (prodIdxToInt ix) <+> ppE tup)

    pp (Let a b) =
      let x  = char 'x' <> int lvl
          a' = prettyExp lvl     alvl id a
          b' = prettyExp (lvl+1) alvl id b

          isLet Let{} = True
          isLet _     = False
      in
      if not (isLet a) && isLet b
         then vcat [ text "let" <+> x <+> equals <+> a' <+> text "in", b'               ]
         else vcat [ text "let" <+> x <+> equals <+> a',               text "in" <+> b' ]

    pp (Prod p) =
      let collect :: Prod (OpenExp env aenv) p -> [Doc]
          collect EmptyProd      = []
          collect (PushProd t e) = prettyExp lvl alvl id e : collect t
      in
      tupled (reverse (collect p))

    pp (PrimApp f x) =
      let ppF :: PrimFun f -> Doc
          ppF PrimAdd{}     = text "(+)"
          ppF PrimMul{}     = text "(*)"
          ppF PrimToFloat{} = text "toFloat"
      in
      wrap (ppF f <+> ppE x)

    pp (If p t e) =
      hang 3 $ vcat [ text "if"   <+> ppE p, text "then" <+> ppE t, text "else" <+> ppE e ]


instance Show (OpenAcc aenv a) where
  show = show . prettyAcc 0 id

instance Show (OpenFun env aenv f) where
  show = show . prettyFun 0 0

instance Show (OpenExp env aenv t) where
  show = show . prettyExp 0 0 id

