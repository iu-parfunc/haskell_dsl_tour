{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Pretty printing
--
module Pretty where

import AST
import Sugar
import Text.PrettyPrint.Leijen


prettyExp :: forall env t. Int -> (Doc -> Doc) -> OpenExp env t -> Doc
prettyExp lvl wrap = pp
  where
    ppE :: OpenExp env s -> Doc
    ppE = prettyExp lvl parens

    pp :: OpenExp env t -> Doc
    pp (Var ix)         = char 'x' <> int (lvl - idxToInt ix - 1)
    pp (Const c)        = text (show (toElt c :: t))
    pp (Prj ix tup)     = wrap (char '#' <> int (prodIdxToInt ix) <+> ppE tup)

    pp (Let a b) =
      let x  = char 'x' <> int lvl
          a' = prettyExp lvl     id a
          b' = prettyExp (lvl+1) id b

          isLet Let{} = True
          isLet _     = False
      in
      if not (isLet a) && isLet b
         then vcat [ text "let" <+> x <+> equals <+> a' <+> text "in", b'               ]
         else vcat [ text "let" <+> x <+> equals <+> a',               text "in" <+> b' ]

    pp (Prod p) =
      let collect :: Prod (OpenExp env) p -> [Doc]
          collect EmptyProd      = []
          collect (PushProd t e) = prettyExp lvl id e : collect t
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


instance Show (OpenExp env t) where
  show = show . prettyExp 0 id

