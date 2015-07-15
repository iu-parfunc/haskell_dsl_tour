

module RemoveLet where

import           FrameworkHs.GenGrammars.MicroScheme  as S
import qualified FrameworkHs.GenGrammars.NoLets       as T


removeLet :: S.Expr -> T.Expr
removeLet e0 =
  case e0 of
    (Immediate x) -> T.Immediate x
    (Quote x)     -> T.Quote x
    -- (Let ls bods) -> T.Let [(v,go r) | (v,r) <- ls]
    --                        (goBods bods)
    (Let ls bods)  -> let (vs,rhss) = unzip ls
                      in T.App4 (T.Lambda vs (goBods bods))
                                (map go rhss)

    (Lambda ls x2) -> T.Lambda ls (goBods x2)
    (If x1 x2 x3)  -> T.If (go x1) (go x2) (go x3)
    (Begin x1 x2)  -> T.Begin (map go x1) (go x2)
    (Set x1 x2)    -> T.Set x1 (go x2)
    (App1 x1 x2) -> T.App1 x1 (map go x2)
    (App2 x1 x2) -> T.App2 x1 (map go x2)
    (App3 x1 x2) -> T.App3 x1 (map go x2)
    (App4 x1 x2) -> T.App4 (go x1) (map go x2)
    (UVar x)     -> T.UVar x
 where
  goBods bods = [ (T.ExprB (go bod)) | (ExprB bod) <- bods ]
  go = removeLet
