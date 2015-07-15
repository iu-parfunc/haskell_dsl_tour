

(p423-grammars

 (micro-scheme
    (start Prog)
    (Prog Expr)
    (Expr
      Immediate
      (quote Datum)
      (let    ([UVar Expr] *) Body *)
      (lambda (UVar *) Body *)
      (if Expr Expr Expr)
      (begin Expr * Expr)
      (set! UVar Expr)
      (ValPrim Expr *)
      (EffectPrim Expr *)
      (PredPrim Expr *)
      (Expr Expr *)
      UVar
      )
    (Body Expr)
    )

 (no-lets
    (%remove let)
    (%add ))
)
