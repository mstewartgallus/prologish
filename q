[1mdiff --git a/src/Cbpv.hs b/src/Cbpv.hs[m
[1mindex f031051..b194b0e 100644[m
[1m--- a/src/Cbpv.hs[m
[1m+++ b/src/Cbpv.hs[m
[36m@@ -29,9 +29,12 @@[m [mclass Cbpv (k :: forall x y. x -> y -> Type) where[m
   lambda :: k (env * a) b -> k env (a ~> b)[m
   unlambda :: k env (a ~> b) -> k (env * a) b[m
 [m
[31m-  force :: k (U a) a[m
[31m-  thunk :: k a (U a)[m
[31m-  returns :: k a (F a)[m
[32m+[m[32m  force :: k a (U b) -> k a b[m
[32m+[m[32m  thunk :: k a b -> k a (U b)[m
[32m+[m
[32m+[m[32m  returns :: k a b -> k a (F a)[m
[32m+[m
[32m+[m[32m  letTo :: k env (F a) -> k a b -> k env b[m
 [m
   u64 :: Word64 -> k x U64[m
   add :: k x (U64 ~> U64 ~> F U64)[m
[1mdiff --git a/src/Sort.hs b/src/Sort.hs[m
[1mindex c6ba2ee..63257f0 100644[m
[1m--- a/src/Sort.hs[m
[1m+++ b/src/Sort.hs[m
[36m@@ -20,6 +20,7 @@[m [mmodule Sort[m
     F,[m
     Void,[m
     type (~>),[m
[32m+[m[32m    type (&),[m
     SAlgebra (..),[m
     KnownAlgebra,[m
     inferAlgebra,[m
[36m@@ -38,8 +39,12 @@[m [mtype Unit = 'Unit[m
 [m
 type (*) = 'Product[m
 [m
[32m+[m[32minfixl 0 *[m
[32m+[m
 type (+) = 'Sum[m
 [m
[32m+[m[32minfixl 0 +[m
[32m+[m
 type U64 = 'U64[m
 [m
 type Algebra = AlgebraImpl[m
[36m@@ -50,15 +55,15 @@[m [mtype Void = 'Void[m
 [m
 type (~>) = 'Exp[m
 [m
[31m-data SetImpl = U Algebra | Unit | Sum Set Set | Product Set Set | U64[m
[32m+[m[32minfixr 9 ~>[m
 [m
[31m-data AlgebraImpl = F Set | Void | Exp Set Algebra[m
[32m+[m[32mtype (&) = 'Asym[m
 [m
[31m-infixr 9 ~>[m
[32m+[m[32minfixl 0 &[m
 [m
[31m-infixl 0 *[m
[32m+[m[32mdata SetImpl = U Algebra | Unit | Sum Set Set | Product Set Set | U64[m
 [m
[31m-infixl 0 +[m
[32m+[m[32mdata AlgebraImpl = F Set | Void | Exp Set Algebra | Asym Set Algebra[m
 [m
 data SSet a where[m
   SU :: SAlgebra a -> SSet (U a)[m
[36m@@ -71,6 +76,7 @@[m [mdata SAlgebra a where[m
   SF :: SSet a -> SAlgebra (F a)[m
   SVoid :: SAlgebra Void[m
   (:->) :: SSet a -> SAlgebra b -> SAlgebra (a ~> b)[m
[32m+[m[32m  (:&) :: SSet a -> SAlgebra b -> SAlgebra (a & b)[m
 [m
 class KnownSet t where[m
   inferSet :: SSet t[m
[36m@@ -102,6 +108,9 @@[m [minstance KnownAlgebra 'Void where[m
 instance (KnownSet a, KnownAlgebra b) => KnownAlgebra ('Exp a b) where[m
   inferAlgebra = inferSet :-> inferAlgebra[m
 [m
[32m+[m[32minstance (KnownSet a, KnownAlgebra b) => KnownAlgebra ('Asym a b) where[m
[32m+[m[32m  inferAlgebra = inferSet :& inferAlgebra[m
[32m+[m
 eqAlgebra :: SAlgebra a -> SAlgebra b -> Maybe (a :~: b)[m
 eqAlgebra x y = case (x, y) of[m
   (SF x, SF x') -> case eqSet x x' of[m
[36m@@ -111,6 +120,9 @@[m [meqAlgebra x y = case (x, y) of[m
   (a :-> b, a' :-> b') -> case (eqSet a a', eqAlgebra b b') of[m
     (Just Refl, Just Refl) -> Just Refl[m
     _ -> Nothing[m
[32m+[m[32m  (a :& b, a' :& b') -> case (eqSet a a', eqAlgebra b b') of[m
[32m+[m[32m    (Just Refl, Just Refl) -> Just Refl[m
[32m+[m[32m    _ -> Nothing[m
   _ -> Nothing[m
 [m
 eqSet :: SSet a -> SSet b -> Maybe (a :~: b)[m
[36m@@ -133,11 +145,12 @@[m [minstance Show (SSet a) where[m
     SU x -> "(U " ++ show x ++ ")"[m
     SUnit -> "Unit"[m
     SU64 -> "U64"[m
[31m-    x :*: y -> "(" ++ show x ++ " * " ++ show y ++ ")"[m
[32m+[m[32m    x :*: y -> "(" ++ show x ++ " × " ++ show y ++ ")"[m
     x :+: y -> "(" ++ show x ++ " + " ++ show y ++ ")"[m
 [m
 instance Show (SAlgebra a) where[m
   show expr = case expr of[m
     SF x -> "(F " ++ show x ++ ")"[m
     SVoid -> "Void"[m
[31m-    x :-> y -> "(" ++ show x ++ " ~> " ++ show y ++ ")"[m
[32m+[m[32m    x :-> y -> "(" ++ show x ++ " → " ++ show y ++ ")"[m
[32m+[m[32m    x :-> y -> "(" ++ show x ++ " ⊗ " ++ show y ++ ")"[m
