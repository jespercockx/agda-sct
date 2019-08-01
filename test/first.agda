{-# OPTIONS --rewriting #-}

postulate _↦_ : {A : Set} → A → A → Set
{-# BUILTIN REWRITE _↦_ #-}
infix 0 _↦_

postulate
  Nat  : Set
  zero : Nat
  suc  : Nat → Nat
  _+_  : Nat → Nat → Nat

variable
  x y z : Nat

postulate
  zero+  : zero + x ↦ x
  suc+   : (suc x) + y ↦ suc (x + y)
  +zero  : x + zero ↦ x
  +suc   : x + (suc y) ↦ suc (x + y)
  assoc+ : (x + y) + z ↦ x + (y + z)

{-# REWRITE zero+ suc+ +zero +suc assoc+ #-}

postulate
  _*_    : Nat → Nat → Nat
  zero*  : zero * x ↦ zero
  suc*   : (suc x) * y ↦ (x * y) + y
  *zero  : x * zero ↦ zero
  *suc   : x * (suc y) ↦ x + (x * y)
  assoc* : (x * y) * z ↦ x * (y * z)
  distrl : x * (y + z) ↦ (x * y) + (x * z)
  distrr : (x + y) * z ↦ (x * z) + (y * z)

{-# REWRITE zero* suc* *zero *suc assoc* distrl distrr #-}
