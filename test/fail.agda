{-# OPTIONS --rewriting #-}

postulate _↦_ : {A : Set} → A → A → Set
{-# BUILTIN REWRITE _↦_ #-}
infix 0 _↦_

postulate
  A : Set
  loop : A
  rew : loop ↦ loop

{-# REWRITE rew #-}
