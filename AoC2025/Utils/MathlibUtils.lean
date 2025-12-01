/-! These are utilities that are useful for computation, but are only defined in Mathlib.
If Mathlib is imported, this file should not be imported. -/

/-- Auxiliary function to construct the list of all sublists of a given length. Given an
integer `n`, a list `l`, a function `f` and an auxiliary list `L`, it returns the list made of
`f` applied to all sublists of `l` of length `n`, concatenated with `L`. -/
def List.sublistsLenAux {α β} : Nat → List α → (List α → β) → List β → List β
  | 0, _, f, r => f [] :: r
  | _ + 1, [], _, r => r
  | n + 1, a :: l, f, r => sublistsLenAux (n + 1) l f (sublistsLenAux n l (f ∘ List.cons a) r)

/-- The list of all sublists of a list `l` that are of length `n`. For instance, for
`l = [0, 1, 2, 3]` and `n = 2`, one gets
`[[2, 3], [1, 3], [1, 2], [0, 3], [0, 2], [0, 1]]`. -/
def List.sublistsLen {α} (n : Nat) (l : List α) : List (List α) :=
  sublistsLenAux n l id []
