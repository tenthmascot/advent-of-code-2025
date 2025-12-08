import Std
open Std

section

variable {α : Type u} [BEq α] [Hashable α]

/-- A DSU, represented as a combined parent & size map.
Uses union by size. No way to ensure safe usage.

This is a terrible way to represent a DSU,
but it's usable enough for unsound applications like this one.
It might be a cool project to refactor this and add safe options,
or to introduce path compression. -/
structure DSU (α : Type u) [BEq α] [Hashable α] where
  data : HashMap α (α × Nat)

namespace DSU

instance : Membership α (DSU α) :=
  ⟨fun dsu x => x ∈ dsu.data⟩

/-- Create a new DSU from a list. -/
def new_ofList (l : List α) : DSU α where
  data := HashMap.ofList $ l.map $ fun x => (x, (x, 1))

def parent! [Inhabited α] (dsu : DSU α) (x : α) : α :=
  dsu.data.get! x |>.1

def size! [Inhabited α] (dsu : DSU α) (x : α) : Nat :=
  dsu.data.get! x |>.2

/-- Auxillary for `DSU.find!`. -/
private def find!_aux [Inhabited α] (dsu : DSU α) (x : α) (fuel : Nat) : α :=
  match fuel with
  | 0 => panic! "DSU.find!_aux ran out of fuel"
  | fuel+1 =>
    let y := dsu.parent! x
    if x == y then x else (dsu.find!_aux y fuel)

/-- Find the set an item belongs to. -/
def find! [Inhabited α] (dsu : DSU α) (x : α) : α :=
  dsu.find!_aux x dsu.data.size

/-- Auxillary for `DSU.join!`, taking in a proof of size comparison. -/
private def join!AuxLe [Inhabited α] (dsu : DSU α) {x y : α} (_ : dsu.size! x ≤ dsu.size! y) : DSU α :=
  ⟨dsu.data.insert x (y, dsu.size! x) |>.insert y (y, dsu.size! y + dsu.size! x)⟩

/-- Join the sets containing `x` and `y`. -/
def join! [Inhabited α] (dsu : DSU α) (x y : α) : DSU α :=
  let x := dsu.find! x
  let y := dsu.find! y
  if x != y then
    if h : (dsu.size! x ≤ dsu.size! y)
      then dsu.join!AuxLe h
      else dsu.join!AuxLe (Nat.le_of_not_le h)
  else dsu

end DSU

end section
