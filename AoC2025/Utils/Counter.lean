import Batteries
open Std Batteries

/-- A specialization of `Std.HashMap` for counting frequencies, intended to be thought of as a finite multiset.

It can be thought of as an alternative to `Multiset` in Mathlib, but utilizing hashing. -/
def Counter (α) [BEq α] [Hashable α] := Std.HashMap α Nat
deriving EmptyCollection, Membership

namespace Counter

variable {α} [BEq α] [Hashable α]

/-- Creates a new empty counter, with an optional capacity. -/
def emptyWithCapacity (capacity : Nat := 8) : Counter α :=
  HashMap.emptyWithCapacity capacity

instance : EmptyCollection (Counter α) where
  emptyCollection := emptyWithCapacity

/-- The frequency of an item in a `Counter`. -/
def get (m : Counter α) (a : α) := m.getD a 0

/-- Insert one item into a `Counter`. -/
def insert (m : Counter α) (a : α) := HashMap.insert m a (m.get a + 1)

/-- Erase one item from a `Counter`, if it exists. -/
def erase (m : Counter α) (a : α) :=
  let m : Counter α := HashMap.insert m a (m.get a - 1)
  if m.get a == 0 then (HashMap.erase m a) else m

/-- The size of a counter: the sum of the multiplicities of its elements.
Note that this is different from `Std.HashMap.size`! -/
def size (m : Counter α) := m.values.sum

/-- Construct a `Counter` from a list. -/
def ofList (l : List α) := l.foldl insert ∅

/-- Turn a `Counter` into a list of its elements, in some order. -/
def toList (m : Counter α) :=
  HashMap.toList m |>.map (fun (a, n) => List.replicate n a) |>.flatten

end Counter
