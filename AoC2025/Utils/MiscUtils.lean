import Std
open Std

/-- The four unit vectors, as `Int × Int`:
`List.padj4 = [(1, 0), (-1, 0), (0, 1), (0, -1)]`. -/
def List.padj4 : List (Int × Int) := [(1, 0), (-1, 0), (0, 1), (0, -1)]
/-- The eight unit vectors, as `Int × Int`:
`List.padj4 = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]`. -/
def List.padj8 : List (Int × Int) := [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]

/-- Returns the list `l` without duplicates, in some order.
Uses a HashSet for faster performance over the `List.dedup` function in Mathlib. -/
def List.fast_dedup {α} [BEq α] [Hashable α] (l : List α) : List α :=
  HashSet.ofList l |>.toList

/--
Returns successive adjacent pairs from a list.

Examples:
* `[].adj_pairs = []`
* `[1].adj_pairs = []`
* `[1, 2, 3, 4].adj_pairs = [(1, 2), (2, 3), (3, 4)]`
-/
def List.adj_pairs {α} (l : List α) : List (α × α) := match l with
| [] => []
| [_] => []
| a :: b :: as => (a, b) :: (b :: as).adj_pairs

/--
Returns all sublists of length 2 from a list as pairs.
Equivalent to Mathlib's `List.sublistsLen 2`, but with a list of pairs.

Examples:
* `[].all_pairs = []`
* `[1, 2].all_pairs = [(1, 2)]`
* `[1, 2, 3, 4].all_pairs = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]`
-/
def List.all_pairs {α} (l : List α) : List (α × α) := match l with
| [] => []
| a :: as => as.map (Prod.mk a ·) ++ as.all_pairs


/--
Returns successive adjacent pairs from an array.

Examples:
* `#[].adj_pairs = #[]`
* `#[1].adj_pairs = #[]`
* `#[1, 2, 3, 4].adj_pairs = #[(1, 2), (2, 3), (3, 4)]`
-/
def Array.adj_pairs {α} (xs : Array α) : Array (α × α) :=
  Array.finRange (xs.size - 1) |>.map (fun i => (xs[i.1], xs[i.1+1]))

/--
Returns all subsequences of length 2 from an array as pairs.

Examples:
* `#[].adj_pairs = #[]`
* `#[1].adj_pairs = #[]`
* `#[1, 2, 3, 4].adj_pairs = #[(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]`
-/
def Array.all_pairs {α} (xs : Array α) : Array (α × α) :=
  Array.finRange xs.size |>.flatMap
    (fun i => Array.finRange xs.size |>.toSubarray (i.1 + 1)
      |>.toArray.map (fun j => (i, j)))
  |>.map (fun (i, j) => (xs[i], xs[j]))

/--
Returns successive adjacent pairs from a vector.

Examples:
* `#v[].adj_pairs = #v[]`
* `#v[1].adj_pairs = #v[]`
* `#v[1, 2, 3, 4].adj_pairs = #v[(1, 2), (2, 3), (3, 4)]`
-/
def Vector.adj_pairs {α n} (xs : Vector α n) : Vector (α × α) (n - 1) :=
  Vector.finRange (n - 1) |>.map (fun i => (xs[i.1], xs[i.1+1]))
