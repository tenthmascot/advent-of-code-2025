import AoC2025.Utils
import AoC2025.AoCUtils
open Std

/-- List corresponding to the interval `[m, n]`. This is empty if `m > n`. -/
private def Int.irange_aux (m n : Int) : List Int :=
  ((List.range (Int.toNat (n - m + 1))) : List Nat).map fun (r : Nat) => (m + r : Int)

/-- If `m ≤ n`, list corresponding to the interval `[m, n]` if `m ≤ n`; otherwise `[m, n]` reversed. -/
private def Int.irange (m n : Int) : List Int :=
  if m ≤ n then m.irange_aux n else (n.irange_aux m).reverse

/-- The length of `Int.irange a b`. -/
private def Int.irange_length (a b : Int) : Nat :=
  (a - b).natAbs + 1

namespace Day09

def day := "09"

abbrev Point := Int × Int

def parse_single (line : String) : Point :=
  match line.splitOn "," |>.map String.toInt! with
  | [a, b] => (a, b)
  | _ => panic! s!"invalid data in day {day}"

def parse (raw : String) : Array Point :=
  raw.splitOn "\n" |>.map parse_single |>.toArray

def part1 (points : Array Point) : Nat :=
  points.all_pairs.map
    (fun ((x₁, y₁), (x₂, y₂)) => (Int.irange_length x₁ x₂) * (Int.irange_length y₁ y₂))
  |>.maxI

/-- Coordinate compresses a list of `Int`s.

Sort the list, removes duplicates, and constructs a new list starting at 1,
increasing by
* 1 if the next number is only 1 more than the previous number, and
* 2 if the next number is at least 2 more than the previous number.

Finally, returns a `HashMap` sending the list to its compression. -/
def compress_ints (l : List Int) : HashMap Int Int :=
  let l := l.mergeSort.eraseReps
  let compressed_l := l.adj_pairs.map
    (fun (a, b) => if b - a > 1 then (2 : Int) else
      (if b - a = 1 then 1 else panic! s!"compress_aux got bad pair {a}, {b}")
    )
  |>.scanl (· + ·) 1
  HashMap.ofList (l.zip compressed_l)

/-- Coordinate compresses an array of `Point`s. -/
def part2.compress (points : Array Point) : Array Point :=
  let xs := compress_ints <| points.map Prod.fst |>.toList
  let ys := compress_ints <| points.map Prod.snd |>.toList
  points.map (fun (x, y) => (xs[x]!, ys[y]!))

/-- Constructs the boundary of an axis-aligned polygon, given as a list of `Point`s of its vertices. -/
def part2.boundary (points : Array Point) : List Point :=
  points.push points[0]! |>.toList.adj_pairs
  |>.flatMap (fun ((x₁, y₁), (x₂, y₂)) => ((x₁.irange x₂).product (y₁.irange y₂)).tail)

/-- Gets the bounding box with padding 1 of the `points`,
removing the boundary of the points as determined by `part2_boundary`. -/
def part2.grid_without_boundary (points : Array Point) : HashSet Point :=
  let boundary := part2.boundary points
  let x_max := points.map Prod.fst |>.maxI
  let y_max := points.map Prod.snd |>.maxI
  Int.irange 0 (x_max + 1) |>.product (Int.irange 0 (y_max + 1))
    |>.filter (· ∉ boundary) |> HashSet.ofList

/-- Use DFS to delete the region containing `(0, 0)` in a `grid` created by `part2.grid_without_boundary`. -/
def part2.DFS (grid : HashSet Point) : HashSet Point :=
  aux grid ∅ [(0, 0)] (fuel := 10 * (grid.size + 1))
  where
  /-- Helper for `part2.DFS`. -/
  aux (grid seen : HashSet Point) (todo : List Point) (fuel : Nat) : HashSet Point :=
  match fuel with
  | 0 => panic! s!"day {day} part2.DFS.aux ran out of fuel"
  | fuel+1 =>
    match todo with
    | [] => grid.filter (· ∉ seen)
    | p :: todo =>
      if p ∈ seen then aux grid seen todo fuel else
      let seen := seen.insert p
      let (x, y) := p
      let new_todo := [(1, 0), (-1, 0), (0, 1), (0, -1)]
        |>.map (fun (dx, dy) => (x + dx, y + dy))
        |>.filter (· ∈ grid)
      aux grid seen (new_todo ++ todo) fuel

-- takes ~20s on my input!
-- does not handle common would-be edge cases, but it should work for all AoC inputs
def part2 (points : Array Point) : Nat :=
  let new_points := part2.compress points
  let boundary := HashSet.ofList <| part2.boundary new_points
  let grid := part2.grid_without_boundary new_points
  let inside := boundary.insertMany (part2.DFS grid)
  points.zip new_points |>.all_pairs.filter (
    fun ((_, (x₁, y₁)), (_, (x₂, y₂))) =>
    (part2.boundary #[(x₁, y₁), (x₁, y₂), (x₂, y₂), (x₂, y₁)]).all (· ∈ inside)
  ) |>.map (
    fun (((old_x₁, old_y₁), _), ((old_x₂, old_y₂), _)) =>
    (old_x₁.irange_length old_x₂) * (old_y₁.irange_length old_y₂)
  ) |>.maxI

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day09
