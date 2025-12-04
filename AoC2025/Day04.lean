import AoC2025.Utils
import AoC2025.AoCUtils
import Std
open Std

namespace Day04

def day := "04"

def parse (raw : String) : HashSet (Int × Int) :=
  raw.splitOn "\n"
    |>.zipIdx.flatMap (
      fun (line, r) => line.toList.zipIdx.map (fun (ch, c) => (((r, c) : Int × Int), ch))
    ) |>.filter (fun (_, ch) => ch == '@')
    |>.map Prod.fst |> HashSet.ofList

def cycle (data : HashSet (Int × Int) × Nat) : HashSet (Int × Int) × Nat :=
  let (grid, acc) := data
  let new_grid := grid.filter (fun (r, c) => (List.padj8.countP (
      fun (dr, dc) => (r + dr, c + dc) ∈ grid
    )) ≥ 4
  )
  -- intentional Nat subtraction
  let new_acc := acc + (grid.size - new_grid.size)
  (new_grid, new_acc)

def repeat_cycle (data : HashSet (Int × Int) × Nat) : HashSet (Int × Int) × Nat :=
  let new_data := cycle data
  if _h : new_data.1.size < data.1.size then repeat_cycle new_data else data
termination_by data.1.size

def part1 (grid : HashSet (Int × Int)) : Nat :=
  cycle (grid, 0) |>.2

def part2 (grid : HashSet (Int × Int)) : Nat :=
  repeat_cycle (grid, 0) |>.2

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day04
