import AoC2025.Utils
import AoC2025.AoCUtils
import AoC2025.Utils.Bad.DSU
open Std

namespace Day08

def day := "08"

abbrev Point := Int × Int × Int

def Point.sqDist (p q : Point) : Int := match p, q with
  | (x₁, y₁, z₁), (x₂, y₂, z₂) => (x₁ - x₂) ^ 2 + (y₁ - y₂) ^ 2 + (z₁ - z₂) ^ 2

def parse_single (line : String) : Point :=
  match line.splitOn "," |>.map String.toInt! with
  | [a, b, c] => (a, b, c)
  | _ => panic! s!"invalid data in day {day}"

def parse (raw : String) : Array Point :=
  raw.splitOn "\n" |>.map parse_single |>.toArray

def sorted_distances (points : Array Point) :
    List (Point × Point) :=
  points.all_pairs.toList.mergeSort
    (fun (p₁, q₁) (p₂, q₂) => p₁.sqDist q₁ ≤ p₂.sqDist q₂)

def part1 (points : Array Point) : Int :=
  let edges := sorted_distances points |>.take 1000
  let dsu := edges.foldl (fun dsu (p, q) => dsu.join! p q)
    (DSU.new_ofList points.toList)
  let roots := points.filter (fun p => dsu.find! p == p)
  let sizes := roots.map dsu.size!
  sizes.qsort (· > ·) |>.take 3 |>.prod

def part2_aux (edges : List (Point × Point)) (dsu : DSU Point) (merges_needed : Nat) : Int :=
  match edges with
  | [] => panic! s!"day {day} part 2 finished without connecting graph"
  | (p, q) :: edges =>
    if dsu.find! p == dsu.find! q
      then part2_aux edges dsu merges_needed
      -- intentional Nat subtraction
      else match merges_needed - 1 with
      | 0 => match p, q with
        | (x₁, _, _), (x₂, _, _) => x₁ * x₂
      | merges_needed => part2_aux edges (dsu.join! p q) merges_needed

def part2 (points : Array Point) : Int :=
  let edges := sorted_distances points
  -- intentional Nat subtraction
  part2_aux edges (DSU.new_ofList points.toList) (points.size - 1)

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day08
