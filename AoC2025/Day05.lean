import AoC2025.Utils
import AoC2025.AoCUtils
import Std
open Std

local instance : Inhabited Range := ⟨[:0]⟩
local instance {i : Nat} {r : Range} : Decidable (i ∈ r) :=
  decidable_of_iff (r.start ≤ i ∧ i < r.stop ∧ (i - r.start) % r.step = 0) (by rfl)

namespace Day05

def day := "05"

def parse_fst (para : String) : List Range :=
  para.splitOn "\n" |>.map (match ·.splitOn "-" with
    | [s1, s2] => [s1.toNat!:s2.toNat!+1]
    | _ => panic! s!"invalid data in day {day}"
  )

def parse (raw : String) : List Range × List Nat :=
  match raw.splitOn "\n\n" with
  | [p1, p2] => (parse_fst p1, (p2.splitOn "\n" |>.map String.toNat!))
  | _ => panic! s!"invalid data in day {day}"

def part1 (data : List Range × List Nat) : Nat :=
  let (ranges, vals) := data
  vals.countP (fun val => ranges.any (val ∈ ·))

def part2 (data : List Range × List Nat) : Nat :=
  let (ranges, _) := data
  let endpoints := ranges.flatMap (fun r => [(r.start, 1), (r.stop, -1)])
    |>.mergeSort (·.1 ≤ ·.1)
  -- intentional Nat subtraction
  let groups := endpoints.adj_pairs.map (fun ((x1, d1), (x2, _)) => (x2 - x1, d1))
    |>.scanl (fun (_, d) (dx, sd) => (dx, d+sd)) (0, (0 : Int))
  groups.filter (·.2 > 0) |>.map Prod.fst |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day05
