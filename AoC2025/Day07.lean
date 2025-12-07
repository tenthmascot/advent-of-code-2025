import AoC2025.Utils
import AoC2025.AoCUtils
open Std

namespace Day07

def day := "07"

def parse (raw : String) : HashMap (Int × Int) Char :=
  raw.splitOn "\n"
    |>.zipIdx.flatMap (
      fun (line, r) => line.toList.zipIdx.map (fun (ch, c) => (((r, c) : Int × Int), ch))
    ) |> HashMap.ofList

def make_dp.grow (grid : HashMap (Int × Int) Char) (r0 : Int)
    (dp : HashMap (Int × Int) Nat) : HashMap (Int × Int) Nat :=
  let cells := grid.keys.filter (fun (r, _) => r == r0)
  let values := cells.map (fun (r, c) =>
    (if grid[(r, c)]? == 'S' then 1 else 0) +
    (if grid[(r-1, c)]? ∈ [some '.', 'S'] then dp.getD (r-1, c) 0 else 0) +
    (if grid[(r-1, c-1)]? == '^' then dp.getD (r-1, c-1) 0 else 0) +
    (if grid[(r-1, c+1)]? == '^' then dp.getD (r-1, c+1) 0 else 0)
  )
  dp.insertMany <| HashMap.ofList (cells.zip values)

def make_dp (grid : HashMap (Int × Int) Char) : HashMap (Int × Int) Nat :=
  let h := (grid.keys.map Prod.fst |>.max?.get!.toNat) + 1
  List.range h |>.foldl (
    fun dp (r0 : Nat) => make_dp.grow grid r0 dp
  ) ∅

def part1 (grid : HashMap (Int × Int) Char) : Nat :=
  (make_dp grid).toList.countP (fun (p, val) => grid[p]? == '^' && val > 0)

def part2 (grid : HashMap (Int × Int) Char) : Nat :=
  let max_r := grid.keys.map Prod.fst |>.max?.get!
  (make_dp grid).toList.filter (fun ((r, _), _) => r == max_r)
    |>.map Prod.snd |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day07
