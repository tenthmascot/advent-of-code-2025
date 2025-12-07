import AoC2025.Utils
import AoC2025.AoCUtils

namespace Day06

def day := "06"

/-- Transposes a "rectangular" string. -/
def str_transpose (s : String) : String :=
  let grid := s.splitOn "\n" |>.map String.toList
  "\n".intercalate (grid.transpose.map String.ofList)

def parse (raw : String) : List String :=
  let cols := (str_transpose raw).splitOn "\n"
  let cols := cols.map (fun s => if s.all (· == ' ') then "" else s)
  "\n".intercalate cols |>.splitOn "\n\n"

def compute (s : String) : Nat :=
  let op := if s.contains '+' then List.sum else List.prod
  let s := s.toList.filter (fun ch => !" +*".contains ch) |> String.ofList
  let nums := s.trim.splitOn "\n" |>.map String.toNat!
  op nums

def part1 (grids : List String) : Nat :=
  grids.map (compute ∘ str_transpose) |>.sum

def part2 (grids : List String) : Nat :=
  grids.map compute |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day06
