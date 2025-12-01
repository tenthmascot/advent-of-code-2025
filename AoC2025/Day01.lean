import AoC2025.Utils
import AoC2025.AoCUtils

namespace Day01

def day := "01"

def parse_step (op : String) : Int × Nat :=
  (if String.Pos.Raw.get! op 0 == 'R' then 1 else -1, (op.drop 1).toNat!)

def parse (raw : String) : List (Int × Nat) :=
  raw.splitOn "\n" |>.map parse_step

def solve_aux (l : List Int) : Nat :=
  l.scanl (· + Fin.intCast ·) (50 : Fin 100) |>.count 0

def part1 (l : List (Int × Nat)) : Nat :=
  solve_aux <| l.map (fun (d, n) => d * n)

def part2 (l : List (Int × Nat)) : Nat :=
  solve_aux <| l.flatMap (fun (d, n) => List.replicate n d)

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day01
