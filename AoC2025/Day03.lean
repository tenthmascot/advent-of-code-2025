import AoC2025.Utils
import AoC2025.AoCUtils

namespace Day03

def day := "03"

def parse_line (line : String) : Array Nat :=
  line.toList.toArray.map (String.toNat! ∘ toString)

def parse (raw : String) : List (Array Nat) :=
  raw.splitOn "\n" |>.map parse_line

def solve_aux (line : Array Nat) (n : Nat) (start := 0) := match n with
| 0 => 0
| n+1 =>
    -- intentional Nat subtraction
    let head := line.maxI start (line.size - n)
    -- i'm sorry
    let idx := Array.findIdx?.loop (· == head) line start |>.get!
    head * 10 ^ n + solve_aux line n (idx + 1)

def part1 (lines : List (Array Nat)) : Nat :=
  lines.map (solve_aux · 2) |>.sum

def part2 (lines : List (Array Nat)) : Nat :=
  lines.map (solve_aux · 12) |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day03
