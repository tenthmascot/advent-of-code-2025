import AoC2025.Utils
import AoC2025.AoCUtils

namespace Day02

def day := "02"

def parse_line (line : String) : List Nat :=
  let nums := line.splitOn "-" |>.map String.toNat!
  [nums[0]!:nums[1]!+1].toList

def parse (raw : String) : List (Nat × String) :=
  raw.splitOn "," |>.flatMap parse_line
  |>.map (fun x => (x, toString x))

def part1_ok (s : String) : Bool :=
  s.toSlice.take (s.length / 2) == s.toSlice.drop (s.length / 2)

def part1 (l : List (Nat × String)) : Nat :=
  l.filter (fun (_, s) => part1_ok s) |>.map Prod.fst |>.sum

def part2_ok (s : String) : Bool :=
  [1:s.length].toList.any (fun p => s == s.drop p ++ s.take p)

def part2 (l : List (Nat × String)) : Nat :=
  l.filter (fun (_, s) => part2_ok s) |>.map Prod.fst |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day02
