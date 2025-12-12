import AoC2025.Utils
import AoC2025.AoCUtils

namespace Day12

def day := "12"

structure LineData where
  area : Nat
  shapeBank : List Nat
deriving Inhabited

structure Data where
  shapeAreas : List Nat
  lineDatas : List LineData

def parse_lineData (line : String) : LineData :=
  match line.replace "x" " " |>.replace ":" "" |>.splitOn " " |>.map String.toNat! with
  | h :: w :: as => ⟨h * w, as⟩
  | _ => panic! s!"invalid data in day {day}"

def parse (raw : String) : Data :=
  let paras := raw.splitOn "\n\n"
  let shapeAreas := paras.dropLast |>.map (·.count '#')
  let lineDatas := paras.getLast!.splitOn "\n" |>.map parse_lineData
  ⟨shapeAreas, lineDatas⟩

def part1 (data : Data) : Nat :=
  let ⟨shapeAreas, lineDatas⟩ := data
  lineDatas.countP (fun ⟨area, shapeBank⟩ => (shapeBank.zipWith (· * ·) shapeAreas).sum ≤ area)

def part2 (_ : Data) : String :=
  "Congratulations!"

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day12
