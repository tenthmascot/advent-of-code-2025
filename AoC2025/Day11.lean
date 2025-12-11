import AoC2025.Utils
import AoC2025.AoCUtils
open Std

namespace Day11

def day := "11"

def parse (raw : String) : HashMap String (List String) :=
  raw.splitOn "\n" |>.map (match ·.splitOn " " with
    | [] => panic! s!"invalid data in day {day}"
    | head :: tail => (head.dropRight 1, tail)
  ) |> HashMap.ofList

def dp (graph : HashMap String (List String)) (first last : String) : Nat :=
  inner graph first last (fuel := 10 * (graph.size + 1)) |>.run' ∅ |>.run where
  inner (graph : HashMap String (List String)) (first last : String) (fuel : Nat) : StateM (HashMap String Nat) Nat := do
    if let some answer := (← get)[first]?
      then return answer
    return ←
      match fuel with
      | 0 => panic! s!"dp.inner ran out of fuel with {first}, {last}"
      | fuel+1 =>
        if first = last then return 1 else do
          let answer := (← graph.getD first [] |>.mapM (inner graph · last fuel)).sum
          modify (·.insert first answer)
          return answer

def part1 (graph : HashMap String (List String)) : Nat :=
  dp graph "you" "out"

def part2 (graph : HashMap String (List String)) : Nat :=
    (dp graph "svr" "dac" * dp graph "dac" "fft" * dp graph "fft" "out")
  + (dp graph "svr" "fft" * dp graph "fft" "dac" * dp graph "dac" "out")

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day11
