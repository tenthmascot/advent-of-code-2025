import AoC2025.Utils
import AoC2025.AoCUtils
open Std

private local instance {α} [Hashable α] {n} : Hashable (Vector α n) where
  hash as := hash as.toArray

namespace Day10

def day := "10"

structure RawData where
  lights : Array (Fin 2)
  buttons : Array (Array Nat)
  joltages : Array Nat
deriving Inhabited

structure Data where
  {n : Nat}
  lights : Vector (Fin 2) n
  buttons : Array (Vector Nat n)
  joltages : Vector Nat n

def RawData.unraw (data : RawData) : Data :=
  let ⟨lights, buttons, joltages⟩ := data
  let n := lights.size
  let lights := lights.toVector.cast rfl
  let joltages := if h : joltages.size = n
    then joltages.toVector.cast h
    else panic! s!"invalid data in day {day}"
  let buttons := buttons.map (
    fun row => if h : row.size = n
    then row.toVector.cast h
    else panic! s!"invalid data in day {day}"
  )
  Data.mk (n := n) lights buttons joltages

def parse_single_raw (raw : String) : RawData :=
  let raw := raw.toList.filter (!"[](){}".contains ·) |> String.ofList
  match raw.splitOn " " with
  | [] => panic! s!"invalid data in day {day}"
  | lights :: words =>
    let lights := lights.toList.toArray.map (if · = '#' then 1 else 0)
    let joltages := words.getLast!.splitOn "," |>.toArray.map String.toNat!
    let buttons := words.dropLast.toArray.map (fun word =>
      let vals := word.splitOn "," |>.map String.toNat!
      Array.range lights.size |>.map (if · ∈ vals then 1 else 0)
    )
    ⟨lights, buttons, joltages⟩

def parse (raw : String) : List Data :=
  raw.splitOn "\n" |>.map (RawData.unraw ∘ parse_single_raw)

def patterns {n} (buttons : Array (Vector Nat n)) : HashMap (Vector (Fin 2) n) (HashMap (Vector Nat n) Nat) :=
  buttons.toList.sublists.foldl (
    fun out row =>
      let pattern := row.sum
      let parityPattern := pattern.map (Fin.ofNat _ ·)
      if h : parityPattern ∈ out then
        out.insert parityPattern
          (out[parityPattern].insert pattern (min (out[parityPattern].getD pattern 1000000) row.length))
      else out.insert parityPattern {(pattern, row.length)}
  ) ∅

def part1_single (data : Data) : Nat :=
  let ⟨lights, buttons, _⟩ := data
  let patternCosts := patterns buttons
  patternCosts[lights]!.toList.map Prod.snd |>.min?.get!

def part1 (datas : List Data) : Nat :=
  datas.map part1_single |>.sum

def part2_single (data : Data) : Nat :=
  let ⟨_, buttons, joltages⟩ := data
  inner (patterns buttons) joltages (fuel := 10) |>.run' ∅ |>.run where
  inner {n} (patternCosts : HashMap (Vector (Fin 2) n) (HashMap (Vector Nat n) Nat))
      (joltages : Vector Nat n) (fuel : Nat) :
      StateM (HashMap (Vector Nat n) Nat) Nat := do
    if let some answer := (← get)[joltages]?
      then return answer
    return ←
      match fuel with
      | 0 => panic! s!"part2_single.inner ran out of fuel"
      | fuel+1 =>
        if joltages = 0 then return 0 else do
        let parityJoltages := joltages.map (Fin.ofNat _ ·)
        let mut possibleAnswers := []
        for (pattern, patternCost) in patternCosts.getD parityJoltages ∅ do
          if pattern.zip joltages |>.all (fun (i, j) => i ≤ j) then
            -- intentional Nat subtraction
            let newJoltages := pattern.zipWith (fun i j => (j - i) / 2) joltages
            possibleAnswers := possibleAnswers.cons (patternCost + 2 * (← inner patternCosts newJoltages fuel))
        let answer := possibleAnswers.min?.getD 1000000
        modify (·.insert joltages answer)
        return answer

def part2 (datas : List Data) : Nat :=
  datas.map part2_single |>.sum

def solution := AoCSolution.mkOfParse parse part1 part2

def main := solution.run

end Day10
