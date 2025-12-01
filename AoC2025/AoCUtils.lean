import AoC2025.Utils.SolutionUtils

/-- An Advent of Code solution, consisting of two `SolutionPart`s, as well as the day number. -/
structure AoCSolution {α : Type u} {β : Type v} where
  part1 : @SolutionPart α
  part2 : @SolutionPart β
  day : String := by exact day

/-- An alternate convenience constructor for `AoCSolution` that applies a function as preprocessing for both parts. -/
def AoCSolution.mkOfParse {α β ω} (parse : String → ω) (part1 : ω → α) (part2 : ω → β)
    (day : String := by exact day) :=
  AoCSolution.mk (part1 ∘ parse) (part2 ∘ parse)

/-- The folder containing the input notes. -/
def inputFolder : System.FilePath := "input"

/-- The filename corresponding to a particular day. -/
def AoCInputOfDay (day : String) : System.FilePath :=
  inputFolder / s!"{day}"

/-- Runs a solution, printing both answers. -/
def AoCSolution.run (solution : @AoCSolution α β) [ToString α] [ToString β]
    (input : System.FilePath := by exact AoCInputOfDay day) := do
  IO.println s!"Day {solution.day}:"
  solution.part1.run "Part 1" input
  solution.part2.run "Part 2" input
