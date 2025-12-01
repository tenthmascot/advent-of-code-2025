/-- A solution to a part. -/
def SolutionPart {α} := String → α

/-- Solve a part, given an input file. -/
def SolutionPart.solve {α} (solution : @SolutionPart α) (file : System.FilePath) : IO α := do
  IO.FS.readFile file >>= pure ∘ solution

/-- Runs a solution to a part, printing the answer. -/
def SolutionPart.run {α} [ToString α] (part : String)
  (solution : @SolutionPart α) (file : System.FilePath) := do
  try
    let answer ← solution.solve file
    IO.println s!"Answer to {part}: {answer}"
  catch e =>
    IO.println s!"Error while solving {part}: {e}"
