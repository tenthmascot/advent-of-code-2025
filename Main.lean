import AoC2025

/-- A list of the main definitions of all quests. -/
def mains := [
  Day01.main,
  Day02.main,
  Day03.main,
  Day04.main,
  Day05.main,
  Day06.main,
  Day07.main,
  Day08.main,
  Day09.main,
  Day10.main,
  Day11.main,
]

def main (args : List String) : IO Unit := do
  if args.length > 1 then
    IO.println "Too many arguments."
    IO.println "Run with no arguments to run all days,"
    IO.println s!"or provide one argument (1~{mains.length}) to run that day."
  else if h : args.length = 1 then
    let day := args[0].toNat?
    if h : day.isSome = true then
      let day := day.get h
      if h : 1 ≤ day ∧ day ≤ mains.length then
        let main := mains[day - 1]
        main
      else
        IO.println "Day number is out of bounds."
    else
      IO.println "Day number is not a natural number."
  else
    for main in mains.intersperse (IO.println "") do
      main
