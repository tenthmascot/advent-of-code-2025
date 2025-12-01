# AoC2025

These are my solutions for [Advent of Code 2025](https://adventofcode.com/2025) in Lean 4.

## Usage

Place your puzzle input in [`input`](input). See [that folder's README](input/README.md) for more details.

Run `lake exe aoc2025` to output answers to all days. To only output answers for a single day, use the day number as the sole argument. (For example, to only output answers for day 1, run `lake exe aoc2025 1`.)

This project does not have Mathlib as a dependency. If you want to include Mathlib, then you must uncomment `import AoC2025.Utils.MathlibUtils` from [`Utils.lean`](AoC2025/Utils.lean), as [`MathlibUtils.lean`](AoC2025/Utils/MathlibUtils.lean) duplicates functions from Mathlib.
