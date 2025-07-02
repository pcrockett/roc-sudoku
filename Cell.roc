module [
    Cell,
    candidates,
    from_value,
    is_known,
    new,
    to_str,
]

Cell := { candidates : List U8 } implements [Eq, Hash, Inspect]

new : List U8 -> Cell
new = |candidate_values| @Cell({ candidates: candidate_values })

from_value : U8 -> Cell
from_value = |value|
    when value is
        0 -> default
        v -> new([v])

expect from_value(5) == new([5])
expect from_value(0) == new([1, 2, 3, 4, 5, 6, 7, 8, 9])

candidates : Cell -> List U8
candidates = |@Cell(cell)| cell.candidates

default : Cell
default = List.range({ start: At(1), end: At(9), step: 1 }) |> new

is_known : Cell -> Bool
is_known = |cell| List.len(candidates(cell)) == 1

expect is_known(new([1]))
expect !is_known(new([1, 2]))

to_str : Cell -> Str
to_str = |cell|
    when candidates(cell) is
        [] -> crash "Found no candidates"
        [single] -> Num.to_str(single)
        _ -> "·"
