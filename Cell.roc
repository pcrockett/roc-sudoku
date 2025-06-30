module [
    Cell,
    new,
    is_known,
    to_str,
]

Cell : { candidates : List U8 }

new : U8 -> Cell
new = |value|
    when value is
        0 -> default
        v -> { candidates: [v] }

expect new(5) == { candidates: [5] }
expect new(0) == { candidates: [1, 2, 3, 4, 5, 6, 7, 8, 9] }

default : Cell
default = {
    candidates: List.range({ start: At(1), end: At(9), step: 1 }),
}

is_known : Cell -> Bool
is_known = |cell| List.len(cell.candidates) == 1

expect is_known({ candidates: [1] })
expect !is_known({ candidates: [1, 2] })

to_str : Cell -> Str
to_str = |cell|
    when cell is
        { candidates: [single] } -> Num.to_str(single)
        { candidates: [] } -> crash "Found no candidates in render_cell"
        _ -> "·"
