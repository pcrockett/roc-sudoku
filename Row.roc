module [
    Row,
    eliminate_candidates,
    known_values,
    new,
    to_str,
]

import Cell exposing [Cell]

Row : List Cell

new : List U8 -> Row
new = |values| List.map(values, Cell.new)

known_values : List Cell -> List U8
known_values = |cells|
    cells
    |> List.keep_if(Cell.is_known)
    |> List.map(|c| List.first(c.candidates) ?? crash "Found no candidates in known_values")

expect
    known_values(
        [
            { candidates: [1, 2, 3] },
            { candidates: [2] },
            { candidates: [5] },
            { candidates: [4, 6] },
        ],
    )
    == [2, 5]

eliminate_candidates : Row, List U8 -> List Cell
eliminate_candidates = |row, values|
    row
    |> List.map(
        |cell|
            if Cell.is_known(cell) then
                cell
            else
                {
                    candidates: cell.candidates
                    |> List.drop_if(
                        |v| List.contains(values, v),
                    ),
                },
    )

expect
    [0, 5, 0]
    |> new
    |> eliminate_candidates([2, 5, 9])
    == [
        { candidates: [1, 3, 4, 6, 7, 8] },
        { candidates: [5] },
        { candidates: [1, 3, 4, 6, 7, 8] },
    ]

to_str : Row -> Str
to_str = |row|
    row
    |> List.map_with_index(
        |cell, index|
            when index is
                0 -> Cell.to_str(cell)
                3 | 6 -> "│${Cell.to_str(cell)}"
                _ -> " ${Cell.to_str(cell)}",
    )
    |> Str.join_with("")

expect
    [1, 1, 1, 2, 2, 2, 3, 3, 3]
    |> new
    |> to_str
    == "1 1 1│2 2 2│3 3 3"
