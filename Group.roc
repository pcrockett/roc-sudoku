module [
    Group,
    cells,
    eliminate_candidates,
    from_values,
    known_values,
    new,
    to_str,
]

import Cell exposing [Cell]

Group := List Cell implements [Eq, Inspect]

new : List Cell -> Group
new = |cell_list| @Group(cell_list)

from_values : List U8 -> Group
from_values = |values| List.map(values, Cell.from_value) |> new

cells : Group -> List Cell
cells = |@Group(row)| row

known_values : Group -> List U8
known_values = |row|
    row
    |> cells
    |> List.keep_if(Cell.is_known)
    |> List.map(
        |cell|
            only_candidate =
                cell
                |> Cell.candidates
                |> List.first
            only_candidate ?? crash "Found no candidates",
    )

expect
    known_values(
        new(
            [
                Cell.new([1, 2, 3]),
                Cell.new([2]),
                Cell.new([5]),
                Cell.new([4, 6]),
            ],
        ),
    )
    == [2, 5]

eliminate_candidates : Group, List U8 -> Group
eliminate_candidates = |row, values|
    row
    |> cells
    |> List.map(
        |cell|
            if Cell.is_known(cell) then
                cell
            else
                cell
                |> Cell.candidates
                |> List.drop_if(
                    |v| List.contains(values, v),
                )
                |> Cell.new,
    )
    |> new

expect
    from_values([0, 5, 0])
    |> eliminate_candidates([2, 5, 9])
    == new(
        [
            Cell.new([1, 3, 4, 6, 7, 8]),
            Cell.new([5]),
            Cell.new([1, 3, 4, 6, 7, 8]),
        ],
    )

to_str : Group -> Str
to_str = |row|
    row
    |> cells
    |> List.map_with_index(
        |cell, index|
            when index is
                0 -> Cell.to_str(cell)
                3 | 6 -> "│${Cell.to_str(cell)}"
                _ -> " ${Cell.to_str(cell)}",
    )
    |> Str.join_with("")

expect
    from_values([1, 1, 1, 2, 2, 2, 3, 3, 3])
    |> to_str
    == "1 1 1│2 2 2│3 3 3"
