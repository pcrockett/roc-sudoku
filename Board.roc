module [
    Board,
    cells,
    new,
    rows,
    to_str,
]

import Cell exposing [Cell]
import Row exposing [Row]

Board := { rows : List Row } implements [Eq]

new : List Row -> Board
new = |rows_list|
    @Board(
        {
            rows: rows_list,
        },
    )

rows : Board -> List Row
rows = |@Board(board)| board.rows

cells : Board -> List Cell
cells = |board|
    board
    |> rows
    |> List.join_map(Row.cells)

to_str : Board -> Str
to_str = |board|
    board
    |> rows
    |> List.map_with_index(
        |row, index|
            when index is
                3 | 6 -> "─────┼─────┼─────\n${Row.to_str(row)}"
                _ -> Row.to_str(row),
    )
    |> Str.join_with("\n")
