app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]

Board : { rows : List Row }
Row : List Cell
Cell : { candidates : List U8 }
Transform : Board -> Board

main! : List Arg => Result {} _
main! = |_args|
    board = new_board

    transforms : List Transform
    transforms = [
        # TODO: `unique_in_column` and `unique_in_box`
        unique_in_row,
    ]

    transformed_board : Board
    transformed_board =
        transforms
        |> List.walk(
            board,
            |b, transform| transform(b),
        )

    transformed_board
    |> render_board
    |> Stdout.line!?

    cells_to_eliminate =
        transformed_board.rows
        |> all_cells
        |> to_eliminate
        |> Num.to_str

    Stdout.line!("\nCandidates left to eliminate: ${cells_to_eliminate}")

render_board = |board|
    board.rows
    |> List.map_with_index(
        |row, index|
            when index is
                3 | 6 -> "─────┼─────┼─────\n${render_row(row)}"
                _ -> render_row(row),
    )
    |> Str.join_with("\n")

render_row = |row|
    row
    |> List.map_with_index(
        |cell, index|
            when index is
                0 -> render_cell(cell)
                3 | 6 -> "│${render_cell(cell)}"
                _ -> " ${render_cell(cell)}",
    )
    |> Str.join_with("")

render_cell = |cell|
    when cell is
        { candidates: [single] } -> Num.to_str(single)
        { candidates: [] } -> crash "Found no candidates in render_cell"
        _ -> "·"

new_board : Board
new_board = {
    # i would move this hard-coded board up to the main function and take it as a
    # parameter here, however that for some reason triggers a panic in the compiler.
    # there are plenty of other similar panics in the open issues on the roc repo, so i
    # think it's safe to assume it'll be fixed in time.
    rows: List.map(
        [
            [3, 0, 0, 1, 0, 0, 8, 0, 5],
            [0, 0, 0, 9, 0, 0, 7, 2, 0],
            [0, 0, 6, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 8],
            [0, 2, 0, 4, 8, 7, 0, 0, 0],
            [0, 7, 0, 0, 0, 1, 0, 0, 0],
            [2, 3, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 5, 0, 0, 9, 0, 4, 0],
            [4, 0, 0, 0, 0, 0, 2, 0, 1],
        ],
        new_row,
    ),
}

new_row : List U8 -> Row
new_row = |values| List.map(values, new_cell)

new_cell : U8 -> Cell
new_cell = |value|
    when value is
        0 -> empty_cell
        v -> { candidates: [v] }

empty_cell : Cell
empty_cell = {
    candidates: List.range({ start: At(1), end: At(9), step: 1 }),
}

all_cells : List Row -> List Cell
all_cells = |rows|
    List.join_map(rows, |r| r)

to_eliminate : List Cell -> U64
to_eliminate = |cells|
    cells
    |> List.map(|c| List.len(c.candidates) - 1)
    |> List.sum

is_known = |cell| List.len(cell.candidates) == 1

expect is_known({ candidates: [1] })
expect !is_known({ candidates: [1, 2] })

known_values : List Cell -> List U8
known_values = |cells|
    cells
    |> List.keep_if(is_known)
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

eliminate_candidates : List Cell, List U8 -> List Cell
eliminate_candidates = |cells, knowns|
    List.map(
        cells,
        |cell|
            if is_known(cell) then
                cell
            else
                {
                    candidates: cell.candidates |> List.drop_if(|v| List.contains(knowns, v)),
                },
    )

unique_in_row : Transform
unique_in_row = |board| {
    rows: board.rows
    |> List.map(
        |row| eliminate_candidates(row, known_values(row)),
    ),
}

expect
    unique_in_row(
        {
            rows: [
                [{ candidates: [4] }, { candidates: [1, 2, 3, 4, 5] }, { candidates: [3] }],
                [{ candidates: [3] }, { candidates: [3, 4, 5, 6, 7] }],
            ],
        },
    )
    == {
        rows: [
            [{ candidates: [4] }, { candidates: [1, 2, 5] }, { candidates: [3] }],
            [{ candidates: [3] }, { candidates: [4, 5, 6, 7] }],
        ],
    }
