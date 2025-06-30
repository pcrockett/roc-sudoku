app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]

Board : { rows : List Row }
Row : List Cell
Cell : { candidates : List U8 }

main! : List Arg => Result {} _
main! = |_args|
    new_board
    |> render_board
    |> Stdout.line!

render_board = |board|
    List.map(board.rows, render_row)
    |> Str.join_with("\n")

render_row = |row|
    List.map(row, render_cell)
    |> Str.join_with(" ")

render_cell = |cell|
    when cell is
        { candidates: [single] } -> Num.to_str(single)
        { candidates: [] } -> "!"
        _ -> "?"

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
