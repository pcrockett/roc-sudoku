app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]

main! : List Arg => Result {} _
main! = |_args|
    board = new_board(
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
    )

    render_board(board)
    |> Stdout.line!

Cell : { candidates : List U8 }
Row : List Cell
Board : { rows : List Row }

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

new_board : List List U8 -> Board
new_board = |rows| {
    rows: List.map(rows, new_row),
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
