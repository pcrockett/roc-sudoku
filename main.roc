app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]

main! : List Arg => Result {} _
main! = |_args|
    render_board(init_board)
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

init_board = {
    rows: init_row |> List.repeat(9),
}

init_row = init_cell |> List.repeat(9)

init_cell = {
    candidates: List.range({ start: At(1), end: At(9), step: 1 }),
}
