app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]
import Cell exposing [Cell]
import Group
import Board exposing [Board]

Transform : Board -> Board

main! : List Arg => Result {} _
main! = |_args|
    board =
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
        ]
        |> List.map(Group.from_values)
        |> Board.new

    transforms : List Transform
    transforms = [
        # TODO: `unique_in_box`
        unique_in_row,
        unique_in_col,
    ]

    transformed_board : Board
    transformed_board =
        transforms
        |> List.walk(
            board,
            |b, transform| transform(b),
        )

    transformed_board
    |> Board.to_str
    |> Stdout.line!?

    cells_to_eliminate =
        transformed_board
        |> Board.cells
        |> to_eliminate
        |> Num.to_str
    Stdout.line!("\nCandidates left to eliminate: ${cells_to_eliminate}")?

    known_cells =
        transformed_board
        |> Board.cells
        |> List.keep_if(|c| Cell.is_known(c))
        |> List.len
        |> Num.to_str
    Stdout.line!("Solved cells: ${known_cells}")

to_eliminate : List Cell -> U64
to_eliminate = |cells|
    cells
    |> List.map(|c| (c |> Cell.candidates |> List.len) - 1)
    |> List.sum

expect
    to_eliminate(
        [
            Cell.new([1, 2, 3]),
            Cell.new([4]),
            Cell.new([5, 6, 7, 8]),
        ],
    )
    == 5

unique_in_row : Transform
unique_in_row = |board|
    board
    |> Board.rows
    |> List.map(
        |row|
            known_values = Group.known_values(row)
            row |> Group.eliminate_candidates(known_values),
    )
    |> Board.new

unique_in_col : Transform
unique_in_col = |board|
    board
    |> Board.cols
    |> List.map(
        |col|
            known_values = Group.known_values(col)
            col |> Group.eliminate_candidates(known_values),
    )
    |> Board.new

# TODO: find a more elegant way to test this on a full board
#       maybe fill missing values with default cells?
#
# expect
#     Board.new(
#         [
#             Group.from_values([4, 0, 3]),
#             Group.from_values([3, 0]),
#         ],
#     )
#     |> unique_in_row
#     == Board.new(
#         [
#             Group.new(
#                 [
#                     Cell.new([4]),
#                     Cell.new([1, 2, 5, 6, 7, 8, 9]),
#                     Cell.new([3]),
#                 ],
#             ),
#             Group.new(
#                 [
#                     Cell.new([3]),
#                     Cell.new([1, 2, 4, 5, 6, 7, 8, 9]),
#                 ],
#             ),
#         ],
#     )
