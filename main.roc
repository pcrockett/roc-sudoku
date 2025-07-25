app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]
import Cell exposing [Cell]
import Group
import Board exposing [Board]

main! : List Arg => Result {} _
main! = |_args|
    board =
        [
            [0, 0, 2, 6, 0, 0, 0, 5, 0],
            [8, 0, 0, 0, 0, 0, 0, 9, 3],
            [0, 5, 0, 7, 0, 0, 2, 0, 0],
            [0, 0, 0, 0, 4, 0, 5, 0, 0],
            [0, 3, 0, 0, 0, 0, 0, 0, 9],
            [0, 0, 0, 0, 0, 2, 0, 0, 0],
            [0, 0, 0, 2, 6, 0, 0, 0, 1],
            [3, 1, 0, 0, 5, 0, 7, 4, 0],
            [0, 0, 0, 0, 0, 1, 0, 3, 0],
        ]
        |> List.map(Group.from_values)
        |> Board.from_rows

    Stdout.line!(Board.to_str(board))?

    Stdout.line!("\nSolving...\n")?

    transformed_board =
        [
            unique_in_group(Board.rows, Board.from_rows),
            unique_in_group(Board.cols, Board.from_cols),
            unique_in_group(Board.boxes, Board.from_boxes),
        ]
        |> solve(board)

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

solve = |transforms, board|
    transformed_board : Board
    transformed_board =
        transforms
        |> List.walk(
            board,
            |b, transform| transform(b),
        )
    if transformed_board == board then
        transformed_board
    else
        solve(transforms, transformed_board)

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

unique_in_group = |group_func, reassemble_func|
    transform = |board|
        board
        |> group_func
        |> List.map(
            |grp|
                known_values = Group.known_values(grp)
                grp |> Group.eliminate_candidates(known_values),
        )
        |> reassemble_func
    transform

expect
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
        |> Board.from_rows

    # none of these transforms should be able to solve the board; they just eliminate
    # candidates. so the string representation of the board should be identical for all
    # of them.
    expected =
        """
        3 · ·│1 · ·│8 · 5
        · · ·│9 · ·│7 2 ·
        · · 6│· · ·│· · ·
        ─────┼─────┼─────
        · · ·│· · ·│· · 8
        · 2 ·│4 8 7│· · ·
        · 7 ·│· · 1│· · ·
        ─────┼─────┼─────
        2 3 ·│· · ·│· · ·
        · · 5│· · 9│· 4 ·
        4 · ·│· · ·│2 · 1
        """

    transforms = [
        unique_in_group(Board.rows, Board.from_rows),
        unique_in_group(Board.cols, Board.from_cols),
        unique_in_group(Board.boxes, Board.from_boxes),
    ]

    transforms
    |> List.map(|transform| transform(board))
    |> List.map(Board.to_str)
    |> List.map(|actual| actual == expected)
    |> List.all(|result| result)

identical_cells = |group_func, reassemble_func|
    transform = |board|

        insert_index = |dict, (cell, index)|
            dict
            |> Dict.insert(
                cell,
                when Dict.get(dict, cell) is
                    Err(KeyNotFound) -> [index]
                    Ok(list) -> List.append(list, index),
            )

        board
        |> group_func
        |> List.map(
            |grp|
                cell_lookup =
                    grp
                    |> List.map_with_index(|cell, index| (cell, index))
                    |> List.walk(Dict.empty({}), insert_index)
                # TODO: find entries in the dict where the len(key_candidates) == len(value)
                grp,
        )
        |> reassemble_func
    transform

