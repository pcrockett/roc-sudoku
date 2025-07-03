module [
    Board,
    cells,
    col,
    cols,
    new,
    row,
    rows,
    to_str,
]

import Cell exposing [Cell]
import Group exposing [Group]

board_size = 9

Board := { cells : List Cell } implements [Eq]

new : List Group -> Board
new = |rows_list|
    @Board(
        { cells: rows_list |> List.join_map(|r| r |> Group.cells) },
    )

row : Board, U64 -> Group
row = |@Board(board), index|
    List.range(
        {
            start: At(index * board_size),
            end: Before((index + 1) * board_size),
            step: 1,
        },
    )
    |> List.map(
        |i| (board.cells |> List.get(i)) ?? crash "Index out of bounds",
    )
    |> Group.new

expect
    result =
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
        |> new
        |> row(0)
        |> Group.to_str
    result == "3 · ·│1 · ·│8 · 5"

expect
    result =
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
        |> new
        |> row(5)
        |> Group.to_str
    result == "· 7 ·│· · 1│· · ·"

expect
    result =
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
        |> new
        |> row(8)
        |> Group.to_str
    result == "4 · ·│· · ·│2 · 1"

rows : Board -> List Group
rows = |board|
    List.range({ start: At(0), end: Before(board_size), step: 1 })
    |> List.map(|i| row(board, i))

expect
    result =
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
        |> new
        |> rows
        |> List.map(|r| Group.to_str(r))
        |> Str.join_with("\n")
    result
    ==
    """
    3 · ·│1 · ·│8 · 5
    · · ·│9 · ·│7 2 ·
    · · 6│· · ·│· · ·
    · · ·│· · ·│· · 8
    · 2 ·│4 8 7│· · ·
    · 7 ·│· · 1│· · ·
    2 3 ·│· · ·│· · ·
    · · 5│· · 9│· 4 ·
    4 · ·│· · ·│2 · 1
    """

col : Board, U64 -> Group
col = |@Board(board), index|
    List.range(
        {
            start: At(index),
            end: Before(board_size * board_size),
            step: board_size,
        },
    )
    |> List.map(
        |i| (board.cells |> List.get(i)) ?? crash "Index out of bounds",
    )
    |> Group.new

expect
    result =
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
        |> new
        |> col(0)
        |> Group.to_str
    result == "3 · ·│· · ·│2 · 4"

expect
    result =
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
        |> new
        |> col(5)
        |> Group.to_str
    result == "· · ·│· 7 1│· 9 ·"

expect
    result =
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
        |> new
        |> col(8)
        |> Group.to_str
    result == "5 · ·│8 · ·│· · 1"

cols : Board -> List Group
cols = |board|
    List.range({ start: At(0), end: Before(board_size), step: 1 })
    |> List.map(|i| col(board, i))

expect
    result =
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
        |> new
        |> cols
        |> List.map(|r| Group.to_str(r))
        |> Str.join_with("\n")
    result
    ==
    """
    3 · ·│· · ·│2 · 4
    · · ·│· 2 7│3 · ·
    · · 6│· · ·│· 5 ·
    1 9 ·│· 4 ·│· · ·
    · · ·│· 8 ·│· · ·
    · · ·│· 7 1│· 9 ·
    8 7 ·│· · ·│· · 2
    · 2 ·│· · ·│· 4 ·
    5 · ·│8 · ·│· · 1
    """

cells : Board -> List Cell
cells = |board|
    board
    |> rows
    |> List.join_map(Group.cells)

to_str : Board -> Str
to_str = |board|
    board
    |> rows
    |> List.map_with_index(
        |r, index|
            when index is
                3 | 6 -> "─────┼─────┼─────\n${Group.to_str(r)}"
                _ -> Group.to_str(r),
    )
    |> Str.join_with("\n")
