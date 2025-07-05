module [
    Board,
    box,
    boxes,
    cells,
    col,
    cols,
    from_cols,
    from_rows,
    row,
    rows,
    to_str,
]

import Cell exposing [Cell]
import Group exposing [Group]

board_size = 9

Board := { cells : List Cell } implements [Eq]

from_rows : List Group -> Board
from_rows = |rows_list|
    @Board(
        { cells: rows_list |> List.join_map(|r| r |> Group.cells) },
    )

from_cols : List Group -> Board
from_cols = |cols_list|
    @Board(
        {
            cells: List.range({ start: At(0), end: At(8), step: 1 })
            |> List.join_map(
                |index|
                    cols_list
                    |> List.map(
                        |c| Group.get(c, index),
                    ),
            ),
        },
    )

expect
    [
        [1, 1, 1, 2, 2, 2, 3, 3, 3],
        [4, 4, 4, 5, 5, 5, 6, 6, 6],
        [7, 7, 7, 8, 8, 8, 9, 9, 9],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    |> List.map(Group.from_values)
    |> from_cols
    |> to_str
    ==
    """
    1 4 7│· · ·│· · ·
    1 4 7│· · ·│· · ·
    1 4 7│· · ·│· · ·
    ─────┼─────┼─────
    2 5 8│· · ·│· · ·
    2 5 8│· · ·│· · ·
    2 5 8│· · ·│· · ·
    ─────┼─────┼─────
    3 6 9│· · ·│· · ·
    3 6 9│· · ·│· · ·
    3 6 9│· · ·│· · ·
    """

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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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
        |> from_rows
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

box : Board, U64 -> Group
box = |@Board(board), index|
    row_start = (index // 3) * 3
    col_start = (index % 3) * 3
    [0, 1, 2]
    |> List.join_map(
        |r|
            [0, 1, 2]
            |> List.map(
                |c|
                    row(@Board(board), row_start + r) |> Group.get(col_start + c),
            ),
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
        |> from_rows
        |> box(0)
        |> Group.to_str
    result == "3 · ·│· · ·│· · 6"

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
        |> from_rows
        |> box(5)
        |> Group.to_str
    result == "· · 8│· · ·│· · ·"

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
        |> from_rows
        |> box(8)
        |> Group.to_str
    result == "· · ·│· 4 ·│2 · 1"

boxes : Board -> List Group
boxes = |board|
    List.range({ start: At(0), end: Before(board_size), step: 1 })
    |> List.map(|i| box(board, i))

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
