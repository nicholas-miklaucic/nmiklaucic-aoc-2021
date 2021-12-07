#!/usr/bin/env python3

with open('input', 'r') as infile:
    lines = [x.strip() for x in infile.readlines()]
    nums = [int(x) for x in lines[0].split(',')]

    square_lines = [l for l in lines[1:] if l != '']
    squares = []
    square_fills = []
    for i in range(0, len(square_lines), 5):
        square = [[int(x) for x in square_lines[i + j].split()] for j in range(5)]
        squares.append(square)

        square_fills.append([[False for _ in range(5)] for _ in range(5)])


def part1():
    for num in nums:
        for square, fill in zip(squares, square_fills):
            for i in range(5):
                for j in range(5):
                    if square[i][j] == num:
                        fill[i][j] = True
                        if all(fill[i]) or all([fill[y][j] for y in range(5)]):
                            unmarked = 0
                            for x in range(5):
                                for y in range(5):
                                    if not fill[x][y]:
                                        unmarked += square[x][y]
                            print('Part 1:', unmarked * num)
                            return unmarked * num


part1()


def part2():
    board_won = [False for _ in range(len(squares))]
    for num in nums:
        for square_i, (square, fill) in enumerate(zip(squares, square_fills)):
            for i in range(5):
                for j in range(5):
                    if square[i][j] == num:
                        fill[i][j] = True
                        if all(fill[i]) or all([fill[y][j] for y in range(5)]):
                            board_won[square_i] = True
                            unmarked = 0
                            for x in range(5):
                                for y in range(5):
                                    if not fill[x][y]:
                                        unmarked += square[x][y]

                            if all(board_won):
                                print('Part 2:', unmarked * num)
                                return unmarked * num


part2()
