% CPSC 312 - Project #2 (Prolog)
% Sudoku solver for 25x25 puzzles
% (TODO? extend to  arbitrary square dimensions)
% Two implementations - one using a defined finite domain and constraint logic
% via the clpfd module, and one "brute force" implementation using pure prolog.


% Import constraint logic module for clpfd implementation
:- use_module(library(clpfd)).

% Sample puzzle.
puzzle(1, [[_,_,_,_,_,_,_,_,5,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]).


% Puzzle is represented by a list of lists, so individual cells are identified
% by (row, column) coordinates; rows are letters, columns are numbers.

% sudoku_clpfd: takes a puzzle and returns solutions using clpfd
sudoku_clpfd(Rows) :-
    flatten(Rows, Dom),
    Dom ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y],
    subsquare(A, B, C, D, E),
    subsquare(F, G, H, I, J),
    subsquare(K, L, M, N, O),
    subsquare(P, Q, R, S, T),
    subsquare(U, V, W, X, Y).

% sudoku_bf: takes a puzzle and returns solutions using brute-force, pure Prolog
% sudoku_bf(Puzzle) :-



% subsquare: takes one puzzle, returns a number of subsquares equal to the
% dimensions of the puzzle.
subsquare([],[],[],[],[]).
subsquare([A1,A2,A3,A4,A5|Ar],
          [B1,B2,B3,B4,B5|Br],
          [C1,C2,C3,C4,C5|Cr],
          [D1,D2,D3,D4,D5|Dr],
          [E1,E2,E3,E4,E5|Er]) :-
                    all_distinct([A1,A2,A3,A4,A5,
                                 B1,B2,B3,B4,B5,
                                 C1,C2,C3,C4,C5,
                                 D1,D2,D3,D4,D5,
                                 E1,E2,E3,E4,E5]),
                    subsquare(Ar,Br,Cr,Dr,Er).



% sect-unique: checks that all members of a section (row/column/subsquare) are
% unique integers within the defined domain (i.e. 0 < value < 26)
%sect_unique(Section) :-
%    all_distinct(Section).
