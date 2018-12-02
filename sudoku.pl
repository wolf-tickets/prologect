% CPSC 312 - Project #2 (Prolog)
% Sudoku solver for 25x25 puzzles
% (TODO? extend to  arbitrary square dimensions)
% Two implementations - one using a defined finite domain and constraint logic
% via the clpfd module, and one "brute force" implementation using pure prolog.


% Import constraint logic module for clpfd implementation
:- use_module(library(clpfd)).

% Sample puzzle.
puzzle(1, [[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
           [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]).


% Puzzle is represented by a list of lists, so individual cells are identified
% by (row, column) coordinates; rows are letters, columns are numbers.

% sudoku_clpfd: takes a puzzle and returns solutions using clpfd
sudoku_clpfd(Puzzle) :-
    flatten(Puzzle, Dom),
    Dom ins 1..25,
    Rows = Puzzle,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y],
    subsquare(A, B, C, D, E),
    subsquare(F, G, H, I, J),
    subsquare(K, L, M, N, O),
    subsquare(P, Q, R, S, T),
    subsquare(U, V, W, X, Y),
    maplist(label, Rows).

% sudoku_bf: takes a puzzle and returns solutions using brute-force, pure Prolog
sudoku_bf(Puzzle) :-
    flatten(Puzzle, Dom),
    in_domain(Dom, 1..25),
    Rows = Puzzle,
    maplist(check_distinct, Rows),
    transpose(Rows, Columns),
    maplist(check_distinct, Columns),
    Rows = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y],
    subsquare_bf(A, B, C, D, E),
    subsquare_bf(F, G, H, I, J),
    subsquare_bf(K, L, M, N, O),
    subsquare_bf(P, Q, R, S, T),
    subsquare_bf(U, V, W, X, Y).


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


% subsquare_bf: brute force implementation of subsquare
subsquare_bf([],[],[],[],[]).
subsquare_bf([A1,A2,A3,A4,A5|Ar],
          [B1,B2,B3,B4,B5|Br],
          [C1,C2,C3,C4,C5|Cr],
          [D1,D2,D3,D4,D5|Dr],
          [E1,E2,E3,E4,E5|Er]) :-
                    check_distinct([A1,A2,A3,A4,A5,
                                    B1,B2,B3,B4,B5,
                                    C1,C2,C3,C4,C5,
                                    D1,D2,D3,D4,D5,
                                    E1,E2,E3,E4,E5]),
                    subsquare(Ar,Br,Cr,Dr,Er).


% check_distinct: takes a list and checks that all elements in the list are distinct
check_distinct(Lst) :-
    sort(Lst, Sorted),
    length(Lst, OriginalLength),
    length(Sorted, SortedLength),
    OriginalLength == SortedLength.


% in_domain: checks that all elements of L1 are in L2
in_domain(L1, L2) :- maplist(part_of(L2), L1).


% part_of: inverses the member relationship so it works with maplist in in_domain
part_of(Lst, Elem) :- member(Elem, Lst).
