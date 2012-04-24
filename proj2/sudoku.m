%  File      : sudoku.m
%  Author    : Hengfeng Li
%  Purpose   : This mercury program mainly solve the sudoku puzzle
%   by combining three different strategies to figure out those 
%   unfinished element. The first strategy is to check each element
%   in the puzzle and to fill those unfinished elements who only has
%   one possible answer. The second one is to check row, column, and
%   square seperately to see how many positions can be filled for 
%   each remaining answer. If there is only 1 position for a 
%   unfilled answer, then fill it in that position. The third one 
%   is to guess one possible answer and validates its correctness.

:- module sudoku.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
%  The grouping module is mainly responsible for dividing the 
%  puzzle into row, column and square groups. Also, it provides
%  a method to regroup them. The grouping module is written 
%  by Peter Schachte.
:- import_module list, char, int, grouping.

%  The main entry of the whole program.
%  From: sudoku-starter.m written by Peter Schachte
main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(   Args = [File]
	->  io.see(File, Result, !IO),
	    (   Result = ok,
		sudoku(!IO),
		io.seen(!IO)
	    ;   Result = error(_),
		io.write_string("Could not open puzzle file\n", !IO),
		set_exit_status(1, !IO)
	    )
	;   usage(!IO),
	    set_exit_status(1, !IO)
	).


:- pred usage(io::di, io::uo) is det.

%  Print out a usage message for this program.
%  From: sudoku-starter.m written by Peter Schachte

usage(!IO) :-
	io.write_string("Usage:  sudoku filname\n", !IO),
	io.write_string(
"  where filname is the name of a file containing a sudoku puzzle\n", !IO),
	io.write_string(
"  written as a file of 4, 9, 16, or 25 lines, each with that same\n", !IO),
	io.write_string(
"  number of characters.  All characters on each line must be either\n", !IO),
	io.write_string(
"  a space (for squares to be solved for) or a digit (except 0) or a\n", !IO),
	io.write_string(
"  letter.  A solved puzzle is the same, except that all spaces have\n", !IO),
	io.write_string(
"  been filled in with letters and digits, and each digit or letter\n", !IO),
	io.write_string(
"  between 1 and the width of the puzzle (where 'a' is taken for 10,\n", !IO),
	io.write_string(
"  and so on) appears exactly once in each row, column, and box.\n", !IO).


:- pred sudoku(io::di, io::uo) is cc_multi.	

%  Read in a puzzle from the current input stream, solve it, and print
%  out the result.

sudoku(!IO) :-
	load_puzzle(Puzzle, !IO),
	(   Puzzle = []
	->  io.write_string("Error reading puzzle\n", !IO),
	    set_exit_status(1, !IO)
	;   valid_sudoku_size(length(Puzzle), Size, Boxsize)
	->  
	    % strategy loop invoke three strategies constantly
		% until the puzzle is solved.
		strategyLoop(-1, Puzzle, Size, Boxsize, Solution, !IO),

		print_puzzle(Solution, Size, Boxsize, !IO)

	;   io.write_string("Invalid puzzle size\n", !IO),
	    set_exit_status(1, !IO)
	).


:- pred strategyLoop(int::in, list(int)::in, int::in, int::in,
		list(int)::out, io::di, io::uo ) is cc_multi.	
%  Firstly, it invokes the first strategy and check whether
%  the number of unfilled elements has changed or not. If 
%  it has been changed, then it will continuely invoke the 
%  strategy, otherwise, it will invoke the second strategy. 
%  If the second strategy cannot fill any elements, the 
%  third strategy will be called.

strategyLoop(LastOne, Puzzle, Size, Boxsize, Solution, !IO) :-
	% Invoke the first strategy
	solve_sudoku(Puzzle,Size,Boxsize, Solution1, NumUnfilled1, !IO),
	(
	    % Check whether the number of unfilled elements has 
		% changed or not, comparing with last time.
	    LastOne = NumUnfilled1
	-> 	% If it is not changed, then invoke the second 
	    % strategy.
	    strategy2(Solution1, Size,Boxsize, Solution2, !IO),
		% Get the number of unfilled elements
		num_unfilled(Solution2, NumUnfilled2),
		(
		    NumUnfilled2 = NumUnfilled1
		->  NumUnfilled = -1
		;   NumUnfilled = NumUnfilled2
		),
		Solution4 = Solution2
	;   
		NumUnfilled = NumUnfilled1,
		Solution4 = Solution1
	),
	(
		if NumUnfilled = 0 
		then % If the number of unfilled elements equals 0,
		     % then the program is terminated.
		     Solution = Solution4
		else if NumUnfilled = -1
	 	then % If it equals -1, it means that the second strategy 
		     % doesn't reduce the number of unfilled elements.
			 % Hence, invoke the third strategy.
			 strategy3(Solution4, Size,Boxsize, Solution, !IO)
		else  
		     % Otherwise, call itself again.
			 strategyLoop(NumUnfilled, Solution4,
				 Size, Boxsize,Solution5, !IO),
			 Solution = Solution5
	).

:- pred strategy3(list(int)::in, int::in, int::in, list(int)::out,
	io::di, io::uo) is cc_multi.

% The third strategy will firstly be looking for the 
% position which only has a minimum possible answers.
% And then try each of the possible answers to see
% whether this answer can solve the puzzle successfully.
strategy3(Puzzle, Size, Boxsize, Solution, !IO) :-
	group_list(Size, Size, 1, Puzzle, RowGroup),

	group_list(1, Size, Size, Puzzle, ColumnGroup),

	group_list(Boxsize, Boxsize, Boxsize, Puzzle, SquareGroup),

    % Looking for the position with minimum possible answers.
	minPossibleAnswers(Puzzle,Size,Boxsize, 
			RowGroup, ColumnGroup, SquareGroup,
			Size*Size, 0, Index, PosiAnswers),
	% Guess each answer and check their correctness
	safeGuess(Index, PosiAnswers,Puzzle,Size,Boxsize, Solution, !IO).

:- pred safeGuess(int::in, list(int)::in, list(int)::in,
		int::in, int::in, list(int)::out, io::di, io::uo) is cc_multi.

% Firstly, guess a value and put it into the puzzle, and then
% call the "strategy loop". After get the solution, it checks
% the number of unfilled elements. If the number equals 0, 
% it means that this answer can successfully solve this puzzle.
% Otherwise, try the next possible answers.

safeGuess(_, [], Puzzle, _, _, Puzzle, !IO).
safeGuess(Index, [X|Xs], Puzzle,Size,Boxsize, Solution, !IO) :-
	NewPuzzle = list.replace_nth_det(Puzzle, 
		Index + 1, X),
	strategyLoop(-1, NewPuzzle,Size,Boxsize, Solution1, !IO), 
    num_unfilled(Solution1, NumUnfilled),
	(
	  if   NumUnfilled = 0
	  then Solution = Solution1
	  else safeGuess(Index, Xs, Puzzle, Size,Boxsize, Solution,!IO)
	).

:- pred minPossibleAnswers(list(int)::in,int::in, int::in,
	list(list(int))::in, list(list(int))::in,
	list(list(int))::in, int::in,
	int::in, int::out, list(int)::out) is cc_multi.

% Find out the position which has minimum possible answers.
minPossibleAnswers(Puzzle, Size, Boxsize,  
		RowGroup, ColumnGroup, SquareGroup, 
		Acc, Index, IndexOut, PosiAnswers) :-
	(
	    % If it is out of the index,
		% then return the longest answers.
	    Acc = 0
	->  getSeq(1, Size, PosiAnswers),
	    IndexOut = -1
	; 
		% Get the element from puzzle
		X = list.index0_det(Puzzle, Index),
		(
			X = -1 
		-> 
			% If this element is not filled, then compute its
			% index in row, column, and square.
			getSeq(1, Size, Answers0),
			Row    = Index / Size,
			Column = Index mod Size,
			SquareRow = Row / Boxsize,
			SquareColumn = Column / Boxsize,
			Square = SquareRow * Boxsize + SquareColumn,

			% Filter those impossible answers.
			RowCountry     = list.index0_det(RowGroup, Row),
			ColumnCountry  = list.index0_det(ColumnGroup, Column),
			SquareCountry = list.index0_det(SquareGroup, Square),
			elem_filter(Answers0, RowCountry, Answers1),
			elem_filter(Answers1, ColumnCountry, Answers2),
			elem_filter(Answers2, SquareCountry, Answers),

			% Find the possible answers of the next element 
			minPossibleAnswers(Puzzle, Size,Boxsize,
				RowGroup, ColumnGroup,
				SquareGroup, Acc-1, Index + 1, IndexOut1, PosiAnswers1),
		    (
			    length(Answers) < length(PosiAnswers1)
		    ->  % If the length of the possible answers less than
		        % the minimum length, then record the current 
				% index and possible answers.
			    IndexOut = Index,
			    PosiAnswers = Answers
		    ;
			    % If not, then keep the original one.
				IndexOut = IndexOut1,
				PosiAnswers = PosiAnswers1
			)
		;
			% If this position has been filled, then look for the
			% next one.
			minPossibleAnswers(Puzzle, Size,Boxsize,
				RowGroup, ColumnGroup,
				SquareGroup, Acc-1, Index + 1, IndexOut, PosiAnswers )
		)
	).
	
:- pred strategy2(list(int)::in, int::in, int::in,
		list(int)::out, io::di, io::uo) is cc_multi.

% The second strategy is to check each row, column, and square, in
% which those unfilled answers has a number of possible postions that
% can be filled.

strategy2(Puzzle, Size, Boxsize, Solution, !IO) :-
	group_list(Size, Size, 1, Puzzle, RowGroup),

	group_list(1, Size, Size, Puzzle, ColumnGroup),

	group_list(Boxsize, Boxsize, Boxsize, Puzzle, SquareGroup),

	puzzleCheckInRows(Puzzle, Size, Boxsize, Size,
			0, RowGroup, ColumnGroup, 
			SquareGroup, Puzzle1),

	puzzleCheckInColumns(Puzzle1,Size, Boxsize, Size,
			0, RowGroup, ColumnGroup,
			SquareGroup, Puzzle2),
	
	puzzleCheckInSquares(Puzzle2, Size, Boxsize, Size,
			0, RowGroup, ColumnGroup,
			SquareGroup, Solution).
	
:- pred puzzleCheckInSquares(list(int)::in,
		int::in, int::in, int::in, int::in,list(list(int))::in,
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

% Firstly, find out those unfilled answers. And then, check 
% how many positions in the square can be filled. Finally, 
% fill those answers which only can be filled in a postion.

puzzleCheckInSquares(Puzzle,_,_, 0, _, _, _, _, Puzzle).
puzzleCheckInSquares(Puzzle,Size,Boxsize, Bound,
		Acc, RowGroup, ColumnGroup, SquareGroup, 
		Solution) :-
	% Get the sequence according to the size given,
	% such as, size is 9 and returns [1,2,3,4,5,6,7,8,9]
	getSeq(1, Size, Answers0),
	
	% Get the remaining unfilled answers
	SquareCountry     = list.index0_det(SquareGroup, Acc),
	elem_filter(Answers0, SquareCountry, Answers1),

	SquareIndex = Acc,
    squareCheck(Answers1,Size,Boxsize, SquareIndex, 
			RowGroup, ColumnGroup, SquareGroup, Z),
	
	squareResultCheck(Z, Answers1,Size,Boxsize, SquareIndex, 0, 
			RowGroup, ColumnGroup, SquareGroup, SquareGroup1),
	
	( 
	     SquareGroup1 = SquareGroup
	->   NewPuzzle = Puzzle,
		 RowGroup1 = RowGroup,
		 ColumnGroup1 = ColumnGroup

	;
		 ungroup_list(Boxsize, Boxsize,Boxsize,
			 NewPuzzle, SquareGroup1),

		 group_list(1, Size, Size, NewPuzzle, ColumnGroup1),
		 
		 group_list(Size, Size, 1, NewPuzzle, RowGroup1)

	),
	puzzleCheckInSquares(NewPuzzle,Size,Boxsize,Bound-1, Acc + 1, 
			RowGroup1, ColumnGroup1, SquareGroup1, Solution).

:- pred puzzleCheckInColumns(list(int)::in,
		int::in, int::in, int::in, int::in,list(list(int))::in,
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

puzzleCheckInColumns(Puzzle,_,_,0, _, _, _, _, Puzzle).
puzzleCheckInColumns(Puzzle,Size,Boxsize,Bound,
		Acc, RowGroup, ColumnGroup, SquareGroup, 
		Solution) :-
	getSeq(1, Size, Answers0),

	ColumnCountry     = list.index0_det(ColumnGroup, Acc),
	elem_filter(Answers0, ColumnCountry, Answers1),

	InitialIndex = Acc,
    columnCheck(Answers1,Size,Boxsize,InitialIndex, 
			RowGroup, ColumnGroup, SquareGroup, Z),
	
	columnResultCheck(Z, Answers1,Size,Boxsize,InitialIndex, 0, 
			RowGroup, ColumnGroup, SquareGroup, ColumnGroup1),
	
	( 
	     ColumnGroup1 = ColumnGroup
	->   NewPuzzle = Puzzle,
		 RowGroup1 = RowGroup,
		 SquareGroup1 = SquareGroup

	;
		 ungroup_list(1, Size,Size, NewPuzzle, ColumnGroup1),
		 
		 group_list(Size, Size, 1, NewPuzzle, RowGroup1),

		 group_list(Boxsize, Boxsize,Boxsize, NewPuzzle, SquareGroup1)
	),
	puzzleCheckInColumns(NewPuzzle,Size,Boxsize,Bound-1,Acc + 1, 
			RowGroup1, ColumnGroup1, SquareGroup1, Solution).

:- pred puzzleCheckInRows(list(int)::in,
		int::in, int::in, int::in, int::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

puzzleCheckInRows(Puzzle,_,_,0, _, _, _, _, Puzzle).
puzzleCheckInRows(Puzzle,Size,Boxsize,Bound,
		Acc, RowGroup, ColumnGroup, SquareGroup, 
		Solution) :-
	getSeq(1, Size, Answers0),

	RowCountry     = list.index0_det(RowGroup, Acc),
	elem_filter(Answers0, RowCountry, Answers1),

	InitialIndex = Acc * Size,
    rowCheck(Answers1,Size,Boxsize, InitialIndex, 
			RowGroup, ColumnGroup, SquareGroup, Z),
	
	rowResultCheck(Z, Answers1,Size,Boxsize, InitialIndex, 0, 
			RowGroup, ColumnGroup, SquareGroup, RowGroup1),
	
	( 
	     RowGroup1 = RowGroup
	->   NewPuzzle = Puzzle,
		 ColumnGroup1 = ColumnGroup,
		 SquareGroup1 = SquareGroup
	;
		 ungroup_list(Size, Size, 1, NewPuzzle, RowGroup1),

		 group_list(1, Size, Size, NewPuzzle, ColumnGroup1),

		 group_list(Boxsize, Boxsize,Boxsize, NewPuzzle, SquareGroup1)
	),
	puzzleCheckInRows(NewPuzzle,Size,Boxsize,Bound-1, Acc + 1, 
			RowGroup1, ColumnGroup1, SquareGroup1,Solution).

:- pred squareResultCheck(list(int)::in, list(int)::in,
		int::in, int::in, int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

% Check whether the number of answers's possible positions equals 1.
% If it is, then updates the square.

squareResultCheck([], _,_,_, _, _, _, _, SquareGroup, SquareGroup). 
squareResultCheck([X|Xs], Answers,Size,Boxsize, SquareIndex, Acc, 
		RowGroup, ColumnGroup, 
		SquareGroup, NewSquareGroup) :-
		Y = list.index0_det(Answers, Acc),
		(
		    X = 1
		->	% If the number equals 1, it means that 
		    % the answer can only be place in 1 position. 
			% So it fills the answer to that positon.
		    updateSquare(Y, SquareIndex,Size,Boxsize,Size,0, 
				RowGroup, ColumnGroup, SquareGroup,
				SquareGroup1)
		;
		    % If the number doesn't equals 1, then go next.
			SquareGroup1 = SquareGroup 
		),
		squareResultCheck(Xs,Answers,Size,Boxsize,SquareIndex,Acc+1, 
				RowGroup, ColumnGroup, SquareGroup1, NewSquareGroup).

:- pred updateSquare(int::in, int::in,int::in,int::in,int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

% Updates the square and insert the element to the first
% available position in the square.

updateSquare(_, _,_,_,0, _, _, _, SquareGroup, SquareGroup).
updateSquare(X, Square,Size,Boxsize,Bound, Acc, RowGroup, 
		ColumnGroup, SquareGroup, NewSquareGroup) :-
	% compute the index of the element in square
	SquareRowIndex      = Square / Boxsize,
	SquareColumnIndex   = Square mod Boxsize,
	RowIndexInSquare    = Acc / Boxsize,
	ColumnIndexInSquare = Acc mod Boxsize,
	Row    = SquareRowIndex * Boxsize + RowIndexInSquare,
	Column = SquareColumnIndex * Boxsize + ColumnIndexInSquare,

	SquareCountry    = list.index0_det(SquareGroup, Square),
	Element = list.index0_det(SquareCountry, Acc),
	( 
	    Element = -1
	->	RowCountry = list.index0_det(RowGroup, Row),
		ColumnCountry = list.index0_det(ColumnGroup, Column),
	
		(
			list.member(X, RowCountry)
		->  SquareGroup1 = SquareGroup
		; 
			(  list.member(X, ColumnCountry)
			-> SquareGroup1 = SquareGroup 
			;  % If this position is available for this answer,
			   % then change it and retuan a new list of squares.
			   SquareCountry1 = list.replace_nth_det(SquareCountry, 
					Acc + 1, X),
			   SquareGroup1 = list.replace_nth_det(SquareGroup,
				    Square + 1, SquareCountry1)
			)
		),
		updateSquare(X, Square,Size,Boxsize,Bound-1,Acc+1, RowGroup, 
			ColumnGroup, SquareGroup1, NewSquareGroup) 
	;   SquareGroup1 = SquareGroup,
		updateSquare(X, Square,Size,Boxsize,Bound-1,Acc+1, RowGroup, 
			ColumnGroup, SquareGroup1, NewSquareGroup) 
	).

:- pred columnResultCheck(list(int)::in,list(int)::in,
		int::in,int::in, int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

columnResultCheck([], _, _, _, _, _, _, ColumnGroup, _, ColumnGroup). 
columnResultCheck([X|Xs], Answers,Size,Boxsize,InitialIndex, Acc, 
		RowGroup, ColumnGroup, 
		SquareGroup, NewColumnGroup) :-
		Y = list.index0_det(Answers, Acc),
		(
		    X = 1
		->	updateColumn(Y, InitialIndex,Size,Boxsize,Size, 0, 
				RowGroup, ColumnGroup, SquareGroup,
				ColumnGroup1)
		;
			ColumnGroup1 = ColumnGroup 
		),
		columnResultCheck(Xs,Answers,Size,Boxsize,InitialIndex,Acc+1, 
				RowGroup, ColumnGroup1, SquareGroup, NewColumnGroup).

:- pred updateColumn(int::in, int::in,int::in,int::in,int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

updateColumn(_, _,_,_,0, _, _, ColumnGroup, _, ColumnGroup).
updateColumn(X, InitialIndex,Size,Boxsize,Bound, Acc, RowGroup, 
		ColumnGroup, SquareGroup, NewColumnGroup) :-
	Index  = InitialIndex + Acc * Size,
	Row    = Index / Size,
	Column = Index mod Size,
	SquareRow = Row / Boxsize,
	SquareColumn = Column / Boxsize,
	Square = SquareRow * Boxsize + SquareColumn,

	ColumnCountry    = list.index0_det(ColumnGroup, Column),
	Element = list.index0_det(ColumnCountry, Row),
	( 
	    Element = -1
	->	RowCountry = list.index0_det(RowGroup, Row),
		SquareCountry = list.index0_det(SquareGroup, Square),
	
		(
			list.member(X, RowCountry)
		->  ColumnGroup1 = ColumnGroup
		; 
			(  list.member(X, SquareCountry)
			-> ColumnGroup1 = ColumnGroup 
			;  ColumnCountry1 = list.replace_nth_det(ColumnCountry, 
					Row + 1, X),
			   ColumnGroup1 = list.replace_nth_det(ColumnGroup,
				    Column + 1, ColumnCountry1)
			)
		),
		updateColumn(X, InitialIndex,
			Size,Boxsize,Bound-1,Acc+1,RowGroup, 
			ColumnGroup1, SquareGroup, NewColumnGroup) 
	;   ColumnGroup1 = ColumnGroup,
		updateColumn(X, InitialIndex,
			Size,Boxsize,Bound-1, Acc + 1, RowGroup, 
			ColumnGroup1, SquareGroup, NewColumnGroup) 
	).

:- pred rowResultCheck(list(int)::in, list(int)::in,
		int::in,int::in,int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

rowResultCheck([], _, _,_,_, _, RowGroup, _, _, RowGroup). 
rowResultCheck([X|Xs], Answers,Size,Boxsize, InitialIndex, Acc, 
		RowGroup, ColumnGroup, 
		SquareGroup, NewRowGroup) :-
		Y = list.index0_det(Answers, Acc),
		(
		    X = 1
		->	updateRow(Y, InitialIndex,Size,Boxsize,Size, 0, 
				RowGroup, ColumnGroup, SquareGroup,
				RowGroup1)
		;
			RowGroup1 = RowGroup 
		),
		rowResultCheck(Xs,Answers,Size,Boxsize,InitialIndex,Acc+1, 
				RowGroup1, ColumnGroup, SquareGroup, NewRowGroup).

:- pred updateRow(int::in, int::in,int::in,int::in,int::in,int::in,
		list(list(int))::in, list(list(int))::in,
		list(list(int))::in, list(list(int))::out) is cc_multi.

updateRow(_, _,_,_,0, _, RowGroup, _, _, RowGroup).
updateRow(X, InitialIndex,Size,Boxsize,Bound, Acc, RowGroup, 
		ColumnGroup, SquareGroup, NewRowGroup) :-
	Index  = InitialIndex + Acc,
	Row    = Index / Size,
	Column = Index mod Size,
	SquareRow = Row / Boxsize,
	SquareColumn = Column / Boxsize,
	Square = SquareRow * Boxsize + SquareColumn,

	RowCountry    = list.index0_det(RowGroup, Row),
	Element = list.index0_det(RowCountry, Column),
	( 
	    Element = -1
	->	ColumnCountry = list.index0_det(ColumnGroup, Column),
		SquareCountry = list.index0_det(SquareGroup, Square),
	
		(
			list.member(X, ColumnCountry)
		->  RowGroup1 = RowGroup
		; 
			(  list.member(X, SquareCountry)
			-> RowGroup1 = RowGroup 
			;  RowCountry1 = list.replace_nth_det(RowCountry, 
					Column + 1, X),
			   RowGroup1 = list.replace_nth_det(RowGroup,
				    Row + 1, RowCountry1)
			)
		),
		updateRow(X, InitialIndex,
			Size,Boxsize,Bound-1, Acc + 1, RowGroup1, 
			ColumnGroup, SquareGroup, NewRowGroup) 
	;   RowGroup1 = RowGroup,
		updateRow(X, InitialIndex,
			Size,Boxsize,Bound-1, Acc + 1, RowGroup1, 
			ColumnGroup, SquareGroup, NewRowGroup) 
	).

:- pred squareCheck(list(int)::in,int::in,int::in,
		int::in, list(list(int))::in, 
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

% According to the list of remaining answers, it finds out 
% how many positions each answer can be placed.

squareCheck([],_,_, _, _, _,_, []).
squareCheck([X|Xs],Size,Boxsize,
		SquareIndex, RowGroup, ColumnGroup, SquareGroup,
		[Z|Zs]) :-
	% Count the number of postions for each possible answer
	numSquareCells(X, SquareIndex,Size,Boxsize,Size, 0, 
			RowGroup, ColumnGroup, SquareGroup, Z),
	squareCheck(Xs,Size,Boxsize, 
			SquareIndex, RowGroup, ColumnGroup, SquareGroup,
			Zs).

:- pred numSquareCells(int::in,int::in,
		int::in,int::in,int::in,int::in,
		list(list(int))::in, list(list(int))::in, 
		list(list(int))::in ,int::out) is cc_multi.

% Compute how many positions that a possible answer can be place.

numSquareCells(_, _,_,_,0, _,_, _, _, 0 ).
numSquareCells(X, Square,Size,Boxsize,Bound, Acc, RowGroup, 
		ColumnGroup, SquareGroup, NumCells) :- 
	% Compute the index of the element in square
	SquareRowIndex      = Square / Boxsize,
	SquareColumnIndex   = Square mod Boxsize,
	RowIndexInSquare    = Acc / Boxsize,
	ColumnIndexInSquare = Acc mod Boxsize,
	Row    = SquareRowIndex * Boxsize + RowIndexInSquare,
	Column = SquareColumnIndex * Boxsize + ColumnIndexInSquare,

	% Get the position in the square and check it whether 
	% is filled or not. If it is not filled, then see 
	% whether the answer can be filled in this position.
	SquareCountry    = list.index0_det(SquareGroup, Square),
	Element = list.index0_det(SquareCountry, Acc),
	( 
	    Element = -1
	->	RowCountry = list.index0_det(RowGroup, Row),
		ColumnCountry = list.index0_det(ColumnGroup, Column),
	
		Acc1 = Acc + 1,
		% Call itself resursively and get the number of positons 
		% from sublist
		numSquareCells(X, Square,Size,Boxsize,Bound-1, Acc1, RowGroup,
			ColumnGroup, SquareGroup, NumCells1),
		(
			list.member(X, RowCountry)
		->  NumCells = NumCells1
		; 
			(  list.member(X, ColumnCountry)
			-> NumCells = NumCells1
			;  NumCells = NumCells1 + 1
			)
		)
	;   
		Acc1 = Acc + 1,
		numSquareCells(X, Square, Size,Boxsize,Bound-1,
			Acc1, RowGroup, ColumnGroup, SquareGroup, NumCells1),
		NumCells = NumCells1
	).

:- pred columnCheck(list(int)::in,int::in,int::in,
		int::in, list(list(int))::in, 
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

% Check a column's possible answers that how many possible
% positions each answer can be placed.

columnCheck([], _,_,_, _, _,_, []).
columnCheck([X|Xs],Size,Boxsize,
		InitialIndex, RowGroup, ColumnGroup, SquareGroup,
		[Z|Zs]) :-
	numColumnCells(X, InitialIndex,Size,Boxsize,Size, 0, 
			RowGroup, ColumnGroup, SquareGroup, Z),
	columnCheck(Xs,Size,Boxsize,
			InitialIndex,RowGroup,ColumnGroup, SquareGroup,
			Zs).

:- pred numColumnCells(int::in, int::in,
		int::in,int::in,int::in, int::in,
		list(list(int))::in, list(list(int))::in, 
		list(list(int))::in ,int::out) is cc_multi.

% Compute how many positions a answer can be placed in a column.

numColumnCells(_, _,_,_,0, _,_, _, _, 0 ).
numColumnCells(X, InitialIndex,Size,Boxsize,Bound,Acc, RowGroup, 
		ColumnGroup, SquareGroup, NumCells) :- 
	Index  = InitialIndex + Acc * Size,
	Row    = Index / Size,
	Column = Index mod Size,
	SquareRow = Row / Boxsize,
	SquareColumn = Column / Boxsize,
	Square = SquareRow * Boxsize + SquareColumn,

	ColumnCountry    = list.index0_det(ColumnGroup, Column),
	Element = list.index0_det(ColumnCountry, Row),
	( 
	    Element = -1
	->	RowCountry = list.index0_det(RowGroup, Row),
		SquareCountry = list.index0_det(SquareGroup, Square),
	
		Acc1 = Acc + 1,
		numColumnCells(X, InitialIndex,
			Size,Boxsize,Bound-1, Acc1, RowGroup,
			ColumnGroup, SquareGroup, NumCells1),
		(
			list.member(X, RowCountry)
		->  NumCells = NumCells1
		; 
			(  list.member(X, SquareCountry)
			-> NumCells = NumCells1
			;  NumCells = NumCells1 + 1
			)
		)
	;   
		Acc1 = Acc + 1,
		numColumnCells(X, InitialIndex, Size,Boxsize,Bound-1,
			Acc1, RowGroup, ColumnGroup, SquareGroup, NumCells1),
		NumCells = NumCells1
	).

:- pred rowCheck(list(int)::in,
		int::in,int::in, int::in, list(list(int))::in, 
		list(list(int))::in, list(list(int))::in, 
		list(int)::out) is cc_multi.

% Check a row's possible answers that how many possible
% positions each answer can be placed.

rowCheck([],_,_, _, _, _,_, []).
rowCheck([X|Xs],Size,Boxsize,
		InitialIndex, RowGroup, ColumnGroup, SquareGroup,
		[Z|Zs]) :-
	numCells(X, InitialIndex,
			Size,Boxsize,Size, 0, RowGroup, ColumnGroup, SquareGroup,
			Z),
	rowCheck(Xs,Size,Boxsize,
			InitialIndex, RowGroup, ColumnGroup, SquareGroup,
			Zs).

:- pred numCells(int::in, int::in,
		int::in,int::in,int::in, int::in,
		list(list(int))::in, list(list(int))::in, 
		list(list(int))::in ,int::out) is cc_multi.

% Compute how many positions a answer can be placed in a row.

numCells(_, _,_,_,0, _,_, _, _, 0 ).
numCells(X, InitialIndex,Size,Boxsize,Bound, Acc, RowGroup, 
		ColumnGroup, SquareGroup, NumCells) :- 
	Index  = InitialIndex + Acc,
	Row    = Index / Size,
	Column = Index mod Size,
	SquareRow = Row / Boxsize,
	SquareColumn = Column / Boxsize,
	Square = SquareRow * Boxsize + SquareColumn,

	RowCountry    = list.index0_det(RowGroup, Row),
	Element = list.index0_det(RowCountry, Column),
	( 
	    Element = -1
	->	ColumnCountry = list.index0_det(ColumnGroup, Column),
		SquareCountry = list.index0_det(SquareGroup, Square),
	
		Acc1 = Acc + 1,
	
		numCells(X, InitialIndex,Size,Boxsize,Bound-1,Acc1, RowGroup,
			ColumnGroup, SquareGroup, NumCells1),
		(
			list.member(X, ColumnCountry)
		->  NumCells = NumCells1
		; 
			(  list.member(X, SquareCountry)
			-> NumCells = NumCells1
			;  NumCells = NumCells1 + 1
			)
		)
	;   Acc1 = Acc + 1,
		numCells(X, InitialIndex, Size,Boxsize,Bound-1,
			Acc1, RowGroup, ColumnGroup, SquareGroup, NumCells1),
		NumCells = NumCells1
	).


:- pred solve_sudoku(list(int)::in,int::in,int::in, list(int)::out,
		int::out, io::di, io::uo) is cc_multi.

% Invoke the first strategy to solve the problem.

solve_sudoku(Puzzle,Size,Boxsize, Solution, NumUnfilled, !IO) :-
	group_list(Size, Size, 1, Puzzle, RowGroup),

	group_list(1, Size, Size, Puzzle, ColumnGroup),

	group_list(Boxsize, Boxsize,Boxsize, Puzzle, SquareGroup),

	strategy1(Puzzle,Size,Boxsize, RowGroup, ColumnGroup, 
			SquareGroup, 0, NumUnfilled, Solution0, !IO),
	Solution = Solution0.

:- pred elem_filter(list(int)::in,
		list(int)::in, list(int)::out) is det.

% Filter those elements which has been appeared in the list.

elem_filter([],_,[]).
elem_filter([X|Xs], Country, AnswersOut) :-
	elem_filter(Xs, Country, AnswersOut1),
	( 
	    list.member(X, Country)
	 -> AnswersOut = AnswersOut1
	 ; 	AnswersOut = [X|AnswersOut1]							
	).

:- pred strategy1(list(int)::in,int::in,int::in,list(list(int))::in, 
	list(list(int))::in, list(list(int))::in,
	int::in,int::out, list(int)::out, io::di, io::uo) is cc_multi.

% The first strategy is to check each position of the puzzle and 
% see how many possible answers a position can be placed.

strategy1([],_,_,_,_,_,_,0,[],!IO).
strategy1([X|Xs],Size,Boxsize,RowGroup, ColumnGroup,
		SquareGroup, Index, NumUnfilled, [Answer|Zs], !IO) :-
	(
	 X = -1 
	 -> 
	    % Get the initial possible answers
		getSeq(1, Size, Answers0),
		% Compute the index in row, column, and square
		Row    = Index / Size,
		Column = Index mod Size,
		SquareRow = Row / Boxsize,
		SquareColumn = Column / Boxsize,
		Square = SquareRow * Boxsize + SquareColumn,

		% Fileter those impossible answers.
		RowCountry     = list.index0_det(RowGroup, Row),
		ColumnCountry  = list.index0_det(ColumnGroup, Column),
		SquareCountry = list.index0_det(SquareGroup, Square),
		elem_filter(Answers0, RowCountry, Answers1),
		elem_filter(Answers1, ColumnCountry, Answers2),
		elem_filter(Answers2, SquareCountry, Answers),

		(
		    length(Answers) = 1
		->  % If there is only 1 possible answer,
		    % then fill it into the puzzle.
		    Answer = list.det_head(Answers),
			ungroup_list(Size, Size, 1, Puzzle, RowGroup),
			NewPuzzle = list.replace_nth_det(Puzzle, 
				Index + 1, Answer),

			% Updates the groups so that the next step will 
			% get the real-time infomation about the puzzle.
			group_list(Size, Size, 1, NewPuzzle, RowGroup1),

			group_list(1, Size, Size, NewPuzzle, ColumnGroup1),

			group_list(Boxsize,Boxsize,Boxsize,NewPuzzle,SquareGroup1)

		;   Answer = -1,
			RowGroup1    = RowGroup,
			ColumnGroup1 = ColumnGroup,
			SquareGroup1 = SquareGroup
		),
		% Go to the next position
		strategy1(Xs,Size,Boxsize, RowGroup1, ColumnGroup1,
			SquareGroup1, Index + 1, NumUnfilled1, Zs, !IO),
		(
		    Answer = -1 
		->  NumUnfilled = NumUnfilled1 + 1
		;   NumUnfilled = NumUnfilled1
		)
	;
	    % If the position has been filled, then skip to next step.
		Answer = X, 
		strategy1(Xs,Size,Boxsize, RowGroup, ColumnGroup,
			SquareGroup, Index + 1, NumUnfilled1, Zs, !IO),
		NumUnfilled = NumUnfilled1
	).


:- pred num_unfilled(list(int)::in, int::out) is multi.

% Check a puzzle to see how many unfilled positions it has.

num_unfilled([],0).
num_unfilled([X|Xs],N) :-
	num_unfilled(Xs,N1), 
	( X = -1 -> N = N1 + 1 ; N = N1 ).

:- pred print_lists(list(list(int))::in, io::di, io::uo) is det.

% print out a list of lists

print_lists([], !IO). 
print_lists([X|Xs], !IO) :-
	print_list(X, !IO),
	nl(!IO),
	print_lists(Xs, !IO).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

% print out a single list

print_list([], !IO).
print_list([Int|Xs], !IO) :-
	(
	 int_to_digit(Int, Digit)
	 ->  write_char(Digit, !IO)
	 ;   write_char('.', !IO)
	),
	print_list(Xs, !IO).

:- pred valid_sudoku_size(int::in, int::out, int::out) is semidet.

%  valid_sudoku_size(Size, Width, Boxsize)
%  holds if Width x Width is a valid size for a Sudoku puzzle, Size is
%  Width x Width, and Boxsize is the square root of Width.

valid_sudoku_size( 16, 4, 2).
valid_sudoku_size( 81, 9, 3).
valid_sudoku_size(256, 16, 4).
valid_sudoku_size(625, 25, 5).
% We could go bigger, but we wouldn't have enough letters to handle
% input and output of the puzzles.  We'd have to use a different
% file format.


:- pred load_puzzle(list(int)::out, io::di, io::uo) is det.

%  Read in a Sudoku puzzle from the current input stream and return
%  it.  We ignore the way the input is broken into lines and simply
%  return the content as a flat list of values, with -1 used for
%  un-filled cells.
%  From: sudoku-starter.m written by Peter Schachte

load_puzzle(Puzzle, !IO) :-
	io.read_char(Result, !IO),
	(   ( Result = eof ; Result = error(_) ),
	    Puzzle = []
	;   Result = ok(Char),
	    (   char.digit_to_int(Char, Int)
	    ->  Puzzle = [Int|Puzzle1]
	    ;   % for flexibility, we accept spaces, underscores and
		% periods as indicating an unfilled cell.
		( Char = ' ' ; Char = ('.') ; Char = '_' )
	    ->  Puzzle = [-1|Puzzle1]
	    ;   Puzzle = Puzzle1
	    ),
	    load_puzzle(Puzzle1, !IO)
	).

:- pred print_puzzle(list(int)::in, int::in, int::in,
		     io::di, io::uo) is det.

%  print_puzzle(Puzzle, Size, Boxsize, !IO)
%  Print out a (possibly partially filled) sudoku puzzle
%  From: sudoku-starter.m written by Peter Schachte

print_puzzle(Puzzle, Size, Boxsize, !IO) :-
	print_hbar(Size, Boxsize, !IO),
	(   Puzzle = []
	->  true
	;   print_chunk(Puzzle, Puzzle1, Boxsize, Size, Boxsize, !IO),
	    print_puzzle(Puzzle1, Size, Boxsize, !IO)
	).


:- pred print_chunk(list(int)::in, list(int)::out,
		    int::in, int::in, int::in, io::di, io::uo) is det.

%  Print out one Boxsize-height chunk of the given puzzle.
%  From: sudoku-starter.m written by Peter Schachte

print_chunk(!Puzzle, Rowsleft, Size, Boxsize, !IO) :-
	(   Rowsleft = 0
	->  true
	;   print_row(!Puzzle, Size, Boxsize, !IO),
	    print_chunk(!Puzzle, Rowsleft-1, Size, Boxsize, !IO)
	).


:- pred print_row(list(int)::in, list(int)::out,
		    int::in, int::in, io::di, io::uo) is det.

%  Print out one row of the given puzzle.
%  From: sudoku-starter.m written by Peter Schachte

print_row(!Puzzle, Remaining, Boxsize, !IO) :-
	(   0 = Remaining mod Boxsize
	->  write_char('|', !IO)
	;   true
	),
	(   Remaining = 0
	->  nl(!IO)
	;   !.Puzzle = [Int|!:Puzzle]
	->  (   int_to_digit(Int, Digit)
	    ->  write_char(Digit, !IO)
	    ;   write_char('.', !IO)
	    ),
	    print_row(!Puzzle, Remaining-1, Boxsize, !IO)
	;   nl(!IO)
	).

:- pred print_hbar(int::in, int::in, io::di, io::uo) is det.

%  print_hbar(Width, Boxsize, !IO)
%  Print out a horizontal bar of Width '-' characters punctuated by a '+'
%  every Boxsize characters and beginning and ending with a '+'.
%  From: sudoku-starter.m written by Peter Schachte

print_hbar(Remaining, Boxsize, !IO) :-
	(   0 = Remaining mod Boxsize
	->  write_char('+', !IO)
	;   true
	),
	(   Remaining = 0
	->  nl(!IO)
	;   write_char('-', !IO),
	    print_hbar(Remaining-1, Boxsize, !IO)
	).

:- pred getSeq(int::in, int::in, list(int)::out) is cc_multi.

% Get the sequence number according to the given range and size.

getSeq(_, 0, []).
getSeq(Acc, Size, [Acc|List]) :-
	getSeq(Acc +1 , Size - 1, List).

