




grid([      [.,.,.,.,.,.], 
            [.,.,1,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

testGrid([  [a,b,c],
            [d,e,f] ]).

/**
score(State,Player, PlayerScore) :-
    Counter is 0,
    iterMatrix(State, I, J, Value),
    checkVal(Value,Player,Counter,NextCount, NewScore),
    incrementScore(NextCount, NewScore, PlayerScore),
    write(PlayerScore),fail.
*/
/**
score(State,Player, PlayerScore) :-
    %iterMatrix(State, I, J, Value),
    iterat(State,Player, 0, PlayerScore).
    %checkVal(Value,Player,Counter,NextCount, NewScore),
    %incrementScore(NextCount, NewScore, PlayerScore),
    %write(PlayerScore),fail.

iterat(State,Player,Counter,TotalScore) :-
    iterMatrix(State, I, J, Value),
    checkVal(Value, Player, Counter, NextCount, NewScore),
    incrementScore(NextCount, NewScore, TotalScore),
    incrementVar(NextCount, Counter),fail.

iterMatrix(State,I,J, Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value),
    nl,
    write(Value).


checkVal('.',_,_, 0).
checkVal(Value, Player,_, 0) :- Value \= Player.
checkVal(Value, Player, Counter, NextCount, 1) :- 
    Value = Player,
    nl, write('Value:'),
    write(Value),
    incrementVar(Counter, NextCount),
    nl, write('NextCount:'),
    write(NextCount).

incrementVar(X, X1) :- X1 is X+1.

incrementScore(Counter,Value, FinalScore) :- 
    FinalScore is Counter+Value.


%retry but with lists
winner(State,Plyr) :-
	%terminal(State),!, %if terminal state is true, cant backtrack since its always true
	getScore(State, 1, Plyr1Score),!,
    getScore(State, 2, Plyr2Score),!,
	Plyr1Score =\= Plyr2Score,
	((Plyr1Score < Plyr2Score) ->  Plyr = 2 ; Plyr = 1).%if then else predicate

getScore(State, Player, PlayerScore) :-
    calcPlayerScore(State, Player, ScoreList),!,
    length(ScoreList, PlayerScore),!.

calcPlayerScore(State, Player, Score) :- 
    calcPlayerScore(State, Player, Score, 0,0).

calcPlayerScore(_, _, [], Row, Column) :-
    Row >5,
    Column >5.
calcPlayerScore(State,Player, Score,Row, Column) :-
    Column >5, % need to reset in this basecase
    calcPlayerScore(State, Player, Score, Row+1, 0),!. %no backtracking from this stage.
calcPlayerScore(State, Player, Score, I, J) :-
    Row is I,
    Column is J,
    (iterMatrix(State, [Row, Column], Player) -> Score = [[Row, Column]|NextStone] ; Score = NextStone),
    calcPlayerScore(State, Player, NextStone, I, J+1),!. %stop backtrack.

iterMatrix(State, [I,J], Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value).

*/
/**

getScore(State, Player1Score,Player2Score) :-
	score(State,1,Player1Score),!,
	score(State,2,Player2Score),!,
    write(Player1Score),
    write(Player2Score).
	%Plyr1 is Player1Score,
	%Plyr2 is Player2Score.

%Check amount of stones on the board for player

score(State,Player, PlayerScore) :-
    Counter is 0,
    iterMatrix(State, I, J, Value),
    checkVal(Value,Player,Counter,NextCount, NewScore),
    incrementScore(NextCount, NewScore, PlayerScore),!.

%iterate function based on https://stackoverflow.com/questions/34949724/prolog-iterate-through-matrix
iterMatrix(State,I,J, Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value).

checkVal('.',_,_, 0).
checkVal(Value, Player,_, 0) :- Value \= Player.
checkVal(Value, Player, Counter, NextCount, 1) :- 
    Value = Player, 
    incrementVar(Counter, NextCount).

incrementVar(X, X1) :- X1 is X+1.

incrementScore(Counter,Value, FinalScore) :- 
    FinalScore is Counter+Value.
*/

% VARFÖR HAR IF THEN ELSE INTE INTRODUCERATS TIDIGARE!?

%ite(Number) :-
%    Number = 1 -> write('ett') ; write('nope').
/**
directions( [[-1,1],[0,1],[1,1], [-1,0],[1,0], [-1,-1],[0,-1],[1,-1]] ).

nextDirection([], []).
nextDirection([CurrentPosition|NextPosition], NextPosition).

trav([]).
trav([Dir|Next], Dir, Next).
    %write(Dir),nl,nl,
    %trav(Next).
    %nextDirection(Dir, Next),
printer([]).
printer(Dir) :-
    trav(Dir, Current, Next),
    write(Current),nl,nl,
    printer(Next).
*/
%-------------------------------------------------------------------

calcPlayerScore(State, Player, Score) :- 
    calcPlayerScore(State, Player, Score, 0,0).

calcPlayerScore(_, _, [], Row, Column) :-
    Row >5,
    Column >5.
calcPlayerScore(State,Player, Score,Row, Column) :-
    Column >5, % need to reset in this basecase
    calcPlayerScore(State, Player, Score, Row+1, 0),!. %no backtracking from this stage.
calcPlayerScore(State, Player, Score, I, J) :-
    Row is I,
    Column is J,
    (iterMatrix(State, [Row, Column], Player) -> Score = [[Row, Column]|NextStone] ; Score = NextStone),
    calcPlayerScore(State, Player, NextStone, I, J+1),!. %stop backtrack.

%iterate function based on https://stackoverflow.com/questions/34949724/prolog-iterate-through-matrix
iterMatrix(State,[I,J], Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value).


terminal(State) :-
	% i.e. true if there are no more moves available. Ahh that is if only pass moves are still avail.
	checkMoves(State, 1, Empty1),!,
	checkMoves(State, 2, Empty2),!,
	Empty1 = [pass],
	Empty2 = [pass].



checkMoves(State,Player, MoveList) :-
	calcPlayerScore(State, Player, StoneList),
	checkMoves(State, Player,StoneList, AllMoves), % stone list is coordinate of player stone for example [2,2] 
	(AllMoves = [] -> MoveList = [pass] ; sort(AllMoves, MoveList)). % IF allMoves are not empty THEN sort allmoves to moveList  ELSE set Movelist to contain pass move only

checkMoves(State, Player, [DxDy|Positions],MoveList) :-
	%need to find the opposite player, then find possible moves.
	getOtherPlayer(Player, OtherPlayer),
	checkMoves(State, Player, Positions, PossibleMoves),!, %get the moves 
	(getAllMoves(State, Player, OtherPlayer, DxDy, Moves) -> append(Moves, PossibleMoves, MoveList) ; MoveList = PossibleMoves).

checkMoves(_,_,[],[]).
% How to define this without a lot of pattern matching? DxDy traversal in a grid?
%
%directions: [1, 2 ,3],
%			[4,pos,5],
%			[6, 7 ,8].
%x and y from -1 to 1.
%
directions( [   [-1,1],
                [0,1],
                [1,1],
                [-1,0],
                [1,0],
                [-1,-1],
                [0,-1],
                [1,-1]  ] ).
%take current coordinate and give next, only need to pick next in list of directions until empty
nextDirection([]).
nextDirection([Current|NextPosition], Current, NextPosition).

addLists([],[],[]).
addLists([H1|T1], [H2|T2], [NewList|NewTail]) :-
    addLists(T1, T2, NewTail), NewList is H1+H2.
%getCoordinates([X|Y], X, Y).

getCoordinate(CoordList,C, Out) :-
    nth0(C, CoordList, Out).

getAllMoves(State,Player, OtherPlayer,StoneList, Moves) :-
    directions(C_List),
    getAllMoves(State, Player, OtherPlayer, StoneList, Moves, C_List).

getAllMoves(State, Player, OtherPlayer, StoneList, Moves, [FirstCheck|Rest]) :-
    getAllMoves(State, Player, OtherPlayer, StoneList, Next_Moves, Rest),
    (getAMove(State,Player,OtherPlayer, StoneList, FirstCheck, Player, ThisMove) ->
        Moves = [ThisMove|Next_Moves] 
    ; 
        Moves = Next_Moves).

getAllMoves(_,_,_,_,[],[]). %base

getAMove(State, Player, OtherPlayer, [Cx, Cy], [Nx,Ny], P, ThisMove) :-
    addLists([Cx, Cy], [Nx, Ny], NewCoordinates),
    %getCoordinates(NewCoordinates, X, Y),
    getCoordinate(NewCoordinates,0,X),
    getCoordinate(NewCoordinates,1,Y),
    write(X),
    write(Y),
    iterMatrix(State, [X,Y], Square),
    \+(Square = Player),
    ( 
        (Square = OtherPlayer),
        getAMove(State, Player, OtherPlayer, [X, Y], [Nx,Ny], OtherPlayer, ThisMove)
    ; 
        (Square = ., P = OtherPlayer),
         
        ThisMove = [X,Y]
    ). %check other player stone in "the way".

getOtherPlayer(Me, Other) :-
	Me = 1 -> Other is 2 ; Other is 1.


%---------------------------------------------------------------------

%getCoordinate(CoordList,C, Out) :-
%    nth0(C, CoordList, Out).

%list_sum([],[],[]).
%list_sum([H1|T1],[H2|T2],[X|L3]):-list_sum(T1,T2,L3), X is H1+H2.
%addLists([],[],[]).
%addLists([H1|T1], [H2|T2], [NewList|NewTail]) :-
%    addLists(T1, T2, NewTail), NewList is H1+H2.

/**
getAllMoves(State, Player, OtherPlayer, DxDy, Moves) :-
	directions(Directions),
	getAllMoves(State, Player, OtherPlayer, DxDy, Moves, Directions). %pass list of all directions

getAllMoves(State, Player, OtherPlayer, DxDy, Moves, [H|Dir]):- %[H|T] , [H|Dir]
	getAllMoves(State, Player, OtherPlayer, DxDy, Next, Dir), % , Dir
	(getTheMove(State,Player,OtherPlayer, DxDy, H, Current, Player) -> Moves = [Current|Next] ; Moves = Next).

getAllMoves(_, _, _, _, [],[]).

getTheMove(State, Player, OtherPlayer, Coordinate, [X,Y], TheMove, P) :-
	%nextDirection(Coordinate, Current, Next),
	(iterMatrix(State,[X,Y], Square)), \+(Square = Player),
    %get(State, [X,Y],Square),\+(Square = Player), %ahh there already is an iter...
	(Square = OtherPlayer,
	getTheMove(State, Player, OtherPlayer, Coordinate, [X,Y],TheMove, P) ; Square = ., P = OtherPlayer, TheMove =[X,Y]).


getOtherPlayer(Me, Other) :-
	Me = 1 -> Other is 2 ; Other is 1.

*/
/**
calcPlayerScore(State, Player, Score) :- 
    calcPlayerScore(State, Player, Score, 0,0).

calcPlayerScore(_, _, [], Row, Column) :-
    Row >5,
    Column >5.
calcPlayerScore(State,Player, Score,Row, Column) :-
    Column >5, % need to reset in this basecase
    calcPlayerScore(State, Player, Score, Row+1, 0),!. %no backtracking from this stage.
calcPlayerScore(State, Player, Score, I, J) :-
    Row is I,
    Column is J,
    (iterMatrix(State, [Row, Column], Player) -> Score = [[Row, Column]|NextStone] ; Score = NextStone),
    calcPlayerScore(State, Player, NextStone, I, J+1),!. %stop backtrack.

%iterate function based on https://stackoverflow.com/questions/34949724/prolog-iterate-through-matrix
iterMatrix(State,[I,J], Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value).

%----------------

moves(Plyr, State, MvList) :-
	calcPlayerScore(State, Plyr, Pieces),
    write(Pieces),
	moves(Plyr, State, Pieces, UnsortedMvList),
	(UnsortedMvList = [] ->
		MvList = [n]
	;
		sort(UnsortedMvList, MvList)).

% Recurse through the pieces and check for valid moves.
moves(Plyr, State, [Coord|Tail], MvList) :-
	getOpponent(Plyr, Opp),
	moves(Plyr, State, Tail, NextMvs), !,
	(findMoves(State, Plyr, Opp, Coord, Mvs) ->
		append(Mvs, NextMvs, MvList) % Found moves, append to list.
	;
		MvList = NextMvs). % No moves found.

% Stop recursion on empty piece list.
moves(_Plyr, _State, [], []).

% getOpponent is a helper function that returns the opponent.
getOpponent(1, 2).
getOpponent(2, 1).

% nextCoord :: [Int, Int] -> Atom -> [Int, Int]
% nextCoord increments the X and Y coordinate according to the provided
% direction.
nextCoord([X, Y], 'N', [X, Yi]) :- Yi is Y - 1.
nextCoord([X, Y], 'S', [X, Yi]) :- Yi is Y + 1.
nextCoord([X, Y], 'E', [Xi, Y]) :- Xi is X + 1.
nextCoord([X, Y], 'W', [Xi, Y]) :- Xi is X - 1.
nextCoord([X, Y], 'NW', [Xi, Yi]) :- Xi is X - 1, Yi is Y - 1.
nextCoord([X, Y], 'NE', [Xi, Yi]) :- Xi is X + 1, Yi is Y - 1.
nextCoord([X, Y], 'SW', [Xi, Yi]) :- Xi is X - 1, Yi is Y + 1.
nextCoord([X, Y], 'SE', [Xi, Yi]) :- Xi is X + 1, Yi is Y + 1.

% findMoves :: [[Atom]] -> Int -> Int -> [[Int, Int]] -> [[Int, Int]]
% findMoves finds all legal moves in every direction for a provided slot.
findMoves(State, Plyr, Opp, Coord, Mvs) :-
	findMoves(State, Plyr, Opp, Coord, Mvs,
	['N', 'S', 'E', 'W', 'NW', 'NE', 'SW', 'SE']).
findMoves(State, Plyr, Opp, Coord, Mvs, [Wind|Tail]) :-
	findMoves(State, Plyr, Opp, Coord, NextMvs, Tail),
	(findMove(State, Plyr, Opp, Coord, Wind, Plyr, Mv) ->
		Mvs = [Mv|NextMvs] % Found move, add to list.
	;
		Mvs = NextMvs). % No move.
findMoves(_State, _Plyr, _Opp, _Coord, [], []).

% findMove ::
% 	[[Atom]] -> Int -> Int -> [Int, Int] -> Atom -> Atom -> [[Int, Int]]
% findMove finds a legal move from the provided slot for a direction.
findMove(State, Plyr, Opp, [X, Y], Wind, Prev, Mv) :-
	nextCoord([X, Y], Wind, [Xi, Yi]),
	get(State, [Xi, Yi], Slot),
	\+(Slot = Plyr), % The next slot must not be the same as the player.
	(
		% If the slot is the opponent, look further for an empty slot.
		(Slot = Opp),
		findMove(State, Plyr, Opp, [Xi, Yi], Wind, Opp, Mv)
	;
		% If the slot is empty, and the previous the opponent, it's a
		% valid move.
		(Slot = ., Prev = Opp),
		Mv = [Xi, Yi]
	).

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).


*/
