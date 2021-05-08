/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,1) :- 
	initBoard(InitialState).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

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

%Check amount of stones on the board for player
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
/** part of number increment attempt, lists is most likely smarter.
checkVal('.',_,_, 0).
checkVal(Value, Player,_, 0) :- Value \= Player.
checkVal(Value, Player, Counter, NextCount, 1) :- 
    Value = Player, 
    incrementVar(Counter, NextCount).

incrementVar(X, X1) :- X1 is X+1.

incrementScore(Counter,Value, FinalScore) :- 
    FinalScore is Counter+Value.
*/
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),!,
	getScore(State, 1, Plyr1Score),!,
	getScore(State, 2, Plyr2Score),!,
	Plyr1Score = Plyr2Score.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

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


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
