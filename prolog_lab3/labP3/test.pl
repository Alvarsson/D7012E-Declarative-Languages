




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
*/

%retry but with lists
winner(State,Plyr) :-
	%terminal(State),!, %if terminal state is true, cant backtrack since its always true
	getScore(State, Plyr1, Plyr2),
	Plyr1 =\= Plyr2,
	(Plyr1 < Plyr2) ->  Plyr = 1 ; Plyr = 2.%if then else predicate

getScore(State, Player1Score, Player2Score) :-
    calcPlayerScore(State, 1, Player1Score),
    calcPlayerScore(State, 2, Player2Score).

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
    iterMatrix(State, [Row, Column], Value) -> Score = [[Row, Column]|NextStone] ; Score = NextStone,
    calcPlayerScore(State, Player, Score, I, J+1),!. %stop backtrack.

iterMatrix(State, [I,J], Value) :-
    nth0(I, State, Row),
    nth0(J, Row, Value).




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
