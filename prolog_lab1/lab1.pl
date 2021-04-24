
/*
test if program will "compile" by built in ?- consult('<pl file>')

State: a state, will define with the required keys and packade and current room :)
N: positive integer of "tries"
Trace: List of actions
*/

%starting state would be, no keys, no package, in room 2 state(no,no,no,room1) if state: state(steelKey, brassKey, package, room)
%final state would be wildcard keys, in room2 and drop package.

%move should concist of switching a room, picking up an item, or dropping an item.
%move(State, nextState, Move)

% Good things i learnned the "hard way".
% + A variable is written as a sequence of letters and digits, beginning with a capital letter. The underscore (_) is considered to be a capital letter.
% + An atom begins with lower case letter or enclosed by ''
% + You can pass atom as variable, nice.
% 

% Will follow farmer structure a lot!

solveR(state(_,_,_,room2),_,[]). 
solveR( CurrentRoom, N, Trace) :- N > 0, 
    move(CurrentRoom,NextState, Move), \+(NextState = state(_,hold,hold,hold)), %not provable \+, so if we can prove that the state has keys and package, then dont move 
    solveR(NextState, N-1, RestTrace),
    Trace = [Move | RestTrace]. %same as haskells x:xs in a way


%%%%% Some simplifying predicate
validToPick(A, B) :-
    A = B,
    A == B.

validToDrop(A) :-
    A == hold.

validMove(A, B, C) :-
    A = C,
    B = hold.

validNextState(Next,A,B,C,D) :-
    Next = state(A,B,C,D).


%move logic for room 2
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :- % to move to room 2 we need steelkey, current move and next state
    validMove(CurrentRoom, SteelKey, room2),
    validNextState(NextState, room1,SteelKey,BrassKey,Package),
    Move = 'moved to room 1(from 2)'. % move clause with full stop

%move room 1 (2)
move(state(CurrentRoom,SteelKey,BrassKey,Package), NextState, Move) :-
    validMove(CurrentRoom,SteelKey, room1),
    validNextState(NextState, room2, SteelKey, BrassKey,Package),
    Move = 'moved to room 2'.

%move room  3
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :-
    validMove(CurrentRoom,BrassKey, room3),
    validNextState(NextState, room1, SteelKey,BrassKey,Package),
    Move = 'moved to room 1(from 3)'.

%move room 1 (3)
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :-
    validMove(CurrentRoom,BrassKey, room1),
    validNextState(NextState,room3, SteelKey,BrassKey,Package),
    Move = 'moved to room 3'. 

%pick up clause definitions
%steelKey
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :-
    validToPick(CurrentRoom,SteelKey),
    validNextState(NextState,CurrentRoom,hold,BrassKey,Package),
    Move = 'picked up steel key'.
%brassKey
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :-
    validToPick(CurrentRoom,BrassKey),
    validNextState(NextState,CurrentRoom,SteelKey,hold,Package),
    Move = 'picked up brass key'.
%package
move(state(CurrentRoom,SteelKey, BrassKey, Package), NextState, Move) :-
    validToPick(Package,CurrentRoom), 
    validNextState(NextState,CurrentRoom,SteelKey,BrassKey,hold),
    Move = 'picked up package'.

%Drop clause definitions
%steelKey
move(state(CurrentRoom,SteelKey,BrassKey,Package),NextState,Move) :-
    validToDrop(SteelKey),
    validNextState(NextState,CurrentRoom,CurrentRoom,BrassKey,Package),
    Move = 'dropped steel key'.
%brassKey
move(state(CurrentRoom,SteelKey,BrassKey, Package), NextState,Move) :-
    validToDrop(BrassKey),
    validNextState(NextState,CurrentRoom,SteelKey,CurrentRoom,Package),
    Move = 'dropped brass key'.
%package
move(state(CurrentRoom,SteelKey,BrassKey,Package),NextState,Move) :-
    validToDrop(Package),
    validNextState(NextState,CurrentRoom,SteelKey,BrassKey,CurrentRoom),
    Move = 'dropped package'.





/*
What I know:
+ Can move between r1 <-> r2
+ Can move between r1 <-> r3
+ Can pick up items if robot in same room
+ Dropping an item ends up in current room
+ The robot can carry max 2 items at once
+ No limit on how many items in a room

Question:
- Can the robot fetch the package in r3 and bring it to r2?
-- If so, how would the robot behave?
*/ 

