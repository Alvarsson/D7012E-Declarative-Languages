

% Modified as to output a descending list :) and for sets instead of classic value list ([1,2,3,4])
% sort by optimized quicksort (from Roman Barták's Prolog Programming site)

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G) :- %piv, list, left sorted, right sorted 
    getSumFromSet(H, Psum),
    getSumFromSet(X, Hsum),
    Hsum =< Psum, %comparing
    pivoting(H, T, L, G).
pivoting(H,[X|T],L,[X|G]) :- 
    pivoting(H,T,L,G).

%quick_sort2([],[]).
%quick_sort2(List,Sorted) :- q_sort(List, Sorted).
q_sort([],[]).
q_sort([H|T], Sorted):-
    pivoting(H,T,L1,L2),
    q_sort(L1,Sorted1), q_sort(L2,Sorted2),
    append(Sorted1,[H|Sorted2], Sorted). %build in append to list func

%Need function to get sum from SET structure
getSumFromSet(setStruct(Sum,_,_,_), Sum).

%TEST: sumList([1,2,3]) returns 6
%to summarize a sublist
sumList([],0).
sumList([SingleValue],SingleValue). %One item list gives that value.
sumList([Head1,Head2|Tail], Sum) :- 
    CurrentSum is Head1+Head2,
    sumList([CurrentSum|Tail], Sum). %summarize list


%TEST: makeSubset([1,2,3,4,5],0,4,L). returns [1,2,3,4,5]
% this becomes List-> i -> j -> resultList
makeSubset([Last|_], _, 0, [Last]).
%makeSubset([SingleValue],0,0,[SingleValue]). wasnt smooth
makeSubset([_|Tail],Index1,Index2,OutList) :-
    Index1>0,
    Index1 =< Index2,
    Imin1 is Index1-1,
    Jmin1 is Index2-1,
    makeSubset(Tail,Imin1,Jmin1,OutList).
makeSubset([Head|Tail],0,Index2, OutList) :-
    %0 is Index1,
    0 =< Index2,
    Jmin2 is Index2-1,
    OutList = [Head| Rest],
    makeSubset(Tail,0,Jmin2,Rest).

/**
helpFunc([],[]).
helpFunc(List,SetList) :-
    createSetFromSubs(List, 0, 0, SetList).
*/

% FFFFFFUUUUUU.... order matters apperantly. 

createSetFromSubs(List, Index1, Index2, SetList) :-
    makeSubset(List, Index1, Index2,SetOut), %make the subset
    sumList(SetOut, Sum), % ListSum = sumList(Sub,Sum), duh can just use Sum
    Jindex is Index2+1, %force arithmetic eval.
    createSetFromSubs(List,Index1,Jindex,MemList),!, % recurse until ending, pick up everything after.
    SetList = [setStruct(Sum,Index1,Index2,SetOut)|MemList]. % no backtrack from satisfied.
createSetFromSubs(List,Index1,_,MemList) :- %increment I and the recurse with above
    length(List,MaxLength),
    Index1 < MaxLength,
    Iindex is Index1+1,
    createSetFromSubs(List,Iindex,Iindex,MemList),!. %no backtrack!
createSetFromSubs(_,EQUAL,EQUAL,[]).%base case with empty inlist

%Need to create the sets, sort the sets, then show the asked amount
smallestKSets(K,InList,PrintOut) :-
    createSetFromSubs(InList,0,0,SetOut), %create the set with all subsets
    %helpFunc(InList, SetOut),
    q_sort(SetOut,Sorted),%sort the set
    getAskedKSets(K,Sorted,PrintOut).

%sortingParameter(Set, SortValue) :-
getAskedKSets(0,_,[]).%basecase k = 0
getAskedKSets(Ksmall,[H|T],KSets) :-
    Count is Ksmall-1,
    getAskedKSets(Count,T,ShortList),
    KSets = [H |ShortList].

tester() :-
    smallestKSets(3,[-1,2,-3,4,-5],Out1),
    write(Out1),
    nl,
    nl,
    smallestKSets(6, [24,-11,-34,42,-24,7,-19,21],Out2),
    write(Out2),
    nl,
    nl,
    smallestKSets(8, [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],Out3),
    write(Out3),
    nl,
    nl,
    %findall(X,f(a,X),Bag,Tail).
    findall(P, (between(1,100,I), P is I*(-1)^I), LongTest),
    %P is I*(-1)^I,
    smallestKSets(15, LongTest,Out4),
    write(Out4),
    nl. %newline to make pretty :)

%NOTE! Indexing is off by one since we start at index 0,
% but that doesn't feel all to important.



