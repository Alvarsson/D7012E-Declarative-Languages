% from the website "prolog site" with loads of exercises.
% 1.01 (*): Find the last element of a list

% my_last(X,L) :- X is the last element of the list L
%    (element,list) (?,?)

% Note: last(?Elem, ?List) is predefined

%my_last(X,[X]).
%my_last(X,[_|L]) :- my_last(X,L).

parent( pam, bob).       % Pam is a parent of Bob
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).

female( pam).            % Pam is female
female( liz).
female( ann).
female( pat).
male( tom).              % Tom is male
male( bob).
male( jim).

/* Info for me
If --> :- 
Not ~ Not 
Or V ;
and ^ ,

\+  not provable, succeds if its argument is not provable (not in a way)


*/


