#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :- 
  maplist(term_string, [Max, Sol, Ans], Argv),
  parsons(Max, Sol, Ans, Hist, Score),
  writeln(Score), 
  writeln(Hist).

parsons(Max, Sol, Ans, Corrections, Score) :-
  findall(L-I, (
    member(L-I, Sol), 
    \+ member(L-_, Ans)
  ), MissingLines),
  findall(insert(L), (
    member(L-_, MissingLines)
  ), Insertions),
  findall(delete(L), (
    nth0(L, Ans, x-_)
  ), Deletions),
  findall(L-I, (
    member(L-I, Ans),
    L \= x
  ), ValidLines),
  foldl(insert_missing, MissingLines, ValidLines, State),
  solve(Max, Sol, [State-[]], 0, Edits),
  append(Insertions, Deletions, Edits, Corrections),
  length(Corrections, Changes),
  Score is Max - Changes,
  Score >= 0.
parsons(_, _, _, [], 0).

solve(Max, Sol, Ss, Len, Hist) :-
  Len =< Max,
  member(Sol-HRev, Ss),
  !,
  reverse(HRev, Hist).
solve(Max, Sol, S0s, Len0, Hist) :-
  Len1 is Len0 + 1,
  Len1 =< Max,
  findall(S1-[Act|Hist0], (
    member(S0-Hist0, S0s), 
    action(Sol, Act, S0, S1)
  ), S1sAll),
  remove_duplicates(S1sAll, S1s),
  % length(S1s, L), length(S1sAll, LAll), print(L), print(-), print(LAll), nl,
  solve(Max, Sol, S1s, Len1, Hist).

action(Sol, swap(L0,L1), S0, S1) :-
  append(SStart, [L0-I0|STmp0], S0),
  \+ nth0(L0, Sol, L0-I0),
  append(SMid, [L1-I1|SEnd], STmp0),
  \+ nth0(L1, Sol, L1-I1),
  L1 < L0,
  nth0(L0, Sol, L0-I1),
  nth0(L1, Sol, L1-I0),
  append(SStart, [L1-I0|SMid], [L0-I1|SEnd], S1).
action(Sol, indent(L,IndentedLen), S0, S1) :-
  append(SStart, [L-I0|STmp0], S0),
  nth0(L, Sol, L-I1),
  I0 \= I1,
  IDiff is I1 - I0,
  sign(IDiff, Sign), 
  append(ToIndent, SEnd, STmp0),
  findall(L1-In0, (
    member(L1-In0, ToIndent),
    match_indent_sign(Sol, Sign, L1-In0)
  ), ToIndent),
  \+ (
    [L1-In0|_] = SEnd,
    match_indent_sign(Sol, Sign, L1-In0)
  ),
  maplist(indent(IDiff), [L-I0|ToIndent], Indented),
  append(SStart, Indented, SEnd, S1),
  length(Indented, IndentedLen),
  length(Sol, SolLen),
  (IndentedLen < SolLen / 2 -> true ; !).
action(Sol, cycle(L), S0, S1) :- 
  append(SStart0, [L-I0|SEnd0], S0),
  \+ nth0(L, L-I0, Sol),
  nth0(L, Sol, L-I1),
  append(SStart0, SEnd0, STmp),
  nth0(L, S1, L-I1),
  append(SStart1, SEnd1, STmp),
  append(SStart1, [L-I1|SEnd1], S1).
  
remove_duplicates(XsAll, Xs) :-
  sort(XsAll, XsSorted),
  remove_duplicates_sorted(XsSorted, [], Xs).
  
remove_duplicates_sorted([], L, L).
remove_duplicates_sorted([X0-X1,X0-_|Xs], S0, S) :-
  !,
  remove_duplicates_sorted([X0-X1|Xs], S0, S).
remove_duplicates_sorted([X0-X1|Xs], S0, S) :-
  remove_duplicates_sorted(Xs, [X0-X1|S0], S).

append(Xs, Ys, Zs, XYZs) :- 
  append(Xs, Ys, XYs),
  append(XYs, Zs, XYZs).

indent(Indent, L-I0, L-I1) :-
  I1 is max(0, I0 + Indent).

match_indent_sign(Sol, Sign, L-I0) :-
  nth0(L, Sol, L-I1),
  IDiff is I1 - I0,
  sign(IDiff, Sign).

insert_missing(L-I, Ls0, Ls1) :-
  length(LsStart, L),
  append(LsStart, LsEnd, Ls0),
  append(LsStart, [L-I|LsEnd], Ls1).
