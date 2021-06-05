#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :- 
  maplist(term_string, [MaxEdits, Soln, Ans], Argv),
  parsons(MaxEdits, Soln, Ans, Corrections, Score),
  writeln(Score), 
  writeln(Corrections).

parsons(MaxEdits, Soln, Ans, Corrections, Score) :-
  findall(L-I, (member(L-I, Soln), \+ member(L-_, Ans)), MissingLines),
  findall(L-I, (member(L-I, Ans), L \= x), ValidLines),
  foldl(insert_missing, MissingLines, ValidLines, State),
  findall(insert(L), (member(L-_, MissingLines)), Insertions),
  findall(delete(L), (nth0(L, Ans, x-_)), Deletions),
  append(Insertions, Deletions, InsertionsDeletions),
  length(InsertionsDeletions, NInsertionsDeletions),
  Remaining is MaxEdits - NInsertionsDeletions,
  solve(Remaining, Soln, [State-[]], Edits),
  !,
  append(Edits, InsertionsDeletions, Corrections),
  length(Corrections, NCorrections),
  Score is MaxEdits - NCorrections.
parsons(_, _, _, [fail], 0).

solve(Remaining, Soln, Ss, Edits) :-
  Remaining > 0,
  member(Soln-EditsRev, Ss),
  reverse(EditsRev, Edits).
solve(Rem0, Soln, S0s, Edits) :-
  Rem1 is Rem0 - 1,
  Rem1 > 0,
  findall(S1-[Act|Edits0], (
    member(S0-Edits0, S0s), 
    action(Soln, Act, S0, S1)
  ), S1sAll),
  remove_duplicates(S1sAll, S1s),
  S1s \= [],
  % length(S1s, L), length(S1sAll, LAll), write(L), write(" - "), writeln(LAll),
  % writeln(S1sAll),
  % writeln(S1s),
  solve(Rem1, Soln, S1s, Edits).

action(Soln, swap(L1,L0), S0, S1) :-
  append(SStart, [L0-I0|STmp0], S0),
  \+ nth0(L0, Soln, L0-I0),
  append(SMid, [L1-I1|SEnd], STmp0),
  \+ nth0(L1, Soln, L1-I1),
  L1 < L0,
  nth0(L0, Soln, L0-I1),
  nth0(L1, Soln, L1-I0),
  append(SStart, [L1-I0|SMid], [L0-I1|SEnd], S1).
action(Soln, indent(L,IndentedLen), S0, S1) :-
  append(SStart, [L-I0|STmp0], S0),
  nth0(L, Soln, L-I1),
  I0 \= I1,
  IDiff is I1 - I0,
  sign(IDiff, Sign), 
  append(ToIndent, SEnd, STmp0),
  findall(LIn, (
    member(LIn, ToIndent),
    match_indent_sign(Soln, Sign, LIn)
  ), ToIndent),
  \+ (
    ([LIn0|_] = SEnd ; append(_, [LIn0], SStart)),
    match_indent_sign(Soln, Sign, LIn0)
  ),
  maplist(indent(IDiff), [L-I0|ToIndent], Indented),
  append(SStart, Indented, SEnd, S1),
  length(Indented, IndentedLen).
action(Soln, cycle(Idx,L), S0, S1) :- 
  append(SStart0, [L-_|SEnd0], S0),
  nth0(Idx, S0, L-_),
  Idx \= L,
  nth0(L, Soln, L-I1),
  append(SStart0, SEnd0, STmp),
  nth0(L, S1, L-I1),
  append(SStart1, SEnd1, STmp),
  append(SStart1, [L-I1|SEnd1], S1).

insert_missing(L-I, Ls0, Ls1) :-
  length(LsStart, L),
  append(LsStart, LsEnd, Ls0),
  append(LsStart, [L-I|LsEnd], Ls1).

indent(Indent, L-I0, L-I1) :-
  I1 is max(0, I0 + Indent).

match_indent_sign(Soln, Sign, L-I0) :-
  nth0(L, Soln, L-I1),
  IDiff is I1 - I0,
  sign(IDiff, Sign).
  
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