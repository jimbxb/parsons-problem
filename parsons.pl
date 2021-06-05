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
  ), Filtered),
  foldl(insert_missing, MissingLines, Filtered, State),
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
  % nl, print(S1s), nl,
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
  append(LsStart, [L-I|LsEnd], Ls1), !.

%def foo(x):
%   for y in x:
%       print(y)
%   print("\n")
%[0-0,1-1,2-2,3-1]

%0   def tournvote(votes):
%1       tally = {}
%2       for vote in votes:
%3           num_ranks = len(vote)
%4           for cur_rank in range(num_ranks):
%5               if vote[cur_rank] not in tally:
%6                   tally[vote[cur_rank]] = 0
%7               tally[vote[cur_rank]] += num_ranks - cur_rank
%8       highest_points = max(tally.values()); winner = None
%9       for candidate in tally:
%10         if tally[candidate] == highest_points:
%11             if not winner:
%12                 winner = candidate
%13             else:
%14                 winner = 'tie'
%15     return winner
%[0-0,1-1,2-1,3-2,4-2,5-3,6-4,7-3,8-1,9-1,10-2,11-3,12-4,13-3,14-4,15-1]

% def tournvote(votes):
%     tally = {}
%     for vote in votes:
%         num_ranks = len(vote)
%         for cur_rank in range(num_ranks):
%             if vote[cur_rank] not in tally:
%                 tally[vote[cur_rank]] = 0
%             else:
%     highest_points = max(tally.values()); winner = None
%     for candidate in tally:
%         tally[vote[cur_rank]] += num_ranks - cur_rank
%         if tally[candidate] == highest_points:
%             winner = candidate
%         if not winner:
%             winner = 'tie'
%     return winner
%[0-0,1-1,2-1,3-2,4-2,5-3,6-4,13-3,8-1,9-1,7-2,10-2,12-3,11-2,14-3,15-1]