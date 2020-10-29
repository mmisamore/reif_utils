:- use_module('../prolog/reif_utils').
:- use_module(library(clpfd)).

:- begin_tests(reif_utils).

% Tests for #<

% + + +
test('#<_+_+_+_true') :-
  #<(2, 3, true).

test('#<_+_+_+_not_false', [ fail ]) :-
  #<(2, 3, false).

test('#<_+_+_+_false') :-
  #<(3, 2, false).

test('#<_+_+_+_not_true', [ fail ]) :-
  #<(3, 2, true).

test('#<_+_+_+_eq_false') :-
  #<(3, 3, false).

test('#<_+_+_+_eq_not_true', [ fail ]) :-
  #<(3, 3, true).

% + + -
test('#<_+_+_-_true') :-
  #<(2, 3, Cond),
  Cond = true.

test('#<_+_+_-_not_false', [ fail ]) :-
  #<(2, 3, Cond),
  Cond = false.

test('#<_+_+_-_false') :-
  #<(3, 2, Cond),
  Cond = false.

test('#<_+_+_-_not_true', [ fail ]) :-
  #<(3, 2, Cond),
  Cond = true.

test('#<_+_+_-_eq_false') :-
  #<(3, 3, Cond),
  Cond = false.

test('#<_+_+_-_eq_not_true', [ fail ]) :-
  #<(3, 3, Cond),
  Cond = true.

% + - +
test('#<_+_-_+_true') :-
  #<(2, Y, true),
  2 #< Y.

test('#<_+_-_+_not_false', [ fail ]) :-
  #<(2, Y, false),
  2 #< Y.

test('#<_+_-_+_false') :-
  #<(3, Y, false),
  3 #>= Y.

test('#<_+_-_+_not_true', [ fail ]) :-
  #<(3, Y, true),
  3 #>= Y.

test('#<_+_-_+_eq_false') :-
  #<(3, Y, false),
  3 #= Y.

test('#<_+_-_+_eq_not_true', [ fail ]) :-
  #<(3, Y, true),
  3 #= Y.

% + - -
test('#<_+_-_-_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #<(2, Y, Cond), Cond = true ), Ans),
  2 #< Y,
  Ans = [Y-true].

test('#<_+_-_-_not_false', [ fail ]) :-
  #<(2, Y, Cond),
  Cond = false,
  2 #< Y.

test('#<_+_-_-_false') :-
  #<(3, Y, Cond),
  Cond = false,
  3 #>= Y.

test('#<_+_-_-_not_true', [ fail ]) :-
  #<(3, Y, Cond),
  Cond = true,
  3 #>= Y.

test('#<_+_-_-_eq_false') :-
  #<(3, Y, Cond),
  Cond = false,
  3 #= Y.

test('#<_+_-_-_eq_not_true', [ fail ]) :-
  #<(3, Y, Cond),
  Cond = true,
  3 #= Y.

% - + +
test('#<_-_+_+_true') :-
  #<(X, 3, true),
  X #< 3.

test('#<_-_+_+_not_false', [ fail ]) :-
  #<(X, 3, false),
  X #< 3.

test('#<_-_+_+_false') :-
  #<(X, 3, false),
  X #>= 3.

test('#<_-_+_+_not_true', [ fail ]) :-
  #<(X, 3, true),
  X #>= 3.

test('#<_-_+_+_eq_false') :-
  #<(X, 3, false),
  X #= 3.

test('#<_-_+_+_eq_not_true', [ fail ]) :-
  #<(X, 3, true),
  X #= 3.

% - + -
test('#<_-_+_-_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #<(X, 3, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #< 3.

test('#<_-_+_-_not_false', [ fail ]) :-
  #<(X, 3, Cond),
  Cond = false,
  X #< 3.

test('#<_-_+_-_false') :-
  #<(X, 3, Cond),
  Cond = false,
  X #>= 3.

test('#<_-_+_-_not_true', [ fail ]) :-
  #<(X, 3, Cond),
  Cond = true,
  X #>= 3.

test('#<_-_+_-_eq_false') :-
  #<(X, 3, Cond),
  Cond = false,
  X #= 3.

test('#<_-_+_-_eq_not_true', [ fail ]) :-
  #<(X, 3, Cond),
  Cond = true,
  X #= 3.

% - - +
test('#<_-_-_+_true') :-
  #<(X, Y, true),
  X #< Y.

test('#<_-_-_+_not_false', [ fail ]) :-
  #<(X, Y, false),
  X #< Y,
  [X, Y] ins 0..1.

test('#<_-_-_+_false') :-
  #<(X, Y, false),
  X #>= Y.

test('#<_-_-_+_not_true', [ fail ]) :-
  #<(X, Y, true),
  X #>= Y,
  [X, Y] ins 0..1.

test('#<_-_-_+_eq_false') :-
  #<(X, Y, false),
  X #= Y.

test('#<_-_-_+_eq_not_true', [ fail ]) :-
  #<(X, Y, true),
  X #= Y,
  [X, Y] ins 0..1.

% - - -
test('#<_-_-_-_true') :-
  % Query order forces choice point
  findall(X-Y-Cond, ( #<(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #< Y.

test('#<_-_-_-_not_false', [ fail ]) :-
  #<(X, Y, Cond),
  Cond = false,
  X #< Y,
  [X, Y] ins 0..1.

test('#<_-_-_-_false') :-
  #<(X, Y, Cond),
  Cond = false,
  X #>= Y.

test('#<_-_-_-_not_true', [ fail ]) :-
  #<(X, Y, Cond),
  Cond = true,
  X #>= Y,
  [X, Y] ins 0..1.

test('#<_-_-_+_eq_false') :-
  #<(X, Y, Cond),
  Cond = false,
  X #= Y.

test('#<_-_-_+_eq_not_true', [ fail ]) :-
  #<(X, Y, Cond),
  Cond = true,
  X #= Y,
  [X,Y] ins 0..1.


% Tests for #>

% + + +
test('#>_+_+_+_true') :-
  #>(3, 2, true).

test('#>_+_+_+_not_false', [ fail ]) :-
  #>(3, 2, false).

test('#>_+_+_+_false') :-
  #>(2, 3, false).

test('#>_+_+_+_not_true', [ fail ]) :-
  #>(2, 3, true).

test('#>_+_+_+_eq_false') :-
  #>(3, 3, false).

test('#>_+_+_+_eq_not_true', [ fail ]) :-
  #>(3, 3, true).

% + + -
test('#>_+_+_-_true') :-
  #>(3, 2, Cond),
  Cond = true.

test('#>_+_+_-_not_false', [ fail ]) :-
  #>(3, 2, Cond),
  Cond = false.

test('#>_+_+_-_false') :-
  #>(2, 3, Cond),
  Cond = false.

test('#>_+_+_-_not_true', [ fail ]) :-
  #>(2, 3, Cond),
  Cond = true.

test('#>_+_+_-_eq_false') :-
  #>(3, 3, Cond),
  Cond = false.

test('#>_+_+_-_eq_not_true', [ fail ]) :-
  #>(3, 3, Cond),
  Cond = true.

% + - +
test('#>_+_-_+_true') :-
  #>(2, Y, true),
  2 #> Y.

test('#>_+_-_+_not_false', [ fail ]) :-
  #>(2, Y, false),
  2 #> Y.

test('#>_+_-_+_false') :-
  #>(3, Y, false),
  3 #=< Y.

test('#>_+_-_+_not_true', [ fail ]) :-
  #>(3, Y, true),
  3 #=< Y.

test('#>_+_-_+_eq_false') :-
  #>(3, Y, false),
  3 #= Y.

test('#>_+_-_+_eq_not_true', [ fail ]) :-
  #>(3, Y, true),
  3 #= Y.

% + - -
test('#>_+_-_-_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #>(3, Y, Cond), Cond = true ), Ans),
  3 #> Y,
  Ans = [Y-true].

test('#>_+_-_-_not_false', [ fail ]) :-
  #>(3, Y, Cond),
  Cond = false,
  3 #> Y.

test('#>_+_-_-_false') :-
  #>(3, Y, Cond),
  Cond = false,
  3 #=< Y.

test('#>_+_-_-_not_true', [ fail ]) :-
  #>(3, Y, Cond),
  Cond = true,
  3 #=< Y.

test('#>_+_-_-_eq_false') :-
  #>(3, Y, Cond),
  Cond = false,
  3 #= Y.

test('#>_+_-_-_eq_not_true', [ fail ]) :-
  #>(3, Y, Cond),
  Cond = true,
  3 #= Y.

% - + +
test('#>_-_+_+_true') :-
  #>(X, 3, true),
  X #> 3.

test('#>_-_+_+_not_false', [ fail ]) :-
  #>(X, 3, false),
  X #> 3.

test('#>_-_+_+_false') :-
  #>(X, 3, false),
  X #=< 3.

test('#>_-_+_+_not_true', [ fail ]) :-
  #>(X, 3, true),
  X #=< 3.

test('#>_-_+_+_eq_false') :-
  #>(X, 3, false),
  X #= 3.

test('#>_-_+_+_eq_not_true', [ fail ]) :-
  #>(X, 3, true),
  X #= 3.

% - + -
test('#>_-_+_-_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #>(X, 2, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #> 3.

test('#>_-_+_-_not_false', [ fail ]) :-
  #>(X, 3, Cond),
  Cond = false,
  X #> 3.

test('#>_-_+_-_false') :-
  #>(X, 3, Cond),
  Cond = false,
  X #=< 3.

test('#>_-_+_-_not_true', [ fail ]) :-
  #>(X, 3, Cond),
  Cond = true,
  X #=< 3.

test('#>_-_+_-_eq_false') :-
  #>(X, 3, Cond),
  Cond = false,
  X #= 3.

test('#>_-_+_-_eq_not_true', [ fail ]) :-
  #>(X, 3, Cond),
  Cond = true,
  X #= 3.

% - - +
test('#>_-_-_+_true') :-
  #>(X, Y, true),
  X #> Y.

test('#>_-_-_+_not_false', [ fail ]) :-
  #>(X, Y, false),
  X #> Y,
  [X, Y] ins 0..1.

test('#>_-_-_+_false') :-
  #>(X, Y, false),
  X #=< Y.

test('#>_-_-_+_not_true', [ fail ]) :-
  #>(X, Y, true),
  X #=< Y,
  [X, Y] ins 0..1.

test('#>_-_-_+_eq_false') :-
  #>(X, Y, false),
  X #= Y.

test('#>_-_-_+_eq_not_true', [ fail ]) :-
  #>(X, Y, true),
  X #= Y,
  [X, Y] ins 0..1.

% - - -
test('#>_-_-_-_true') :-
  % Query order forces choice point
  findall(X-Y-Cond, ( #>(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #> Y.

test('#>_-_-_-_not_false', [ fail ]) :-
  #>(X, Y, Cond),
  Cond = false,
  X #> Y,
  [X, Y] ins 0..1.

test('#>_-_-_-_false') :-
  #>(X, Y, Cond),
  Cond = false,
  X #=< Y.

test('#>_-_-_-_not_true', [ fail ]) :-
  #>(X, Y, Cond),
  Cond = true,
  X #=< Y,
  [X, Y] ins 0..1.

test('#>_-_-_+_eq_false') :-
  #>(X, Y, Cond),
  Cond = false,
  X #= Y.

test('#>_-_-_+_eq_not_true', [ fail ]) :-
  #>(X, Y, Cond),
  Cond = true,
  X #= Y,
  [X,Y] ins 0..1.


% Tests for #=<

% + + +
test('#=<_+_+_+_true') :-
  #=<(2, 3, true).

test('#=<_+_+_+_not_false', [ fail ]) :-
  #=<(2, 3, false).

test('#=<_+_+_+_false') :-
  #=<(3, 2, false).

test('#=<_+_+_+_not_true', [ fail ]) :-
  #=<(3, 2, true).

test('#=<_+_+_+_eq_true') :-
  #=<(3, 3, true).

test('#=<_+_+_+_eq_not_false', [ fail ]) :-
  #=<(3, 3, false).

% + + -
test('#=<_+_+_-_true') :-
  #=<(2, 3, Cond),
  Cond = true.

test('#=<_+_+_-_not_false', [ fail ]) :-
  #=<(2, 3, Cond),
  Cond = false.

test('#=<_+_+_-_false') :-
  #=<(3, 2, Cond),
  Cond = false.

test('#=<_+_+_-_not_true', [ fail ]) :-
  #=<(3, 2, Cond),
  Cond = true.

test('#=<_+_+_-_eq_true') :-
  #=<(3, 3, Cond),
  Cond = true.

test('#=<_+_+_-_eq_not_false', [ fail ]) :-
  #=<(3, 3, Cond),
  Cond = false.

% + - +
test('#=<_+_-_+_true') :-
  #=<(2, Y, true),
  2 #=< Y.

test('#=<_+_-_+_not_false', [ fail ]) :-
  #=<(2, Y, false),
  2 #=< Y.

test('#=<_+_-_+_false') :-
  #=<(3, Y, false),
  3 #> Y.

test('#=<_+_-_+_not_true', [ fail ]) :-
  #=<(3, Y, true),
  3 #> Y.

test('#=<_+_-_+_eq_true') :-
  #=<(3, Y, true),
  3 #= Y.

test('#=<_+_-_+_eq_not_false', [ fail ]) :-
  #=<(3, Y, false),
  3 #= Y.

% + - -
test('#=<_+_-_-_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #=<(2, Y, Cond), Cond = true ), Ans),
  2 #=< Y,
  Ans = [Y-true].

test('#=<_+_-_-_not_false', [ fail ]) :-
  #=<(2, Y, Cond),
  Cond = false,
  2 #=< Y.

test('#=<_+_-_-_false') :-
  #=<(3, Y, Cond),
  Cond = false,
  3 #> Y.

test('#=<_+_-_-_not_true', [ fail ]) :-
  #=<(3, Y, Cond),
  Cond = true,
  3 #> Y.

test('#=<_+_-_-_eq_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #=<(3, Y, Cond), Cond = true ), Ans),
  Ans = [Y-true],
  3 #= Y.

test('#=<_+_-_-_eq_not_false', [ fail ]) :-
  #=<(3, Y, Cond),
  Cond = false,
  3 #= Y.

% - + +
test('#=<_-_+_+_true') :-
  #=<(X, 3, true),
  X #=< 3.

test('#=<_-_+_+_not_false', [ fail ]) :-
  #=<(X, 3, false),
  X #=< 3.

test('#=<_-_+_+_false') :-
  #=<(X, 3, false),
  X #> 3.

test('#=<_-_+_+_not_true', [ fail ]) :-
  #=<(X, 3, true),
  X #> 3.

test('#=<_-_+_+_eq_true') :-
  #=<(X, 3, true),
  X #= 3.

test('#=<_-_+_+_eq_not_false', [ fail ]) :-
  #=<(X, 3, false),
  X #= 3.

% - + -
test('#=<_-_+_-_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #=<(X, 3, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #=< 3.

test('#=<_-_+_-_not_false', [ fail ]) :-
  #=<(X, 3, Cond),
  Cond = false,
  X #=< 3.

test('#=<_-_+_-_false') :-
  #=<(X, 3, Cond),
  Cond = false,
  X #> 3.

test('#=<_-_+_-_not_true', [ fail ]) :-
  #=<(X, 3, Cond),
  Cond = true,
  X #> 3.

test('#=<_-_+_-_eq_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #=<(X, 3, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #= 3.

test('#=<_-_+_-_eq_not_false', [ fail ]) :-
  #=<(X, 3, Cond),
  Cond = false,
  X #= 3.

% - - +
test('#=<_-_-_+_true') :-
  #=<(X, Y, true),
  X #=< Y.

test('#=<_-_-_+_not_false', [ fail ]) :-
  #=<(X, Y, false),
  X #=< Y,
  [X, Y] ins 0..1.

test('#=<_-_-_+_false') :-
  #=<(X, Y, false),
  X #> Y.

test('#=<_-_-_+_not_true', [ fail ]) :-
  #=<(X, Y, true),
  X #> Y,
  [X, Y] ins 0..1.

test('#=<_-_-_+_eq_true') :-
  #=<(X, Y, true),
  X #= Y.

test('#=<_-_-_+_eq_not_false', [ fail ]) :-
  #=<(X, Y, false),
  X #= Y.

% - - -
test('#=<_-_-_-_true') :-
  findall(X-Y-Cond, ( #=<(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #=< Y.

test('#=<_-_-_-_not_false', [ fail ]) :-
  #=<(X, Y, Cond),
  Cond = false,
  X #=< Y,
  [X, Y] ins 0..1.

test('#=<_-_-_-_false') :-
  #=<(X, Y, Cond),
  Cond = false,
  X #> Y.

test('#=<_-_-_-_not_true', [ fail ]) :-
  #=<(X, Y, Cond),
  Cond = true,
  X #> Y,
  [X, Y] ins 0..1.

test('#=<_-_-_+_eq_true') :-
  % Query order forces choice point
  findall(X-Y-Cond, ( #=<(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #= Y.

test('#=<_-_-_+_eq_not_false', [ fail ]) :-
  #=<(X, Y, Cond),
  Cond = false,
  X #= Y.


% Tests for #>=

% + + +
test('#>=_+_+_+_true') :-
  #>=(3, 2, true).

test('#>=_+_+_+_not_false', [ fail ]) :-
  #>=(3, 2, false).

test('#>=_+_+_+_false') :-
  #>=(2, 3, false).

test('#>=_+_+_+_not_true', [ fail ]) :-
  #>=(2, 3, true).

test('#>=_+_+_+_eq_true') :-
  #>=(3, 3, true).

test('#>=_+_+_+_eq_not_false', [ fail ]) :-
  #>=(3, 3, false).

% + + -
test('#>=_+_+_-_true') :-
  #>=(3, 2, Cond),
  Cond = true.

test('#>=_+_+_-_not_false', [ fail ]) :-
  #>=(3, 2, Cond),
  Cond = false.

test('#>=_+_+_-_false') :-
  #>=(2, 3, Cond),
  Cond = false.

test('#>=_+_+_-_not_true', [ fail ]) :-
  #>=(2, 3, Cond),
  Cond = true.

test('#>=_+_+_-_eq_true') :-
  #>=(3, 3, Cond),
  Cond = true.

test('#>=_+_+_-_eq_not_false', [ fail ]) :-
  #>=(3, 3, Cond),
  Cond = false.

% + - +
test('#>=_+_-_+_true') :-
  #>=(2, Y, true),
  2 #>= Y.

test('#>=_+_-_+_not_false', [ fail ]) :-
  #>=(2, Y, false),
  2 #>= Y.

test('#>=_+_-_+_false') :-
  #>=(3, Y, false),
  3 #< Y.

test('#>=_+_-_+_not_true', [ fail ]) :-
  #>=(3, Y, true),
  3 #< Y.

test('#>=_+_-_+_eq_true') :-
  #>=(3, Y, true),
  3 #= Y.

test('#>=_+_-_+_eq_not_false', [ fail ]) :-
  #>=(3, Y, false),
  3 #= Y.

% + - -
test('#>=_+_-_-_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #>=(2, Y, Cond), Cond = true ), Ans),
  2 #>= Y,
  Ans = [Y-true].

test('#>=_+_-_-_not_false', [ fail ]) :-
  #>=(2, Y, Cond),
  Cond = false,
  2 #>= Y.

test('#>=_+_-_-_false') :-
  #>=(3, Y, Cond),
  Cond = false,
  3 #< Y.

test('#>=_+_-_-_not_true', [ fail ]) :-
  #>=(3, Y, Cond),
  Cond = true,
  3 #< Y.

test('#>=_+_-_-_eq_true') :-
  % Query order forces choice point
  findall(Y-Cond, ( #>=(3, Y, Cond), Cond = true ), Ans),
  Ans = [Y-true],
  3 #= Y.

test('#>=_+_-_-_eq_not_false', [ fail ]) :-
  #>=(3, Y, Cond),
  Cond = false,
  3 #= Y.

% - + +
test('#>=_-_+_+_true') :-
  #>=(X, 3, true),
  X #>= 3.

test('#>=_-_+_+_not_false', [ fail ]) :-
  #>=(X, 3, false),
  X #>= 3.

test('#>=_-_+_+_false') :-
  #>=(X, 3, false),
  X #< 3.

test('#>=_-_+_+_not_true', [ fail ]) :-
  #>=(X, 3, true),
  X #< 3.

test('#>=_-_+_+_eq_true') :-
  #>=(X, 3, true),
  X #= 3.

test('#>=_-_+_+_eq_not_false', [ fail ]) :-
  #>=(X, 3, false),
  X #= 3.

% - + -
test('#>=_-_+_-_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #>=(X, 3, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #>= 3.

test('#>=_-_+_-_not_false', [ fail ]) :-
  #>=(X, 3, Cond),
  Cond = false,
  X #>= 3.

test('#>=_-_+_-_false') :-
  #>=(X, 3, Cond),
  Cond = false,
  X #< 3.

test('#>=_-_+_-_not_true', [ fail ]) :-
  #>=(X, 3, Cond),
  Cond = true,
  X #< 3.

test('#>=_-_+_-_eq_true') :-
  % Query order forces choice point
  findall(X-Cond, ( #>=(X, 3, Cond), Cond = true ), Ans),
  Ans = [X-true],
  X #= 3.

test('#>=_-_+_-_eq_not_false', [ fail ]) :-
  #>=(X, 3, Cond),
  Cond = false,
  X #= 3.

% - - +
test('#>=_-_-_+_true') :-
  #>=(X, Y, true),
  X #>= Y.

test('#>=_-_-_+_not_false', [ fail ]) :-
  #>=(X, Y, false),
  X #>= Y,
  [X, Y] ins 0..1.

test('#>=_-_-_+_false') :-
  #>=(X, Y, false),
  X #< Y.

test('#>=_-_-_+_not_true', [ fail ]) :-
  #>=(X, Y, true),
  X #< Y,
  [X, Y] ins 0..1.

test('#>=_-_-_+_eq_true') :-
  #>=(X, Y, true),
  X #= Y.

test('#>=_-_-_+_eq_not_false', [ fail ]) :-
  #>=(X, Y, false),
  X #= Y.

% - - -
test('#>=_-_-_-_true') :-
  findall(X-Y-Cond, ( #>=(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #>= Y.

test('#>=_-_-_-_not_false', [ fail ]) :-
  #>=(X, Y, Cond),
  Cond = false,
  X #>= Y,
  [X, Y] ins 0..1.

test('#>=_-_-_-_false') :-
  #>=(X, Y, Cond),
  Cond = false,
  X #< Y.

test('#>=_-_-_-_not_true', [ fail ]) :-
  #>=(X, Y, Cond),
  Cond = true,
  X #< Y,
  [X, Y] ins 0..1.

test('#>=_-_-_-_eq_true') :-
  % Query order forces choice point
  findall(X-Y-Cond, ( #>=(X, Y, Cond), Cond = true ), Ans),
  Ans = [X-Y-true],
  X #= Y.

test('#>=_-_-_-_eq_not_false', [ fail ]) :-
  #>=(X, Y, Cond),
  Cond = false,
  X #= Y.


% Tests for ==

% + + +
test('==_+_+_+_true') :-
  ==(abc, abc, true).

test('==_+_+_+_not_false', [ fail ]) :-
  ==(abc, abc, false).

test('==_+_+_+_false') :-
  ==(abc, def, false).

test('==_+_+_+_not_true', [ fail ]) :-
  ==(abc, def, true).

% + + -
test('==_+_+_-_true') :-
  ==(abc, abc, Cond),
  Cond = true.

test('==_+_+_-_not_false', [ fail ]) :-
  ==(abc, abc, Cond),
  Cond = false.

test('==_+_+_-_false') :-
  ==(abc, def, Cond),
  Cond = false.

test('==_+_+_-_not_true', [ fail ]) :-
  ==(abc, def, Cond),
  Cond = true.

% + - +
test('==_+_-_+_false') :-
  ==(abc, _, false).

test('==_+_-_+_not_true', [ fail ]) :-
  ==(abc, _, true).

% + - -
test('==_+_-_-_false') :-
  ==(abc, _, Cond),
  Cond = false.

test('==_+_-_-_not_true', [ fail ]) :-
  ==(abc, _, Cond),
  Cond = true.

% - + +
test('==_-_+_+_false') :-
  ==(_, def, false).

test('==_-_+_+_not_true', [ fail ]) :-
  ==(_, def, true).

% - + -
test('==_-_+_-_false') :-
  ==(_, def, Cond),
  Cond = false.

test('==_-_+_-_not_true', [ fail ]) :-
  ==(_, def, Cond),
  Cond = true.

% - - +
test('==_-_-_+_false') :-
  ==(_, _, false).

test('==_-_-_+_not_true', [ fail ]) :-
  ==(_, _, true).

test('==_-_-_+_true') :-
  ==(X, X, true).

test('==_-_-_+_not_false', [ fail ]) :-
  ==(X, X, false).

% - - -
test('==_-_-_-_false') :-
  ==(_, _, Cond),
  Cond = false.

test('==_-_-_-_not_true', [ fail ]) :-
  ==(_, _, Cond),
  Cond = true.

test('==_-_-_-_true') :-
  ==(X, X, Cond),
  Cond = true.

test('==_-_-_-_not_false', [ fail ]) :-
  ==(X, X, Cond),
  Cond = false.


% Tests for \==

% + + +
test('\\==_+_+_+_false') :-
  \==(abc, abc, false).

test('\\==_+_+_+_not_true', [ fail ]) :-
  \==(abc, abc, true).

test('\\==_+_+_+_true') :-
  \==(abc, def, true).

test('\\==_+_+_+_not_false', [ fail ]) :-
  \==(abc, def, false).

% + + -
test('\\==_+_+_-_false') :-
  \==(abc, abc, Cond),
  Cond = false.

test('\\==_+_+_-_not_true', [ fail ]) :-
  \==(abc, abc, Cond),
  Cond = true.

test('\\==_+_+_-_true') :-
  \==(abc, def, Cond),
  Cond = true.

test('\\==_+_+_-_not_false', [ fail ]) :-
  \==(abc, def, Cond),
  Cond = false.

% + - +
test('\\==_+_-_+_true') :-
  \==(abc, _, true).

test('\\==_+_-_+_not_false', [ fail ]) :-
  \==(abc, _, false).

% + - -
test('\\==_+_-_-_true') :-
  \==(abc, _, Cond),
  Cond = true.

test('\\==_+_-_-_not_false', [ fail ]) :-
  \==(abc, _, Cond),
  Cond = false.

% - + +
test('\\==_-_+_+_true') :-
  \==(_, def, true).

test('\\==_-_+_+_not_false', [ fail ]) :-
  \==(_, def, false).

% - + -
test('\\==_-_+_-_true') :-
  \==(_, def, Cond),
  Cond = true.

test('\\==_-_+_-_not_false', [ fail ]) :-
  \==(_, def, Cond),
  Cond = false.

% - - +
test('\\==_-_-_+_true') :-
  \==(_, _, true).

test('\\==_-_-_+_not_false', [ fail ]) :-
  \==(_, _, false).

test('\\==_-_-_+_false') :-
  \==(X, X, false).

test('\\==_-_-_+_not_true', [ fail ]) :-
  \==(X, X, true).

% - - -
test('\\==_-_-_-_true') :-
  \==(_, _, Cond),
  Cond = true.

test('\\==_-_-_-_not_false', [ fail ]) :-
  \==(_, _, Cond),
  Cond = false.

test('\\==_-_-_-_false') :-
  \==(X, X, Cond),
  Cond = false.

test('\\==_-_-_-_not_true', [ fail ]) :-
  \==(X, X, Cond),
  Cond = true.


% Tests for @<

% + + +
test('@<_+_+_+_true') :-
  @<(a, b, true).

test('@<_+_+_+_not_false', [ fail ]) :-
  @<(a, b, false).

test('@<_+_+_+_false') :-
  @<(b, a, false).

test('@<_+_+_+_not_true', [ fail ]) :-
  @<(b, a, true).

test('@<_+_+_+_eq_false') :-
  @<(a, a, false).

test('@<_+_+_+_eq_not_true', [ fail ]) :-
  @<(a, a, true).

% + + -
test('@<_+_+_-_true') :-
  @<(a, b, Cond),
  Cond = true.

test('@<_+_+_-_not_false', [ fail ]) :-
  @<(a, b, Cond),
  Cond = false.

test('@<_+_+_-_false') :-
  @<(b, a, Cond),
  Cond = false.

test('@<_+_+_-_not_true', [ fail ]) :-
  @<(b, a, Cond),
  Cond = true.

test('@<_+_+_-_eq_false') :-
  @<(a, a, Cond),
  Cond = false.

test('@<_+_+_-_eq_not_true', [ fail ]) :-
  @<(a, a, Cond),
  Cond = true.

% + - +
test('@<_+_-_+_false') :-
  @<(a, _, false).

test('@<_+_-_+_not_true', [ fail ]) :-
  @<(a, _, true).

% + - -
test('@<_+_-_-_false') :-
  @<(a, _, Cond),
  Cond = false.

test('@<_+_-_-_not_true', [ fail ]) :-
  @<(a, _, Cond),
  Cond = true.

% - + +
test('@<_-_+_+_true') :-
  @<(_, b, true).

test('@<_-_+_+_not_false', [ fail ]) :-
  @<(_, b, false).

% - + -
test('@<_-_+_-_true') :-
  @<(_, b, Cond),
  Cond = true.

test('@<_-_+_-_not_false', [ fail ]) :-
  @<(_, b, Cond),
  Cond = false.

% - - +
test('@<_-_-_+_true') :-
  @<(_, _, true).

test('@<_-_-_+_not_false', [ fail ]) :-
  @<(_, _, false).

% - - -
test('@<_-_-_-_true') :-
  @<(_, _, Cond),
  Cond = true.

test('@<_-_-_-_not_false', [ fail ]) :-
  @<(_, _, Cond),
  Cond = false.


% Tests for @=<

% + + +
test('@=<_+_+_+_true') :-
  @=<(a, b, true).

test('@=<_+_+_+_not_false', [ fail ]) :-
  @=<(a, b, false).

test('@=<_+_+_+_false') :-
  @=<(b, a, false).

test('@=<_+_+_+_not_true', [ fail ]) :-
  @=<(b, a, true).

test('@=<_+_+_+_eq_true') :-
  @=<(a, a, true).

test('@=<_+_+_+_eq_not_false', [ fail ]) :-
  @=<(a, a, false).

% + + -
test('@=<_+_+_-_true') :-
  @=<(a, b, Cond),
  Cond = true.

test('@=<_+_+_-_not_false', [ fail ]) :-
  @=<(a, b, Cond),
  Cond = false.

test('@=<_+_+_-_false') :-
  @=<(b, a, Cond),
  Cond = false.

test('@=<_+_+_-_not_true', [ fail ]) :-
  @=<(b, a, Cond),
  Cond = true.

test('@=<_+_+_-_eq_true') :-
  @=<(a, a, Cond),
  Cond = true.

test('@=<_+_+_-_eq_not_false', [ fail ]) :-
  @=<(a, a, Cond),
  Cond = false.

% + - +
test('@=<_+_-_+_false') :-
  @=<(a, _, false).

test('@=<_+_-_+_not_true', [ fail ]) :-
  @=<(a, _, true).

% + - -
test('@=<_+_-_-_false') :-
  @=<(a, _, Cond),
  Cond = false.

test('@=<_+_-_-_not_true', [ fail ]) :-
  @=<(a, _, Cond),
  Cond = true.

% - + +
test('@=<_-_+_+_true') :-
  @=<(_, b, true).

test('@=<_-_+_+_not_false', [ fail ]) :-
  @=<(_, b, false).

% - + -
test('@=<_-_+_-_true') :-
  @=<(_, b, Cond),
  Cond = true.

test('@=<_-_+_-_not_false', [ fail ]) :-
  @=<(_, b, Cond),
  Cond = false.

% - - +
test('@=<_-_-_+_true') :-
  @=<(_, _, true).

test('@=<_-_-_+_not_false', [ fail ]) :-
  @=<(_, _, false).

% - - -
test('@=<_-_-_-_true') :-
  @=<(_, _, Cond),
  Cond = true.

test('@=<_-_-_-_not_false', [ fail ]) :-
  @=<(_, _, Cond),
  Cond = false.


% Tests for @>

% + + +
test('@>_+_+_+_true') :-
  @>(b, a, true).

test('@>_+_+_+_not_false', [ fail ]) :-
  @>(b, a, false).

test('@>_+_+_+_false') :-
  @>(a, b, false).

test('@>_+_+_+_not_true', [ fail ]) :-
  @>(a, b, true).

test('@>_+_+_+_eq_false') :-
  @>(a, a, false).

test('@>_+_+_+_eq_not_true', [ fail ]) :-
  @>(a, a, true).

% + + -
test('@>_+_+_-_true') :-
  @>(b, a, Cond),
  Cond = true.

test('@>_+_+_-_not_false', [ fail ]) :-
  @>(b, a, Cond),
  Cond = false.

test('@>_+_+_-_false') :-
  @>(a, b, Cond),
  Cond = false.

test('@>_+_+_-_not_true', [ fail ]) :-
  @>(a, b, Cond),
  Cond = true.

test('@>_+_+_-_eq_false') :-
  @>(a, a, Cond),
  Cond = false.

test('@>_+_+_-_eq_not_true', [ fail ]) :-
  @>(a, a, Cond),
  Cond = true.

% + - +
test('@>_+_-_+_true') :-
  @>(b, _, true).

test('@>_+_-_+_not_false', [ fail ]) :-
  @>(b, _, false).

% + - -
test('@>_+_-_-_true') :-
  @>(b, _, Cond),
  Cond = true.

test('@>_+_-_-_not_false', [ fail ]) :-
  @>(b, _, Cond),
  Cond = false.

% - + +
test('@>_-_+_+_false') :-
  @>(_, a, false).

test('@>_-_+_+_not_true', [ fail ]) :-
  @>(_, a, true).

% - + -
test('@>_-_+_-_false') :-
  @>(_, a, Cond),
  Cond = false.

test('@>_-_+_-_not_true', [ fail ]) :-
  @>(_, a, Cond),
  Cond = true.

% - - +
test('@>_-_-_+_false') :-
  @>(_, _, false).

test('@>_-_-_+_not_true', [ fail ]) :-
  @>(_, _, true).

% - - -
test('@>_-_-_-_false') :-
  @>(_, _, Cond),
  Cond = false.

test('@>_-_-_-_not_true', [ fail ]) :-
  @>(_, _, Cond),
  Cond = true.


% Tests for @>=

% + + +
test('@>=_+_+_+_true') :-
  @>=(b, a, true).

test('@>=_+_+_+_not_false', [ fail ]) :-
  @>=(b, a, false).

test('@>=_+_+_+_false') :-
  @>=(a, b, false).

test('@>=_+_+_+_not_true', [ fail ]) :-
  @>=(a, b, true).

test('@>=_+_+_+_eq_true') :-
  @>=(a, a, true).

test('@>=_+_+_+_eq_not_false', [ fail ]) :-
  @>=(a, a, false).

% + + -
test('@>=_+_+_-_true') :-
  @>=(b, a, Cond),
  Cond = true.

test('@>=_+_+_-_not_false', [ fail ]) :-
  @>=(b, a, Cond),
  Cond = false.

test('@>=_+_+_-_false') :-
  @>=(a, b, Cond),
  Cond = false.

test('@>=_+_+_-_not_true', [ fail ]) :-
  @>=(a, b, Cond),
  Cond = true.

test('@>=_+_+_-_eq_true') :-
  @>=(a, a, Cond),
  Cond = true.

test('@>=_+_+_-_eq_not_false', [ fail ]) :-
  @>=(a, a, Cond),
  Cond = false.

% + - +
test('@>=_+_-_+_true') :-
  @>=(a, _, true).

test('@>=_+_-_+_not_false', [ fail ]) :-
  @>=(a, _, false).

% + - -
test('@>=_+_-_-_true') :-
  @>=(a, _, Cond),
  Cond = true.

test('@>=_+_-_-_not_false', [ fail ]) :-
  @>=(a, _, Cond),
  Cond = false.

% - + +
test('@>=_-_+_+_false') :-
  @>=(_, b, false).

test('@>=_-_+_+_not_true', [ fail ]) :-
  @>=(_, b, true).

% - + -
test('@>=_-_+_-_false') :-
  @>=(_, b, Cond),
  Cond = false.

test('@>=_-_+_-_not_true', [ fail ]) :-
  @>=(_, b, Cond),
  Cond = true.

% - - +
test('@>=_-_-_+_false') :-
  @>=(_, _, false).

test('@>=_-_-_+_not_true', [ fail ]) :-
  @>=(_, _, true).

% - - -
test('@>=_-_-_-_false') :-
  @>=(_, _, Cond),
  Cond = false.

test('@>=_-_-_-_not_true', [ fail ]) :-
  @>=(_, _, Cond),
  Cond = true.


% Tests for $<

% + + +
test('$<_+_+_+_true') :-
  $<(a, b, true).

test('$<_+_+_+_not_false', [ fail ]) :-
  $<(a, b, false).

test('$<_+_+_+_false') :-
  $<(b, a, false).

test('$<_+_+_+_not_true', [ fail ]) :-
  $<(b, a, true).

test('$<_+_+_+_eq_false') :-
  $<(a, a, false).

test('$<_+_+_+_eq_not_true', [ fail ]) :-
  $<(a, a, true).

% + + -
test('$<_+_+_-_true') :-
  $<(a, b, Cond),
  Cond = true.

test('$<_+_+_-_not_false', [ fail ]) :-
  $<(a, b, Cond),
  Cond = false.

test('$<_+_+_-_false') :-
  $<(b, a, Cond),
  Cond = false.

test('$<_+_+_-_not_true', [ fail ]) :-
  $<(b, a, Cond),
  Cond = true.

test('$<_+_+_-_eq_false') :-
  $<(a, a, Cond),
  Cond = false.

test('$<_+_+_-_eq_not_true', [ fail ]) :-
  $<(a, a, Cond),
  Cond = true.

% + - +
test('$<_+_-_+_true') :-
  $<(a, Y, true),
  Y = b.

test('$<_+_-_+_not_false', [ fail ]) :-
  $<(a, Y, false),
  Y = b.

test('$<_+_-_+_false') :-
  $<(b, Y, false),
  Y = a.

test('$<_+_-_+_not_true', [ fail ]) :-
  $<(b, Y, true),
  Y = a.

test('$<_+_-_+_eq_false') :-
  $<(a, Y, false),
  Y = a.

test('$<_+_-_+_eq_not_true', [ fail ]) :-
  $<(a, Y, true),
  Y = a.

% + - -
test('$<_+_-_-_true') :-
  $<(a, Y, Cond),
  Y = b,
  Cond = true.

test('$<_+_-_-_not_false', [ fail ]) :-
  $<(a, Y, Cond),
  Y = b,
  Cond = false.

test('$<_+_-_-_false') :-
  $<(b, Y, Cond),
  Y = a,
  Cond = false.

test('$<_+_-_-_not_true', [ fail ]) :-
  $<(b, Y, Cond),
  Y = a,
  Cond = true.

test('$<_+_-_-_eq_false') :-
  $<(a, Y, Cond),
  Y = a,
  Cond = false.

test('$<_+_-_-_eq_not_true', [ fail ]) :-
  $<(a, Y, Cond),
  Y = a,
  Cond = true.

% - + +
test('$<_-_+_+_true') :-
  $<(X, b, true),
  X = a.

test('$<_-_+_+_not_false', [ fail ]) :-
  $<(X, b, false),
  X = a.

test('$<_-_+_+_false') :-
  $<(X, a, false),
  X = b.

test('$<_-_+_+_not_true', [ fail ]) :-
  $<(X, a, true),
  X = b.

test('$<_-_+_+_eq_false') :-
  $<(X, a, false),
  X = a.

test('$<_-_+_+_eq_not_true', [ fail ]) :-
  $<(X, a, true),
  X = a.

% - + -
test('$<_-_+_-_true') :-
  $<(X, b, Cond),
  X = a,
  Cond = true.

test('$<_-_+_-_not_false', [ fail ]) :-
  $<(X, b, Cond),
  X = a,
  Cond = false.

test('$<_-_+_-_false') :-
  $<(X, a, Cond),
  X = b,
  Cond = false.

test('$<_-_+_-_not_true', [ fail ]) :-
  $<(X, a, Cond),
  X = b,
  Cond = true.

test('$<_-_+_-_eq_false') :-
  $<(X, a, Cond),
  X = a,
  Cond = false.

test('$<_-_+_-_eq_not_true', [ fail ]) :-
  $<(X, a, Cond),
  X = a,
  Cond = true.

% - - +
test('$<_-_-_+_true') :-
  $<(X, Y, true),
  X = a, Y = b.

test('$<_-_-_+_not_false', [ fail ]) :-
  $<(X, Y, false),
  X = a, Y = b.

test('$<_-_-_+_false') :-
  $<(X, Y, false),
  X = b, Y = a.

test('$<_-_-_+_not_true', [ fail ]) :-
  $<(X, Y, true),
  X = b, Y = a.

test('$<_-_-_+_eq_false') :-
  $<(X, X, false).

test('$<_-_-_+_eq_not_true', [ fail ]) :-
  $<(X, X, true).

% - - -
test('$<_-_-_-_true') :-
  $<(X, Y, Cond),
  X = a, Y = b,
  Cond = true.

test('$<_-_-_-_not_false', [ fail ]) :-
  $<(X, Y, Cond),
  X = a, Y = b,
  Cond = false.

test('$<_-_-_-_false') :-
  $<(X, Y, Cond),
  X = b, Y = a,
  Cond = false.

test('$<_-_-_-_not_true', [ fail ]) :-
  $<(X, Y, Cond),
  X = b, Y = a,
  Cond = true.

test('$<_-_-_-_eq_false') :-
  $<(X, X, Cond),
  Cond = false.

test('$<_-_-_-_eq_not_true', [ fail ]) :-
  $<(X, X, Cond),
  Cond = true.


% Tests for $=<

% + + +
test('$=<_+_+_+_true') :-
  $=<(a, b, true).

test('$=<_+_+_+_not_false', [ fail ]) :-
  $=<(a, b, false).

test('$=<_+_+_+_false') :-
  $=<(b, a, false).

test('$=<_+_+_+_not_true', [ fail ]) :-
  $=<(b, a, true).

test('$=<_+_+_+_eq_true') :-
  $=<(a, a, true).

test('$=<_+_+_+_eq_not_false', [ fail ]) :-
  $=<(a, a, false).

% + + -
test('$=<_+_+_-_true') :-
  $=<(a, b, Cond),
  Cond = true.

test('$=<_+_+_-_not_false', [ fail ]) :-
  $=<(a, b, Cond),
  Cond = false.

test('$=<_+_+_-_false') :-
  $=<(b, a, Cond),
  Cond = false.

test('$=<_+_+_-_not_true', [ fail ]) :-
  $=<(b, a, Cond),
  Cond = true.

test('$=<_+_+_-_eq_true') :-
  $=<(a, a, Cond),
  Cond = true.

test('$=<_+_+_-_eq_not_false', [ fail ]) :-
  $=<(a, a, Cond),
  Cond = false.

% + - +
test('$=<_+_-_+_true') :-
  $=<(a, Y, true),
  Y = b.

test('$=<_+_-_+_not_false', [ fail ]) :-
  $=<(a, Y, false),
  Y = b.

test('$=<_+_-_+_false') :-
  $=<(b, Y, false),
  Y = a.

test('$=<_+_-_+_not_true', [ fail ]) :-
  $=<(b, Y, true),
  Y = a.

test('$=<_+_-_+_eq_true') :-
  $=<(a, Y, true),
  Y = a.

test('$=<_+_-_+_eq_not_false', [ fail ]) :-
  $=<(a, Y, false),
  Y = a.

% + - -
test('$=<_+_-_-_true') :-
  $=<(a, Y, Cond),
  Y = b,
  Cond = true.

test('$=<_+_-_-_not_false', [ fail ]) :-
  $=<(a, Y, Cond),
  Y = b,
  Cond = false.

test('$=<_+_-_-_false') :-
  $=<(b, Y, Cond),
  Y = a,
  Cond = false.

test('$=<_+_-_-_not_true', [ fail ]) :-
  $=<(b, Y, Cond),
  Y = a,
  Cond = true.

test('$=<_+_-_-_eq_true') :-
  $=<(a, Y, Cond),
  Y = a,
  Cond = true.

test('$=<_+_-_-_eq_not_false', [ fail ]) :-
  $=<(a, Y, Cond),
  Y = a,
  Cond = false.

% - + +
test('$=<_-_+_+_true') :-
  $=<(X, b, true),
  X = a.

test('$=<_-_+_+_not_false', [ fail ]) :-
  $=<(X, b, false),
  X = a.

test('$=<_-_+_+_false') :-
  $=<(X, a, false),
  X = b.

test('$=<_-_+_+_not_true', [ fail ]) :-
  $=<(X, a, true),
  X = b.

test('$=<_-_+_+_eq_true') :-
  $=<(X, a, true),
  X = a.

test('$=<_-_+_+_eq_not_false', [ fail ]) :-
  $=<(X, a, false),
  X = a.

% - + -
test('$=<_-_+_-_true') :-
  $=<(X, b, Cond),
  X = a,
  Cond = true.

test('$=<_-_+_-_not_false', [ fail ]) :-
  $=<(X, b, Cond),
  X = a,
  Cond = false.

test('$=<_-_+_-_false') :-
  $=<(X, a, Cond),
  X = b,
  Cond = false.

test('$=<_-_+_-_not_true', [ fail ]) :-
  $=<(X, a, Cond),
  X = b,
  Cond = true.

test('$=<_-_+_-_eq_true') :-
  $=<(X, a, Cond),
  X = a,
  Cond = true.

test('$=<_-_+_-_eq_not_false', [ fail ]) :-
  $=<(X, a, Cond),
  X = a,
  Cond = false.

% - - +
test('$=<_-_-_+_true') :-
  $=<(X, Y, true),
  X = a, Y = b.

test('$=<_-_-_+_not_false', [ fail ]) :-
  $=<(X, Y, false),
  X = a, Y = b.

test('$=<_-_-_+_false') :-
  $=<(X, Y, false),
  X = b, Y = a.

test('$=<_-_-_+_not_true', [ fail ]) :-
  $=<(X, Y, true),
  X = b, Y = a.

test('$=<_-_-_+_eq_true') :-
  $=<(X, X, true).

test('$=<_-_-_+_eq_not_false', [ fail ]) :-
  $=<(X, X, false).

% - - -
test('$=<_-_-_-_true') :-
  $=<(X, Y, Cond),
  X = a, Y = b,
  Cond = true.

test('$=<_-_-_-_not_false', [ fail ]) :-
  $=<(X, Y, Cond),
  X = a, Y = b,
  Cond = false.

test('$=<_-_-_-_false') :-
  $=<(X, Y, Cond),
  X = b, Y = a,
  Cond = false.

test('$=<_-_-_-_not_true', [ fail ]) :-
  $=<(X, Y, Cond),
  X = b, Y = a,
  Cond = true.

test('$=<_-_-_-_eq_true') :-
  $=<(X, X, Cond),
  Cond = true.

test('$=<_-_-_-_eq_not_false', [ fail ]) :-
  $=<(X, X, Cond),
  Cond = false.


% Tests for $>

% + + +
test('$>_+_+_+_true') :-
  $>(b, a, true).

test('$>_+_+_+_not_false', [ fail ]) :-
  $>(b, a, false).

test('$>_+_+_+_false') :-
  $>(a, b, false).

test('$>_+_+_+_not_true', [ fail ]) :-
  $>(a, b, true).

test('$>_+_+_+_eq_false') :-
  $>(a, a, false).

test('$>_+_+_+_eq_not_true', [ fail ]) :-
  $>(a, a, true).

% + + -
test('$>_+_+_-_true') :-
  $>(b, a, Cond),
  Cond = true.

test('$>_+_+_-_not_false', [ fail ]) :-
  $>(b, a, Cond),
  Cond = false.

test('$>_+_+_-_false') :-
  $>(a, b, Cond),
  Cond = false.

test('$>_+_+_-_not_true', [ fail ]) :-
  $>(a, b, Cond),
  Cond = true.

test('$>_+_+_-_eq_false') :-
  $>(a, a, Cond),
  Cond = false.

test('$>_+_+_-_eq_not_true', [ fail ]) :-
  $>(a, a, Cond),
  Cond = true.

% + - +
test('$>_+_-_+_true') :-
  $>(b, Y, true),
  Y = a.

test('$>_+_-_+_not_false', [ fail ]) :-
  $>(b, Y, false),
  Y = a.

test('$>_+_-_+_false') :-
  $>(a, Y, false),
  Y = b.

test('$>_+_-_+_not_true', [ fail ]) :-
  $>(a, Y, true),
  Y = b.

test('$>_+_-_+_eq_false') :-
  $>(a, Y, false),
  Y = a.

test('$>_+_-_+_eq_not_true', [ fail ]) :-
  $>(a, Y, true),
  Y = a.

% + - -
test('$>_+_-_-_true') :-
  $>(b, Y, Cond),
  Y = a,
  Cond = true.

test('$>_+_-_-_not_false', [ fail ]) :-
  $>(b, Y, Cond),
  Y = a,
  Cond = false.

test('$>_+_-_-_false') :-
  $>(a, Y, Cond),
  Y = b,
  Cond = false.

test('$>_+_-_-_not_true', [ fail ]) :-
  $>(a, Y, Cond),
  Y = b,
  Cond = true.

test('$>_+_-_-_eq_false') :-
  $>(a, Y, Cond),
  Y = a,
  Cond = false.

test('$>_+_-_-_eq_not_true', [ fail ]) :-
  $>(a, Y, Cond),
  Y = a,
  Cond = true.

% - + +
test('$>_-_+_+_true') :-
  $>(X, a, true),
  X = b.

test('$>_-_+_+_not_false', [ fail ]) :-
  $>(X, a, false),
  X = b.

test('$>_-_+_+_false') :-
  $>(X, b, false),
  X = a.

test('$>_-_+_+_not_true', [ fail ]) :-
  $>(X, b, true),
  X = a.

test('$>_-_+_+_eq_false') :-
  $>(X, a, false),
  X = a.

test('$>_-_+_+_eq_not_true', [ fail ]) :-
  $>(X, a, true),
  X = a.

% - + -
test('$>_-_+_-_true') :-
  $>(X, a, Cond),
  X = b,
  Cond = true.

test('$>_-_+_-_not_false', [ fail ]) :-
  $>(X, a, Cond),
  X = b,
  Cond = false.

test('$>_-_+_-_false') :-
  $>(X, b, Cond),
  X = a,
  Cond = false.

test('$>_-_+_-_not_true', [ fail ]) :-
  $>(X, b, Cond),
  X = a,
  Cond = true.

test('$>_-_+_-_eq_false') :-
  $>(X, a, Cond),
  X = a,
  Cond = false.

test('$>_-_+_-_eq_not_true', [ fail ]) :-
  $>(X, a, Cond),
  X = a,
  Cond = true.

% - - +
test('$>_-_-_+_true') :-
  $>(X, Y, true),
  X = b, Y = a.

test('$>_-_-_+_not_false', [ fail ]) :-
  $>(X, Y, false),
  X = b, Y = a.

test('$>_-_-_+_false') :-
  $>(X, Y, false),
  X = a, Y = b.

test('$>_-_-_+_not_true', [ fail ]) :-
  $>(X, Y, true),
  X = a, Y = b.

test('$>_-_-_+_eq_false') :-
  $>(X, X, false).

test('$>_-_-_+_eq_not_true', [ fail ]) :-
  $>(X, X, true).

% - - -
test('$>_-_-_-_true') :-
  $>(X, Y, Cond),
  X = b, Y = a,
  Cond = true.

test('$>_-_-_-_not_false', [ fail ]) :-
  $>(X, Y, Cond),
  X = b, Y = a,
  Cond = false.

test('$>_-_-_-_false') :-
  $>(X, Y, Cond),
  X = a, Y = b,
  Cond = false.

test('$>_-_-_-_not_true', [ fail ]) :-
  $>(X, Y, Cond),
  X = a, Y = b,
  Cond = true.

test('$>_-_-_-_eq_false') :-
  $>(X, X, Cond),
  Cond = false.

test('$>_-_-_-_eq_not_true', [ fail ]) :-
  $>(X, X, Cond),
  Cond = true.


% Tests for $>=

% + + +
test('$>=_+_+_+_true') :-
  $>=(b, a, true).

test('$>=_+_+_+_not_false', [ fail ]) :-
  $>=(b, a, false).

test('$>=_+_+_+_false') :-
  $>=(a, b, false).

test('$>=_+_+_+_not_true', [ fail ]) :-
  $>=(a, b, true).

test('$>=_+_+_+_eq_true') :-
  $>=(a, a, true).

test('$>=_+_+_+_eq_not_false', [ fail ]) :-
  $>=(a, a, false).

% + + -
test('$>=_+_+_-_true') :-
  $>=(b, a, Cond),
  Cond = true.

test('$>=_+_+_-_not_false', [ fail ]) :-
  $>=(b, a, Cond),
  Cond = false.

test('$>=_+_+_-_false') :-
  $>=(a, b, Cond),
  Cond = false.

test('$>=_+_+_-_not_true', [ fail ]) :-
  $>=(a, b, Cond),
  Cond = true.

test('$>=_+_+_-_eq_true') :-
  $>=(a, a, Cond),
  Cond = true.

test('$>=_+_+_-_eq_not_false', [ fail ]) :-
  $>=(a, a, Cond),
  Cond = false.

% + - +
test('$>=_+_-_+_true') :-
  $>=(b, Y, true),
  Y = a.

test('$>=_+_-_+_not_false', [ fail ]) :-
  $>=(b, Y, false),
  Y = a.

test('$>=_+_-_+_false') :-
  $>=(a, Y, false),
  Y = b.

test('$>=_+_-_+_not_true', [ fail ]) :-
  $>=(a, Y, true),
  Y = b.

test('$>=_+_-_+_eq_true') :-
  $>=(a, Y, true),
  Y = a.

test('$>=_+_-_+_eq_not_false', [ fail ]) :-
  $>=(a, Y, false),
  Y = a.

% + - -
test('$>=_+_-_-_true') :-
  $>=(b, Y, Cond),
  Y = a,
  Cond = true.

test('$>=_+_-_-_not_false', [ fail ]) :-
  $>=(b, Y, Cond),
  Y = a,
  Cond = false.

test('$>=_+_-_-_false') :-
  $>=(a, Y, Cond),
  Y = b,
  Cond = false.

test('$>=_+_-_-_not_true', [ fail ]) :-
  $>=(a, Y, Cond),
  Y = b,
  Cond = true.

test('$>=_+_-_-_eq_true') :-
  $>=(a, Y, Cond),
  Y = a,
  Cond = true.

test('$>=_+_-_-_eq_not_false', [ fail ]) :-
  $>=(a, Y, Cond),
  Y = a,
  Cond = false.

% - + +
test('$>=_-_+_+_true') :-
  $>=(X, a, true),
  X = b.

test('$>=_-_+_+_not_false', [ fail ]) :-
  $>=(X, a, false),
  X = b.

test('$>=_-_+_+_false') :-
  $>=(X, b, false),
  X = a.

test('$>=_-_+_+_not_true', [ fail ]) :-
  $>=(X, b, true),
  X = a.

test('$>=_-_+_+_eq_true') :-
  $>=(X, a, true),
  X = a.

test('$>=_-_+_+_eq_not_false', [ fail ]) :-
  $>=(X, a, false),
  X = a.

% - + -
test('$>=_-_+_-_true') :-
  $>=(X, a, Cond),
  X = b,
  Cond = true.

test('$>=_-_+_-_not_false', [ fail ]) :-
  $>=(X, a, Cond),
  X = b,
  Cond = false.

test('$>=_-_+_-_false') :-
  $>=(X, b, Cond),
  X = a,
  Cond = false.

test('$>=_-_+_-_not_true', [ fail ]) :-
  $>=(X, b, Cond),
  X = a,
  Cond = true.

test('$>=_-_+_-_eq_true') :-
  $>=(X, a, Cond),
  X = a,
  Cond = true.

test('$>=_-_+_-_eq_not_false', [ fail ]) :-
  $>=(X, a, Cond),
  X = a,
  Cond = false.

% - - +
test('$>=_-_-_+_true') :-
  $>=(X, Y, true),
  X = b, Y = a.

test('$>=_-_-_+_not_false', [ fail ]) :-
  $>=(X, Y, false),
  X = b, Y = a.

test('$>=_-_-_+_false') :-
  $>=(X, Y, false),
  X = a, Y = b.

test('$>=_-_-_+_not_true', [ fail ]) :-
  $>=(X, Y, true),
  X = a, Y = b.

test('$>=_-_-_+_eq_true') :-
  $>=(X, X, true).

test('$>=_-_-_+_eq_not_false', [ fail ]) :-
  $>=(X, X, false).

% - - -
test('$>=_-_-_-_true') :-
  $>=(X, Y, Cond),
  X = b, Y = a,
  Cond = true.

test('$>=_-_-_-_not_false', [ fail ]) :-
  $>=(X, Y, Cond),
  X = b, Y = a,
  Cond = false.

test('$>=_-_-_-_false') :-
  $>=(X, Y, Cond),
  X = a, Y = b,
  Cond = false.

test('$>=_-_-_-_not_true', [ fail ]) :-
  $>=(X, Y, Cond),
  X = a, Y = b,
  Cond = true.

test('$>=_-_-_-_eq_true') :-
  $>=(X, X, Cond),
  Cond = true.

test('$>=_-_-_-_eq_not_false', [ fail ]) :-
  $>=(X, X, Cond),
  Cond = false.


% Tests for term_indomain
test('term_indomain_+_+_fails', [ fail ]) :-
  term_indomain(1, all_terms).

test('term_indomain_+_-_fails', [ fail ]) :-
  term_indomain(1, _).

test('term_indomain_-_+_succeeds') :-
  term_indomain(Term, all_terms),
  get_attr(Term, term_order, all_terms).

test('term_indomain_-_+_put_twice_unifies', [ nondet ]) :-
  term_indomain(Term, terms_from(const(1))),
  term_indomain(Term, terms_to(const(2))),
  get_attr(Term, term_order, [const(1), const(2)]). 

test('term_indomain_-_-_succeeds') :-
  put_attr(Term, term_order, all_terms),
  term_indomain(Term, Dom),
  Dom == all_terms.

test('term_indomain_-_-_put_get') :-
  term_indomain(Term, all_terms),
  term_indomain(Term, Dom),
  Dom == all_terms.


% Tests for is_term
test('is_term_-_get') :-
  is_term(Term),
  get_attr(Term, term_order, all_terms).


% Tests for term_at_least
test('term_at_least_-_+') :-
  term_at_least(Term, x),
  get_attr(Term, term_order, terms_from(const(x))).

test('term_at_least_-_-') :-
  term_at_least(Term, X),
  get_attr(Term, term_order, terms_from(variable(X))).


% Tests for term_at_most
test('term_at_most_-_+') :-
  term_at_most(Term, y),
  get_attr(Term, term_order, terms_to(const(y))).

test('term_at_most_-_-') :-
  term_at_most(Term, Y),
  get_attr(Term, term_order, terms_to(variable(Y))).


% Tests for term_normalized
test('term_normalized_c_c') :-
  term_normalized(const(a), const(a)).

test('term_normalized_c_-') :-
  term_normalized(const(a), Term),
  Term == const(a).

test('term_normalized_v_c') :-
  X = a,
  term_normalized(variable(X), const(a)).

test('term_normalized_v_-') :-
  X = a,
  term_normalized(variable(X), Term),
  Term == const(a).

test('term_normalized_v_v') :-
  term_normalized(variable(X), variable(X)).

test('term_normalized_v_v_-') :-
  term_normalized(variable(X), Term),
  Term == variable(X).


% Tests for dom_normalized
test('dom_normalized_all_terms_+_+') :-
  dom_normalized(all_terms, all_terms).

test('dom_normalized_all_terms_+_-') :-
  dom_normalized(all_terms, Dom),
  Dom == all_terms.

test('dom_normalized_terms_from_+_+_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), terms_from(Term)).

test('dom_normalized_terms_from_+_+_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), terms_from(Term)).

test('dom_normalized_terms_from_+_+_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), terms_from(Term)).

test('dom_normalized_terms_from_+_-_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_from_+_-_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_from_+_-_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(terms_from(Term0), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_to_+_+_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), terms_to(Term)).

test('dom_normalized_terms_to_+_+_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), terms_to(Term)).

test('dom_normalized_terms_to_+_+_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), terms_to(Term)).

test('dom_normalized_terms_to_+_-_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), Dom),
  Dom == terms_to(Term).

test('dom_normalized_terms_to_+_-_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), Dom),
  Dom == terms_to(Term).

test('dom_normalized_terms_to_+_-_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(terms_to(Term0), Dom),
  Dom == terms_to(Term).

test('dom_normalized_singleton_+_+_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), singleton(Term)).

test('dom_normalized_singleton_+_+_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), singleton(Term)).

test('dom_normalized_singleton_+_+_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), singleton(Term)).

test('dom_normalized_singleton_+_-_c') :-
  Term0 = const(a),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), Dom),
  Dom == singleton(Term).

test('dom_normalized_singleton_+_-_v') :-
  Term0 = variable(_),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), Dom),
  Dom == singleton(Term).

test('dom_normalized_singleton_+_-_v_to_c') :-
  X = a,
  Term0 = variable(X),
  term_normalized(Term0, Term),
  dom_normalized(singleton(Term0), Dom),
  Dom == singleton(Term).

test('dom_normalized_interval_+_+_c_c') :-
  TermX0 = const(a),
  TermY0 = const(b),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_c_v') :-
  TermX0 = const(a),
  TermY0 = variable(_),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_v_c') :-
  TermX0 = variable(_),
  TermY0 = const(b),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_v_v') :-
  TermX0 = variable(_),
  TermY0 = variable(_),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_v_v2c') :-
  TermX0 = variable(_),
  TermY0 = variable(Y),
  Y = b,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_v2c_v') :-
  TermX0 = variable(X),
  TermY0 = variable(_),
  X = a,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_+_v2c_v2c') :-
  TermX0 = variable(X),
  TermY0 = variable(Y),
  X = a,
  Y = b,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], [TermX, TermY]).

test('dom_normalized_interval_+_-_c_c') :-
  TermX0 = const(a),
  TermY0 = const(b),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_c_v') :-
  TermX0 = const(a),
  TermY0 = variable(_),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_v_c') :-
  TermX0 = variable(_),
  TermY0 = const(b),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_v_v') :-
  TermX0 = variable(_),
  TermY0 = variable(_),
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_v_v2c') :-
  TermX0 = variable(_),
  TermY0 = variable(Y),
  Y = b,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_v2c_v') :-
  TermX0 = variable(X),
  TermY0 = variable(_),
  X = a,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_-_v2c_v2c') :-
  TermX0 = variable(X),
  TermY0 = variable(Y),
  X = a,
  Y = b,
  term_normalized(TermX0, TermX),
  term_normalized(TermY0, TermY),
  dom_normalized([TermX0, TermY0], Dom),
  Dom == [TermX, TermY].

test('dom_normalized_interval_+_+_c_c_sing') :-
  dom_normalized([const(a), const(a)], singleton(const(a))).

test('dom_normalized_interval_+_+_c_v2c_sing') :-
  Y = a,
  dom_normalized([const(a), variable(Y)], singleton(const(a))).

test('dom_normalized_interval_+_+_v2c_c_sing') :-
  X = a,
  dom_normalized([variable(X), const(a)], singleton(const(a))).

test('dom_normalized_interval_+_+_v2c_v2c_sing') :-
  X = a, Y = a,
  dom_normalized([variable(X), variable(Y)], singleton(const(a))).

test('dom_normalized_interval_+_-_c_c_sing') :-
  dom_normalized([const(a), const(a)], Dom),
  Dom = singleton(const(a)).

test('dom_normalized_interval_+_-_c_v2c_sing') :-
  Y = a,
  dom_normalized([const(a), variable(Y)], Dom),
  Dom = singleton(const(a)).

test('dom_normalized_interval_+_-_v2c_c_sing') :-
  X = a,
  dom_normalized([variable(X), const(a)], Dom),
  Dom = singleton(const(a)).

test('dom_normalized_interval_+_-_v2c_v2c_sing') :-
  X = a, Y = a,
  dom_normalized([variable(X), variable(Y)], Dom),
  Dom = singleton(const(a)).


% Tests for terms_intersection (from-from case)
test('terms_from_from_intersection_c_c_>=', [nondet]) :-
  terms_intersection(terms_from(const(y)), terms_from(const(x)), terms_from(const(y))).

test('terms_from_from_intersection_c_c_<', [nondet]) :-
  terms_intersection(terms_from(const(x)), terms_from(const(y)), terms_from(const(y))).

test('terms_from_from_intersection_c_v') :-
  findall(Intersection, terms_intersection(terms_from(const(x)), terms_from(variable(Y)), Intersection), Ans),
  Ans = [terms_from(const(x)), terms_from(variable(Y))].

test('terms_from_from_intersection_v_c') :-
  findall(Intersection, terms_intersection(terms_from(variable(X)), terms_from(const(y)), Intersection), Ans),
  Ans = [terms_from(variable(X)), terms_from(const(y))].

test('terms_from_from_intersection_v_v') :-
  findall(Intersection, terms_intersection(terms_from(variable(X)), terms_from(variable(Y)), Intersection), Ans),
  Ans = [terms_from(variable(X)), terms_from(variable(Y))].


% Tests for terms_intersection (to-to case)
test('terms_to_to_intersection_c_c_>=', [nondet]) :-
  terms_intersection(terms_to(const(y)), terms_to(const(x)), terms_to(const(x))).

test('terms_to_to_intersection_c_c_<', [nondet]) :-
  terms_intersection(terms_to(const(x)), terms_to(const(y)), terms_to(const(x))).

test('terms_to_to_intersection_c_v') :-
  findall(Intersection, terms_intersection(terms_to(const(x)), terms_to(variable(Y)), Intersection), Ans),
  Ans = [terms_to(const(x)), terms_to(variable(Y))].

test('terms_to_to_intersection_v_c') :-
  findall(Intersection, terms_intersection(terms_to(variable(X)), terms_to(const(y)), Intersection), Ans),
  Ans = [terms_to(variable(X)), terms_to(const(y))].

test('terms_to_to_intersection_v_v') :-
  findall(Intersection, terms_intersection(terms_to(variable(X)), terms_to(variable(Y)), Intersection), Ans),
  Ans = [terms_to(variable(X)), terms_to(variable(Y))].


% Tests for terms_intersection (from-to case)
test('terms_from_to_intersection_c_c_=', [nondet]) :-
  terms_intersection(terms_from(const(x)), terms_to(const(x)), singleton(const(x))).

test('terms_from_to_intersection_c_c_<', [nondet]) :-
  terms_intersection(terms_from(const(x)), terms_to(const(y)), [const(x), const(y)]).

test('terms_from_to_intersection_c_c_>', [nondet]) :-
  terms_intersection(terms_from(const(y)), terms_to(const(x)), empty).

test('terms_from_to_intersection_c_v') :-
  findall(Intersection, terms_intersection(terms_from(const(x)), terms_to(variable(Y)), Intersection), Ans),
  Ans = [singleton(const(x)), [const(x), variable(Y)], empty].

test('terms_from_to_intersection_c_v_unified', [nondet]) :-
  terms_intersection(terms_from(const(x)), terms_to(variable(Y)), singleton(const(x))),
  Y == x.

test('terms_from_to_intersection_v_c') :-
  findall(Intersection, terms_intersection(terms_from(variable(X)), terms_to(const(y)), Intersection), Ans),
  Ans = [singleton(const(y)), [variable(X), const(y)], empty].

test('terms_from_to_intersection_v_c_unified', [nondet]) :-
  terms_intersection(terms_from(variable(X)), terms_to(const(y)), singleton(const(y))), 
  X == y.

test('terms_from_to_intersection_v_v') :-
  findall(Intersection, terms_intersection(terms_from(variable(X)), terms_to(variable(Y)), Intersection), Ans),
  Ans = [singleton(variable(X)), [variable(X), variable(Y)], empty].

test('terms_from_to_intersection_v_v_unified', [nondet]) :-
  terms_intersection(terms_from(variable(X)), terms_to(variable(Y)), singleton(variable(X))),
  X == Y.


% Tests for terms_intersection (to-from case)
test('terms_to_from_intersection_agrees_with_from_to', [nondet]) :-
  terms_intersection(terms_to(variable(X)), terms_from(variable(Y)), Intersection),
  terms_intersection(terms_from(variable(Y)), terms_to(variable(X)), Intersection).


% Tests for terms_intersection (from-int case)
% | x R y | x R z | Out                      |
% |   ?   |   ?   |  [y, z]; [x, z]; [z]; [] |
% |   ?   |   <   |  [y, z]; [x, z]          |
% |   ?   |   =   |  [z]                     |
% |   ?   |   >   |  []                      |
% |   <   |   ?   |  [y,z]                   |
% |   <   |   <   |  [y,z]                   |
% |   =   |   <   |  [y,z]                   |
% |   >   |   ?   |  [x,z]; [z]; []          |
% |   >   |   <   |  [x,z]                   |
% |   >   |   =   |  [z]                     |
% |   >   |   >   |  []                      |
test('terms_from_int_intersection_?_?') :-
  findall(
    Intersection,
    terms_intersection(terms_from(const(a)), [variable(Y), variable(Z)], Intersection),
    Ans),
  Ans = [[variable(Y), variable(Z)], [const(a), variable(Z)], singleton(const(a)), empty].

test('terms_from_int_intersection_?_?_unified', [nondet]) :-
  terms_intersection(terms_from(const(a)), [variable(_), variable(Z)], singleton(const(a))),
  Z == a.

test('terms_from_int_intersection_?_<') :-
  findall(
    Intersection,
    terms_intersection(terms_from(const(a)), [variable(Y), const(c)], Intersection),
    Ans),
  Ans = [[variable(Y), const(c)], [const(a), const(c)]].

test('terms_from_int_intersection_?_=') :-
  terms_intersection(terms_from(const(a)), [variable(_), const(a)], singleton(const(a))).

test('terms_from_int_intersection_?_>') :-
  terms_intersection(terms_from(const(b)), [variable(_), const(a)], empty).

test('terms_from_int_intersection_<_?') :-
  terms_intersection(terms_from(const(a)), [const(b), variable(Z)], [const(b), variable(Z)]).

test('terms_from_int_intersection_<_<') :-
  terms_intersection(terms_from(const(a)), [const(b), const(c)], [const(b), const(c)]).

test('terms_from_int_intersection_=_<') :-
  terms_intersection(terms_from(const(a)), [const(a), const(b)], [const(a), const(b)]).

test('terms_from_int_intersection_>_?') :-
  findall(
    Intersection,
    terms_intersection(terms_from(const(b)), [const(a), variable(Z)], Intersection),
    Ans),
  Ans = [[const(b), variable(Z)], singleton(const(b)), empty].

test('terms_from_int_intersection_>_?_unified', [nondet]) :-
  terms_intersection(terms_from(const(b)), [const(a), variable(Z)], singleton(const(b))),
  Z == b.

test('terms_from_int_intersection_>_<') :-
  terms_intersection(terms_from(const(b)), [const(a), const(c)], [const(b), const(c)]).

test('terms_from_int_intersection_>_=') :-
  terms_intersection(terms_from(const(b)), [const(a), const(b)], singleton(const(b))).

test('terms_from_int_intersection_>_>') :-
  terms_intersection(terms_from(const(c)), [const(a), const(b)], empty).


% Tests for terms_intersection (int-from case)
test('terms_int_from_intersection_agrees_with_from_int', [nondet]) :-
    terms_intersection([X, Y], terms_from(Z), Intersection),
    terms_intersection(terms_from(Z), [X, Y], Intersection).

% Tests for terms_intersection (to-int case)
% | x R y | x R z | Out                   |
% |   ?   |   ?   | [y,z]; [y,x]; [y]; [] |
% |   ?   |   <   | [y,x]; [y]; []        |
% |   ?   |   =   | [y,z]                 |
% |   ?   |   >   | [y,z]                 |
% |   <   |   ?   | []                    |
% |   <   |   <   | []                    |
% |   =   |   <   | [y]                   |
% |   >   |   ?   | [y,x]; [y,z]          |
% |   >   |   <   | [y,x]                 |
% |   >   |   =   | [y,z]                 |
% |   >   |   >   | [y,z]                 |
test('terms_to_int_intersection_?_?') :-
  findall(
    Intersection,
    terms_intersection(terms_to(const(a)), [variable(Y), variable(Z)], Intersection),
    Ans),
  Ans = [[variable(Y), variable(Z)], [variable(Y), const(a)], singleton(const(a)), empty].

test('terms_to_int_intersection_?_?_unified', [nondet]) :-
  terms_intersection(terms_to(const(a)), [variable(Y), variable(_)], singleton(const(a))),
  Y == a.

test('terms_to_int_intersection_?_<') :-
  findall(
    Intersection,
    terms_intersection(terms_to(const(a)), [variable(Y), const(c)], Intersection),
    Ans),
  Ans = [[variable(Y), const(a)], singleton(const(a)), empty].

test('terms_to_int_intersection_?_<_unified', [nondet]) :-
  terms_intersection(terms_to(const(a)), [variable(Y), const(c)], singleton(const(a))),
  Y == a.

test('terms_to_int_intersection_?_=') :-
  terms_intersection(terms_to(const(a)), [variable(Y), const(a)], [variable(Y), const(a)]).

test('terms_to_int_intersection_?_>') :-
  terms_intersection(terms_to(const(b)), [variable(Y), const(a)], [variable(Y), const(a)]).

test('terms_to_int_intersection_<_?') :-
  terms_intersection(terms_to(const(a)), [const(b), variable(_)], empty).

test('terms_to_int_intersection_<_<') :-
  terms_intersection(terms_to(const(a)), [const(b), const(c)], empty).

test('terms_to_int_intersection_=_<') :-
  terms_intersection(terms_to(const(a)), [const(a), const(b)], singleton(const(a))).

test('terms_to_int_intersection_>_?') :-
  findall(
    Intersection,
    terms_intersection(terms_to(const(b)), [const(a), variable(Z)], Intersection),
    Ans),
  Ans = [[const(a), const(b)], [const(a), variable(Z)]].

test('terms_to_int_intersection_>_<') :-
  terms_intersection(terms_to(const(b)), [const(a), const(c)], [const(a), const(b)]).

test('terms_to_int_intersection_>_=') :-
  terms_intersection(terms_to(const(b)), [const(a), const(b)], [const(a), const(b)]).

test('terms_to_int_intersection_>_>') :-
  terms_intersection(terms_to(const(c)), [const(a), const(b)], [const(a), const(b)]).


% Tests for terms_intersection (int-to case)
test('terms_int_to_intersection_agrees_with_to_int', [nondet]) :-
  terms_intersection([X, Y], terms_to(Z), Intersection),
  terms_intersection(terms_to(Z), [X, Y], Intersection).


% Tests for terms_intersection (int-int case)
% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |     |     |  <  |     | empty                      | [xy][zw]
% |     |  >  |     |     | empty                      | [zw][xy]
% |     |     |  =  |     | [y]                        | [x[y]w]
% |     |  =  |     |     | [x]                        | [z[x]y]
test('terms_int_int_intersection_y<z') :-
  terms_intersection([variable(_), const(b)], [const(c), variable(_)], empty).

test('terms_int_int_intersection_x>w') :-
  terms_intersection([const(b), variable(_)], [variable(_), const(a)], empty).

test('terms_int_int_intersection_y=z') :-
  terms_intersection([variable(_), const(b)], [const(b), variable(_)], singleton(const(b))).

test('terms_int_int_intersection_x=w') :-
  terms_intersection([const(a), variable(_)], [variable(_), const(a)], singleton(const(a))).

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  <  |  <  |  >  |  <  | [z,y]                      | [x[zy]w]
% |  <  |  <  |  >  |  =  | [z,y]                      | [x[zy]y]
% |  <  |  <  |  >  |  >  | [z,w]                      | [x[zw]y]
test('terms_int_int_intersection_<_<_>_<') :-
  terms_intersection([const(a), const(c)], [const(b), const(d)], [const(b), const(c)]).

test('terms_int_int_intersection_<_<_>_=') :-
  terms_intersection([const(a), const(c)], [const(b), const(c)], [const(b), const(c)]).

test('terms_int_int_intersection_<_<_>_>') :-
  terms_intersection([const(a), const(d)], [const(b), const(c)], [const(b), const(c)]).

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  <  |  <  |  ?  |  ?  | [z,w]; [z,y]; [z]; empty   | [x[zw]y] or [x[zy]w] or [xy][zw]
test('terms_int_int_intersection_<_<_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), variable(Y)], [const(b), const(c)], Intersection),
    Ans),
  Ans = [[const(b), const(c)], [const(b), variable(Y)], singleton(const(b)), empty].

test('terms_int_int_intersection_<_<_?_?_unified', [nondet]) :-
  terms_intersection([const(a), variable(Y)], [const(b), const(c)], singleton(const(b))),
  Y == b.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  <  |  ?  |  >  |  ?  | [z,w]; [z,y]               | [x[zw]y] or [x[zy]w]
test('terms_int_int_intersection_<_?_>_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), const(c)], [const(b), variable(W)], Intersection),
    Ans),
  Ans = [[const(b), variable(W)], [const(b), const(c)]].

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  <  |  ?  |  ?  |  ?  | [z,w]; [z,y]; [y]; empty   | [x[zw]y] or [x[zy]w] or [xy][zw]
test('terms_int_int_intersection_<_?_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), variable(Y)], [const(c), variable(W)], Intersection),
    Ans),
  Ans = [[const(c), variable(W)], [const(c), variable(Y)], singleton(const(c)), empty].

test('terms_int_int_intersection_<_?_?_?_unified', [nondet]) :-
  terms_intersection([const(a), variable(Y)], [const(c), variable(_)], singleton(const(c))),
  Y == c.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  =  |  <  |  >  |  <  | [z,y]                      | [[zy]w]
% |  =  |  <  |  >  |  =  | [z,y]                      | [[zy]y]
% |  =  |  <  |  >  |  >  | [z,w]                      | [[zw]y]
test('terms_int_int_intersection_=_<_>_<') :-
  terms_intersection([const(a), const(b)], [const(a), const(d)], [const(a), const(b)]).

test('terms_int_int_intersection_=_<_>_=') :-
  terms_intersection([const(a), const(d)], [const(a), const(d)], [const(a), const(d)]).

test('terms_int_int_intersection_=_<_>_>') :-
  terms_intersection([const(a), const(d)], [const(a), const(c)], [const(a), const(c)]).

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  =  |  <  |  ?  |  ?  | [z,w]; [z,y]               | [[zw]y] or [[zy]w]
test('terms_int_int_intersection_=_<_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), variable(Y)], [const(a), const(d)], Intersection),
    Ans),
  Ans = [[const(a), const(d)], [const(a), variable(Y)]].

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  =  |  ?  |  >  |  ?  | [z,w]; [z,y]               | [[zw]y] or [[zy]w]
test('terms_int_int_intersection_=_?_>_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), const(b)], [const(a), variable(W)], Intersection),
    Ans),
  Ans = [[const(a), variable(W)], [const(a), const(b)]].

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  =  |  ?  |  ?  |  ?  | [z,w]; [z,y]               | [[zw]y] or [[zy]w]
test('terms_int_int_intersection_=_?_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), variable(Y)], [const(a), variable(W)], Intersection),
    Ans),
  Ans = [[const(a), variable(W)], [const(a), variable(Y)]].

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  >  |  <  |  >  |  <  | [x,y]                      | [z[xy]w]
% |  >  |  <  |  >  |  =  | [x,y]                      | [z[xy]y]
% |  >  |  <  |  >  |  >  | [x,w]                      | [z[xw]y]
test('terms_int_int_intersection_>_<_>_<') :-
  terms_intersection([const(b), const(c)], [const(a), const(d)], [const(b), const(c)]).

test('terms_int_int_intersection_>_<_>_=') :-
  terms_intersection([const(b), const(c)], [const(a), const(c)], [const(b), const(c)]).

test('terms_int_int_intersection_>_<_>_>') :-
  terms_intersection([const(b), const(d)], [const(a), const(c)], [const(b), const(c)]).

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  >  |  <  |  ?  |  ?  | [x,w]; [x,y]               | [z[xw]y] or [z[xy]w]
test('terms_int_int_intersection_>_<_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(c), variable(Y)], [const(a), const(d)], Intersection),
    Ans),
  Ans = [[const(c), const(d)], [const(c), variable(Y)]].

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  >  |  ?  |  >  |  ?  | [x,w]; [x,y]; [x]; empty   | [z[xw]y] or [z[xy]w] or [zw][xy]
test('terms_int_int_intersection_>_?_>_?') :-
  findall(
    Intersection,
    terms_intersection([const(b), const(c)], [const(a), variable(W)], Intersection),
    Ans),
  Ans = [[const(b), variable(W)], [const(b), const(c)], singleton(const(b)), empty].

test('terms_int_int_intersection_>_?_>_?_unified', [nondet]) :-
  terms_intersection([const(b), const(c)], [const(a), variable(W)], singleton(const(b))),
  W == b.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  >  |  ?  |  ?  |  ?  | [x,w]; [x,y]; [x]; empty   | [z[xw]y] or [z[xy]w] or [zw][xy]
test('terms_int_int_intersection_>_?_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(c), variable(Y)], [const(a), variable(W)], Intersection),
    Ans),
  Ans = [[const(c), variable(W)], [const(c), variable(Y)], singleton(const(c)), empty].

test('terms_int_int_intersection_>_?_?_?_unified', [nondet]) :-
  terms_intersection([const(c), variable(_)], [const(a), variable(W)], singleton(const(c))),
  W == c.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  ?  |  <  |  ?  |  <  | [z,y]; [x,y]; [y]; empty   | [z[xy]w] or [x[zy]w] or [xy][zw]
% |  ?  |  <  |  ?  |  =  | [z,y]; [x,y]               | [z[xy]] or [x[zy]]
% |  ?  |  <  |  ?  |  >  | [z,w]; [x,w]               | [z[xw]y] or [x[zw]y]
% |  ?  |  <  |  ?  |  ?  | [z,w]; [x,w]; [z,y];       |
%                         | [x,y]; [y]; empty          |
test('terms_int_int_intersection_?_<_?_<') :-
  findall(
    Intersection,
    terms_intersection([const(a), const(b)], [variable(Z), const(d)], Intersection),
    Ans),
  Ans = [[variable(Z), const(b)], [const(a), const(b)], singleton(const(b)), empty].

test('terms_int_int_intersection_?_<_?_<_unified', [nondet]) :-
  terms_intersection([const(a), const(b)], [variable(Z), const(d)], singleton(const(b))),
  Z == b.

test('terms_int_int_intersection_?_<_?_=') :-
  findall(
    Intersection,
    terms_intersection([const(a), const(b)], [variable(Z), const(b)], Intersection),
    Ans),
  Ans = [[variable(Z), const(b)], [const(a), const(b)]].

test('terms_int_int_intersection_?_<_?_>') :-
  findall(
    Intersection,
    terms_intersection([const(a), const(c)], [variable(Z), const(b)], Intersection),
    Ans),
  Ans = [[variable(Z), const(b)], [const(a), const(b)]].

test('terms_int_int_intersection_?_<_?_?') :-
  findall(
    Intersection,
    terms_intersection([const(a), variable(Y)], [variable(Z), const(b)], Intersection),
    Ans),
  Ans = [[variable(Z), const(b)], [const(a), const(b)], [variable(Z), variable(Y)],
         [const(a), variable(Y)], singleton(variable(Y)), empty].

test('terms_int_int_intersection_?_<_?_?_v', [nondet]) :-
  terms_intersection([const(a), variable(Y)], [variable(Z), const(b)], singleton(variable(Y))),
  Y == Z.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  ?  |  ?  |  >  |  <  | [z,y]; [x,y]               | [x[zy]w] or [z[xy]w]
% |  ?  |  ?  |  >  |  =  | [z,y]; [x,y]               | [x[zy]] or [z[xy]]
% |  ?  |  ?  |  >  |  >  | [z,w]; [x,w]; [x]; empty   | [x[zw]y] or [z[xw]y] or [zw][xy]
% |  ?  |  ?  |  >  |  ?  | [z,w]; [x,w]; [z,y];       |
%                         | [x,y]; [x]; empty          |
test('terms_int_int_intersection_?_?_>_<') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(c)], [const(b), const(d)], Intersection),
    Ans),
  Ans = [[const(b), const(c)], [variable(X), const(c)]].

test('terms_int_int_intersection_?_?_>_=') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(c)], [const(b), const(c)], Intersection),
    Ans),
  Ans = [[const(b), const(c)], [variable(X), const(c)]].

test('terms_int_int_intersection_?_?_>_>') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(d)], [const(b), const(c)], Intersection),
    Ans),
  Ans = [[const(b), const(c)], [variable(X), const(c)], singleton(const(c)), empty].

test('terms_int_int_intersection_?_?_>_>_X==c', [nondet]) :-
  terms_intersection([variable(X), const(d)], [const(b), const(c)], singleton(const(c))),
  X == c.

test('terms_int_int_intersection_?_?_>_?') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(c)], [const(b), variable(W)], Intersection),
    Ans),
  Ans = [[const(b), variable(W)], [variable(X), variable(W)], [const(b), const(c)],
         [variable(X), const(c)], singleton(variable(X)), empty].

test('terms_int_int_intersection_?_?_>_?_v', [nondet]) :-
  terms_intersection([variable(X), const(c)], [const(b), variable(W)], singleton(variable(X))),
  X == W.

% | xRz | xRw | yRz | yRw | Out                        | Analysis
% |  ?  |  ?  |  ?  |  <  | [x,y]; [z,y]; [y]; empty   | [z[xy]w] or [x[zy]w] or [xy][zw]
% |  ?  |  ?  |  ?  |  =  | [x,y]; [z,y]               | [x[zy]] or [z[xy]]
% |  ?  |  ?  |  ?  |  >  | [z,w]; [x,w]; [x]; empty   | [x[zw]y] or [z[xw]y] or [zw][xy]
% |  ?  |  ?  |  ?  |  ?  | [z,w]; [x,w]; [z,y];       |
%                         | [x,y]; [x]; [y]; empty     |
test('terms_int_int_intersection_?_?_?_<') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(b)], [variable(Z), const(d)], Intersection),
    Ans),
  Ans = [[variable(X), const(b)], [variable(Z), const(b)], singleton(const(b)), empty].

test('terms_int_int_intersection_?_?_?_<_Z==b', [nondet]) :-
  terms_intersection([variable(_), const(b)], [variable(Z), const(d)], singleton(const(b))),
  Z == b.

test('terms_int_int_intersection_?_?_?_=') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(b)], [variable(Z), const(b)], Intersection),
    Ans),
  Ans = [[variable(X), const(b)], [variable(Z), const(b)]].

test('terms_int_int_intersection_?_?_?_>') :-
  findall(
    Intersection,
    terms_intersection([variable(X), const(d)], [variable(Z), const(b)], Intersection),
    Ans),
  Ans = [[variable(Z), const(b)], [variable(X), const(b)], singleton(const(b)), empty].

test('terms_int_int_intersection_?_?_?_>_X==b', [nondet]) :-
  terms_intersection([variable(X), const(d)], [variable(_), const(b)], singleton(const(b))),
  X == b.

test('terms_int_int_intersection_?_?_?_?') :-
  findall(
    Intersection,
    terms_intersection([variable(X), variable(Y)], [variable(Z), variable(W)], Intersection),
    Ans),
  Ans = [[variable(Z), variable(W)], [variable(X), variable(W)], [variable(Z), variable(Y)],
         [variable(X), variable(Y)], singleton(variable(X)), singleton(variable(Y)), empty].

test('terms_int_int_intersection_?_?_?_?_x==w', [nondet]) :-
  terms_intersection([variable(X), variable(_)], [variable(_), variable(W)], singleton(variable(X))),
  X == W.

test('terms_int_int_intersection_?_?_?_?_y==z', [nondet]) :-
  terms_intersection([variable(_), variable(Y)], [variable(Z), variable(_)], singleton(variable(Y))),
  Y == Z.


% Tests for terms_singleton_singleton_intersection
%test('terms_singleton_singleton_intersection_c_c_equal') :-
  %terms_singleton_singleton_intersection(const(a), const(a), singleton(const(a))).

%test('terms_singleton_singleton_intersection_c_c_unequal') :-
  %terms_singleton_singleton_intersection(const(a), const(b), empty).

%test('terms_singleton_singleton_intersection_c_v_equal') :-
  %terms_singleton_singleton_intersection(const(a), variable(Y), singleton(const(a))),
  %Y == a.

%test('terms_singleton_singleton_intersection_c_v_unequal') :-
  %dif(Y, a),
  %terms_singleton_singleton_intersection(const(a), variable(Y), empty).

%test('terms_singleton_singleton_intersection_v_c_equal') :-
  %terms_singleton_singleton_intersection(variable(X), const(b), singleton(const(b))),
  %X == b.

%test('terms_singleton_singleton_intersection_v_c_unequal') :-
  %dif(X, b),
  %terms_singleton_singleton_intersection(variable(X), const(b), empty).

%test('terms_singleton_singleton_intersection_v_v_equal') :-
  %terms_singleton_singleton_intersection(variable(X), variable(Y), singleton(variable(X))),
  %X == Y.

%test('terms_singleton_singleton_intersection_v_v_unequal') :-
  %dif(X, Y),
  %terms_singleton_singleton_intersection(variable(X), variable(Y), empty).


% Tests for terms_singleton_from_intersection
%test('terms_singleton_from_intersection_c_c_equal') :-
  %terms_singleton_from_intersection(const(a), const(a), singleton(const(a))).

%test('terms_singleton_from_intersection_c_c_lt') :-
  %terms_singleton_from_intersection(const(a), const(b), empty).

%test('terms_singleton_from_intersection_c_c_gt') :-
  %terms_singleton_from_intersection(const(b), const(a), singleton(const(b))).

%test('terms_singleton_from_intersection_c_v') :-
  %findall(
    %Intersection,
    %terms_singleton_from_intersection(const(a), variable(_), Intersection),
    %Ans),
  %Ans = [singleton(const(a)), empty].

%test('terms_singleton_from_intersection_v_c') :-
  %findall(
    %Intersection,
    %terms_singleton_from_intersection(variable(X), const(b), Intersection),
    %Ans),
  %Ans = [singleton(variable(X)), empty].

%test('terms_singleton_from_intersection_v_v') :-
  %findall(
    %Intersection,
    %terms_singleton_from_intersection(variable(X), variable(_), Intersection),
    %Ans),
  %Ans = [singleton(variable(X)), empty].


% Tests for terms_singleton_to_intersection
%test('terms_singleton_to_intersection_c_c_equal') :-
  %terms_singleton_to_intersection(const(a), const(a), singleton(const(a))).

%test('terms_singleton_to_intersection_c_c_lt') :-
  %terms_singleton_to_intersection(const(a), const(b), singleton(const(a))).

%test('terms_singleton_to_intersection_c_c_gt') :-
  %terms_singleton_to_intersection(const(b), const(a), empty).

%test('terms_singleton_to_intersection_c_v') :-
  %findall(
    %Intersection,
    %terms_singleton_to_intersection(const(a), variable(_), Intersection),
    %Ans),
  %Ans = [singleton(const(a)), empty].

%test('terms_singleton_to_intersection_v_c') :-
  %findall(
    %Intersection,
    %terms_singleton_to_intersection(variable(X), const(b), Intersection),
    %Ans),
  %Ans = [singleton(variable(X)), empty].

%test('terms_singleton_to_intersection_v_v') :-
  %findall(
    %Intersection,
    %terms_singleton_to_intersection(variable(X), variable(_), Intersection),
    %Ans),
  %Ans = [singleton(variable(X)), empty].


% Tests for terms_singleton_int_intersection
% | xRy | xRz | Out
% |  <  |     | empty
% |  =  |     | [x]
% |  >  |  <  | [x]
% |  >  |  =  | [x]
% |  >  |  >  | empty
% |  >  |  ?  | [x] or empty
% |  ?  |  <  | [x] or empty 
% |  ?  |  =  | [x]
% |  ?  |  >  | empty
% |  ?  |  ?  | [x] or empty

%test('terms_singleton_int_intersection_<') :-
  %terms_singleton_int_intersection(const(a), [const(b), variable(_)], empty).

%test('terms_singleton_int_intersection_=') :-
  %terms_singleton_int_intersection(const(a), [const(a), variable(_)], singleton(const(a))).

%test('terms_singleton_int_intersection_>_<') :-
  %terms_singleton_int_intersection(const(b), [const(a), const(c)], singleton(const(b))).

%test('terms_singleton_int_intersection_>_=') :-
  %terms_singleton_int_intersection(const(b), [const(a), const(b)], singleton(const(b))).

%test('terms_singleton_int_intersection_>_>') :-
  %terms_singleton_int_intersection(const(c), [const(a), const(b)], empty).

%test('terms_singleton_int_intersection_>_?') :-
  %findall(
    %Intersection,
    %terms_singleton_int_intersection(const(b), [const(a), variable(_)], Intersection),
    %Ans),
  %Ans = [singleton(const(b)), empty].

%test('terms_singleton_int_intersection_?_<') :-
  %findall(
    %Intersection,
    %terms_singleton_int_intersection(const(a), [variable(_), const(c)], Intersection),
    %Ans),
  %Ans = [singleton(const(a)), empty].

%test('terms_singleton_int_intersection_?_=') :-
  %terms_singleton_int_intersection(const(b), [variable(_), const(b)], singleton(const(b))).
  
%test('terms_singleton_int_intersection_?_=') :-
  %terms_singleton_int_intersection(const(b), [variable(_), const(b)], singleton(const(b))).

%test('terms_singleton_int_intersection_?_>') :-
  %terms_singleton_int_intersection(const(c), [variable(_), const(b)], empty).
  
%test('terms_singleton_int_intersection_?_?_cvv') :-
  %findall(
    %Intersection,
    %terms_singleton_int_intersection(const(a), [variable(_), variable(_)], Intersection),
    %Ans),
  %Ans = [singleton(const(a)), empty].

%test('terms_singleton_int_intersection_?_?_vcc') :-
  %findall(
    %Intersection,
    %terms_singleton_int_intersection(variable(X), [const(b), const(c)], Intersection),
    %Ans),
  %Ans = [singleton(variable(X)), empty].


% Tests for terms_dom_intersection 
test('terms_dom_intersection_allterms_allterms') :-
  terms_dom_intersection(all_terms, all_terms, all_terms).

test('terms_dom_intersection_allterms_singleton') :-
  terms_dom_intersection(all_terms, singleton(X), singleton(X)).

test('terms_dom_intersection_allterms_termsfrom') :-
  terms_dom_intersection(all_terms, terms_from(variable(X)), terms_from(variable(X))).

test('terms_dom_intersection_allterms_termsto') :-
  terms_dom_intersection(all_terms, terms_to(variable(X)), terms_to(variable(X))).

test('terms_dom_intersection_allterms_int') :-
  terms_dom_intersection(all_terms, [variable(X),variable(Y)], [variable(X),variable(Y)]).

test('terms_dom_intersection_termsfrom_allterms') :-
  terms_dom_intersection(terms_from(variable(X)), all_terms, terms_from(variable(X))).

test('terms_dom_intersection_termsfrom_singleton_c_c') :-
  terms_dom_intersection(terms_from(const(1)), singleton(const(2)), singleton(const(2))).

test('terms_dom_intersection_termsfrom_singleton_c_v') :-
  findall(Intersection, 
          terms_dom_intersection(terms_from(const(1)), singleton(variable(X)), Intersection),
          Ans),
  Ans = [singleton(variable(X)), empty].

test('terms_dom_intersection_termsfrom_singleton_v_c') :-
  findall(Intersection, 
          terms_dom_intersection(terms_from(variable(_)), singleton(variable(2)), Intersection),
          Ans),
  Ans = [singleton(const(2)), empty].

test('terms_dom_intersection_termsfrom_singleton_v_v') :-
  findall(Intersection, 
          terms_dom_intersection(terms_from(variable(_)), singleton(variable(Y)), Intersection),
          Ans),
  Ans = [singleton(variable(Y)), empty].

test('terms_dom_intersection_termsfrom_termsfrom', [nondet]) :-
  terms_dom_intersection(terms_from(variable(X)), terms_from(variable(Y)), Intersection),
  terms_intersection(terms_from(variable(X)), terms_from(variable(Y)), Intersection).

test('terms_dom_intersection_termsfrom_termsto', [nondet]) :-
  terms_dom_intersection(terms_from(variable(X)), terms_to(variable(Y)), Intersection),
  terms_intersection(terms_from(variable(X)), terms_to(variable(Y)), Intersection).

test('terms_dom_intersection_termsfrom_int', [nondet]) :-
  terms_dom_intersection(terms_from(variable(X)), [variable(Y),variable(Z)], Intersection),
  terms_intersection(terms_from(variable(X)), [variable(Y),variable(Z)], Intersection).

test('terms_dom_intersection_termsto_allterms') :-
  terms_dom_intersection(terms_to(variable(X)), all_terms, terms_to(variable(X))).

test('terms_dom_intersection_termsto_singleton_c_c') :-
  terms_dom_intersection(terms_to(const(2)), singleton(const(1)), singleton(const(1))).

test('terms_dom_intersection_termsto_singleton_c_v') :-
  findall(Intersection, 
          terms_dom_intersection(terms_to(const(1)), singleton(variable(X)), Intersection),
          Ans),
  Ans = [singleton(variable(X)), empty].

test('terms_dom_intersection_termsto_singleton_v_c') :-
  findall(Intersection, 
          terms_dom_intersection(terms_to(variable(_)), singleton(const(1)), Intersection),
          Ans),
  Ans = [singleton(const(1)), empty].

test('terms_dom_intersection_termsto_singleton_v_v') :-
  findall(Intersection, 
          terms_dom_intersection(terms_to(variable(_)), singleton(variable(Y)), Intersection),
          Ans),
  Ans = [singleton(variable(Y)), empty].

test('terms_dom_intersection_termsto_termsfrom', [nondet]) :-
  terms_dom_intersection(terms_to(variable(X)), terms_from(variable(Y)), Intersection),
  terms_intersection(terms_to(variable(X)), terms_from(variable(Y)), Intersection).

test('terms_dom_intersection_termsto_termsto', [nondet]) :-
  terms_dom_intersection(terms_to(variable(X)), terms_to(variable(Y)), Intersection),
  terms_intersection(terms_to(variable(X)), terms_to(variable(Y)), Intersection).

test('terms_dom_intersection_termsto_int', [nondet]) :-
  terms_dom_intersection(terms_to(variable(X)), [variable(Y),variable(Z)], Intersection),
  terms_intersection(terms_to(variable(X)), [variable(Y),variable(Z)], Intersection).

test('terms_dom_intersection_int_allterms') :-
  terms_dom_intersection([variable(X),variable(Y)], all_terms, [variable(X),variable(Y)]).

test('terms_dom_intersection_int_singleton_[c,c]_c') :-
  terms_dom_intersection([const(1), const(3)], singleton(const(2)), singleton(const(2))). 

test('terms_dom_intersection_int_singleton_[c,c]_v') :-
  findall(Intersection,
          terms_dom_intersection([const(1), const(3)], singleton(variable(Z)), Intersection),
          Ans),
  Ans = [singleton(variable(Z)), empty].

test('terms_dom_intersection_int_singleton_[c,v]_c') :-
  findall(Intersection,
          terms_dom_intersection([const(1), variable(_)], singleton(const(3)), Intersection),
          Ans),
  Ans = [singleton(const(3)), empty].

test('terms_dom_intersection_int_singleton_[c,v]_v') :-
  findall(Intersection,
          terms_dom_intersection([const(1), variable(_)], singleton(variable(Z)), Intersection),
          Ans),
  Ans = [singleton(variable(Z)), empty].

test('terms_dom_intersection_int_singleton_[v,c]_c') :-
  findall(Intersection,
          terms_dom_intersection([variable(_), const(3)], singleton(const(1)), Intersection),
          Ans),
  Ans = [singleton(const(1)), empty].

test('terms_dom_intersection_int_singleton_[v,c]_v') :-
  findall(Intersection,
          terms_dom_intersection([variable(_), const(3)], singleton(variable(Z)), Intersection),
          Ans),
  Ans = [singleton(variable(Z)), empty].

test('terms_dom_intersection_int_singleton_[v,v]_c') :-
  findall(Intersection,
          terms_dom_intersection([variable(_), variable(_)], singleton(const(2)), Intersection),
          Ans),
  Ans = [singleton(const(2)), empty].

test('terms_dom_intersection_int_singleton_[v,v]_v') :-
  findall(Intersection,
          terms_dom_intersection([variable(_), variable(_)], singleton(variable(Z)), Intersection),
          Ans),
  Ans = [singleton(variable(Z)), empty].

test('terms_dom_intersection_int_termsfrom', [nondet]) :-
  terms_dom_intersection([variable(X),variable(Y)], terms_from(variable(Z)), Intersection),
  terms_intersection(terms_from(variable(Z)), [variable(X),variable(Y)], Intersection).

test('terms_dom_intersection_int_termsto', [nondet]) :-
  terms_dom_intersection([variable(X),variable(Y)], terms_to(variable(Z)), Intersection),
  terms_intersection(terms_to(variable(Z)), [variable(X),variable(Y)], Intersection). 

test('terms_dom_intersection_int_int', [nondet]) :-
  terms_dom_intersection([variable(X),variable(Y)], [variable(Z),variable(W)], Intersection),
  terms_intersection([variable(X),variable(Y)], [variable(Z),variable(W)], Intersection).

% TODO: terms_dom_intersection_singleton_ cases


test('terms_dom_intersection_normalizes_allterms_termsfrom') :-
  X = 42, 
  terms_dom_intersection(all_terms, terms_from(variable(X)), terms_from(const(X))). 

test('terms_dom_intersection_normalizes_allterms_termsto') :-
  X = 42, 
  terms_dom_intersection(all_terms, terms_to(variable(X)), terms_to(const(X))). 

test('terms_dom_intersection_normalizes_allterms_int') :-
  X = 42, Y = 43, 
  terms_dom_intersection(all_terms, [variable(X), variable(Y)], [const(X), const(Y)]). 

test('terms_dom_intersection_normalizes_termsfrom_allterms') :-
  X = 42, 
  terms_dom_intersection(terms_from(variable(X)), all_terms, terms_from(const(X))). 

test('terms_dom_intersection_normalizes_termsfrom_termsfrom', [nondet]) :-
  X = 42, Y = 43, 
  terms_dom_intersection(terms_from(variable(X)), terms_from(variable(Y)), terms_from(const(Y))). 

test('terms_dom_intersection_normalizes_termsfrom_termsto', [nondet]) :-
  X = 42, Y = 43, 
  terms_dom_intersection(terms_from(variable(X)), terms_to(variable(Y)), [const(X), const(Y)]). 

test('terms_dom_intersection_normalizes_termsfrom_int') :-
  X = 42, Y = 43, Z = 44,
  terms_dom_intersection(terms_from(variable(X)), [variable(Y), variable(Z)], [const(Y), const(Z)]). 

test('terms_dom_intersection_normalizes_termsto_allterms') :-
  X = 42, 
  terms_dom_intersection(terms_to(variable(X)), all_terms, terms_to(const(X))). 

test('terms_dom_intersection_normalizes_termsto_termsfrom', [nondet]) :-
  X = 43, Y = 42, 
  terms_dom_intersection(terms_to(variable(X)), terms_from(variable(Y)), [const(Y), const(X)]). 

test('terms_dom_intersection_normalizes_termsto_termsto', [nondet]) :-
  X = 42, Y = 43, 
  terms_dom_intersection(terms_to(variable(X)), terms_to(variable(Y)), terms_to(const(X))). 

test('terms_dom_intersection_normalizes_termsto_int') :-
  X = 44, Y = 42, Z = 43,
  terms_dom_intersection(terms_to(variable(X)), [variable(Y), variable(Z)], [const(Y), const(Z)]). 

test('terms_dom_intersection_normalizes_int_allterms') :-
  X = 42, Y = 43, 
  terms_dom_intersection([variable(X), variable(Y)], all_terms, [const(X), const(Y)]). 

test('terms_dom_intersection_normalizes_int_termsfrom', [nondet]) :-
  X = 42, Y = 43, Z = 41, 
  terms_dom_intersection([variable(X), variable(Y)], terms_from(variable(Z)), [const(X), const(Y)]). 

test('terms_dom_intersection_normalizes_int_termsto', [nondet]) :-
  X = 42, Y = 43, Z = 44, 
  terms_dom_intersection([variable(X), variable(Y)], terms_to(variable(Z)), [const(X), const(Y)]). 

test('terms_dom_intersection_normalizes_int_int') :-
  X = 42, Y = 44, Z = 43, W = 45,
  terms_dom_intersection([variable(X), variable(Y)], [variable(Z), variable(W)], [const(Z), const(Y)]). 


% Tests for the unification hook

test('unify_empty_intersection_fails_to_unify', [ fail ]) :-
  term_at_least(X, 2),
  term_at_most(Y, 1),
  X = Y.

test('unify_singleton_intersection_sets_exact_value_and_removes_attributes_c', [ nondet ]) :-
  term_at_least(X, 2),
  term_at_most(Y, 2),
  X = Y,
  X == 2,
  Y == 2,
  \+ get_attr(X, term_order, _),
  \+ get_attr(Y, term_order, _).

test('unify_singleton_intersection2_sets_exact_value_and_removes_attributes_c') :-
  term_indomain(X, [const(1), const(2)]),
  term_indomain(Y, [const(2), const(3)]),
  X = Y,
  X == 2,
  Y == 2,
  \+ get_attr(X, term_order, _),
  \+ get_attr(Y, term_order, _).

test('unify_singleton_intersection_sets_exact_value_and_removes_attributes_v', [ nondet ]) :-
  term_at_least(X, Z),
  term_at_most(Y, Z),
  X = Y,
  X == Z,
  Y == Z,
  \+ get_attr(X, term_order, _),
  \+ get_attr(Y, term_order, _).

test('unify_singleton_intersection2_sets_exact_value_and_removes_attributes_v', [ nondet ]) :-
  term_indomain(X, [const(1), variable(Z)]),
  term_indomain(Y, [variable(Z), const(3)]),
  X = Y,
  X == Z,
  Y == Z,
  \+ get_attr(X, term_order, _),
  \+ get_attr(Y, term_order, _).

test('unify_allterms_allterms') :-
  is_term(X),
  is_term(Y),
  X = Y,
  get_attr(X, term_order, all_terms),
  get_attr(X, term_order, all_terms).

test('unify_allterms_termsfrom_c', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, 1), 
  X = Y,
  get_attr(X, term_order, terms_from(const(1))).

test('unify_allterms_termsfrom_v', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, Z), 
  X = Y,
  get_attr(X, term_order, terms_from(variable(Z))).

test('unify_allterms_termsto_c', [ nondet ]) :-
  is_term(X),
  term_at_most(Y, 1), 
  X = Y,
  get_attr(X, term_order, terms_to(const(1))).

test('unify_allterms_termsto_v', [ nondet ]) :-
  is_term(X),
  term_at_most(Y, Z), 
  X = Y,
  get_attr(X, term_order, terms_to(variable(Z))).

test('unify_allterms_int_c_c', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, 1), term_at_most(Y, 2),
  X = Y,
  get_attr(X, term_order, [const(1), const(2)]).

test('unify_allterms_int_c_v', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, 1), term_at_most(Y, U),
  X = Y,
  get_attr(X, term_order, [const(1), variable(U)]).

test('unify_allterms_int_v_c', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, L), term_at_most(Y, 2),
  X = Y,
  get_attr(X, term_order, [variable(L), const(2)]).

test('unify_allterms_int_v_v', [ nondet ]) :-
  is_term(X),
  term_at_least(Y, L), term_at_most(Y, U),
  X = Y,
  get_attr(X, term_order, [variable(L), variable(U)]).

test('unify_termsfrom_allterms', [ nondet ]) :-
  term_at_least(X, 1),
  is_term(Y),
  X = Y,
  get_attr(Y, term_order, terms_from(const(1))).

test('unify_termsfrom_termsfrom_c_c', [ nondet ]) :-
  term_at_least(X, 1),
  term_at_least(Y, 2),
  X = Y,
  get_attr(Y, term_order, terms_from(const(2))).

test('unify_termsfrom_termsfrom_c_v', [ nondet ]) :-
  term_at_least(X, 1),
  term_at_least(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsfrom_termsfrom_v_c', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, 2),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsfrom_termsfrom_v_v', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsfrom_termsto_c_c', [ nondet ]) :-
  term_at_least(X, 1),
  term_at_most(Y, 2),
  X = Y,
  get_attr(Y, term_order, [const(1), const(2)]).

test('unify_termsfrom_termsto_c_v', [ nondet ]) :-
  term_at_least(X, 1),
  term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom).

test('unify_termsfrom_termsto_v_c', [ nondet ]) :-
  term_at_least(X, _),
  term_at_most(Y, 2),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom).

test('unify_termsfrom_termsto_v_v', [ nondet ]) :-
  term_at_least(X, _),
  term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom).

test('unify_termsfrom_int_c_[c,c]', [ nondet ]) :-
  term_at_least(X, 2),
  term_at_least(Y, 1), term_at_most(Y, 3),
  X = Y,
  get_attr(X, term_order, [const(2), const(3)]).
 
test('unify_termsfrom_int_c_[c,v]', [ nondet ]) :-
  term_at_least(X, 2),
  term_at_least(Y, 1), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_c_[v,c]', [ nondet ]) :-
  term_at_least(X, 2),
  term_at_least(Y, _), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_c_[v,v]', [ nondet ]) :-
  term_at_least(X, 2),
  term_at_least(Y, _), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_v_[c,c]', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, 1), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_v_[c,v]', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, 1), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_v_[v,c]', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, _), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsfrom_int_v_[v,v]', [ nondet ]) :-
  term_at_least(X, _),
  term_at_least(Y, _), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_allterms') :-
  term_at_most(X, 1),
  is_term(Y),
  X = Y,
  get_attr(Y, term_order, terms_to(const(1))).

test('unify_termsto_termsfrom_c_c', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, 1),
  X = Y,
  get_attr(Y, term_order, [const(1), const(2)]).

test('unify_termsto_termsfrom_c_v', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_termsfrom_v_c', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, 1),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_termsfrom_v_v', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_termsto_c_c', [ nondet ]) :-
  term_at_most(X, 1),
  term_at_most(Y, 2),
  X = Y,
  get_attr(Y, term_order, terms_to(const(1))).

test('unify_termsto_termsto_c_v', [ nondet ]) :-
  term_at_most(X, 1),
  term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_termsto_v_c', [ nondet ]) :-
  term_at_most(X, _),
  term_at_most(Y, 2),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_termsto_v_v', [ nondet ]) :-
  term_at_most(X, _),
  term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  \+ \+ (terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom)).

test('unify_termsto_int_c_[c,c]', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, 1), term_at_most(Y, 3),
  X = Y,
  get_attr(X, term_order, [const(1), const(2)]).
 
test('unify_termsto_int_c_[c,v]', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, 1), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_c_[v,c]', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, _), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_c_[v,v]', [ nondet ]) :-
  term_at_most(X, 2),
  term_at_least(Y, _), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_v_[c,c]', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, 1), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_v_[c,v]', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, 1), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_v_[v,c]', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, _), term_at_most(Y, 3),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

test('unify_termsto_int_v_[v,v]', [ nondet ]) :-
  term_at_most(X, _),
  term_at_least(Y, _), term_at_most(Y, _),
  get_attr(X, term_order, Dom1),
  get_attr(Y, term_order, Dom2),
  X = Y,
  terms_intersection(Dom1, Dom2, NewDom), get_attr(Y, term_order, NewDom). 

% interval domains with variable endpoints can collapse to singletons
% this means that terms_dom_intersection must handle these properly


:- end_tests(reif_utils).

