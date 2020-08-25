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
test('term_indomain_-_+_put_get') :-
  term_indomain(Term, all_terms),
  get_attr(Term, term_order, all_terms).

test('term_indomain_-_+_put_put_get') :-
  term_indomain(Term, bad_terms),
  term_indomain(Term, all_terms),
  get_attr(Term, term_order, all_terms).


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
  term_normalized(const(3), const(3)).

test('term_normalized_c_-') :-
  term_normalized(const(3), Term),
  Term = const(3).

test('term_normalized_v_c') :-
  X = 3,
  term_normalized(variable(X), const(3)).

test('term_normalized_v_-') :-
  X = 3,
  term_normalized(variable(X), Term),
  Term = const(3).

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
  term_normalized(const(3), Term),
  dom_normalized(terms_from(const(3)), terms_from(Term)).

test('dom_normalized_terms_from_+_+_v') :-
  term_normalized(variable(X), Term),
  dom_normalized(terms_from(variable(X)), terms_from(Term)).

test('dom_normalized_terms_from_+_+_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized(terms_from(variable(X)), terms_from(Term)).

test('dom_normalized_terms_from_+_-_c') :-
  term_normalized(const(3), Term),
  dom_normalized(terms_from(const(3)), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_from_+_-_v') :-
  term_normalized(variable(X), Term),
  dom_normalized(terms_from(variable(X)), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_from_+_-_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized(terms_from(variable(X)), Dom),
  Dom == terms_from(Term).

test('dom_normalized_terms_to_+_+_c') :-
  term_normalized(const(3), Term),
  dom_normalized(terms_to(const(3)), terms_to(Term)).

test('dom_normalized_terms_to_+_+_v') :-
  term_normalized(variable(X), Term),
  dom_normalized(terms_to(variable(X)), terms_to(Term)).

test('dom_normalized_terms_to_+_+_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized(terms_to(variable(X)), terms_to(Term)).

test('dom_normalized_terms_to_+_-_c') :-
  term_normalized(const(3), Term),
  dom_normalized(terms_to(const(3)), Dom),
  Dom == terms_to(Term).

test('dom_normalized_terms_to_+_-_v') :-
  term_normalized(variable(X), Term),
  dom_normalized(terms_to(variable(X)), Dom),
  Dom == terms_to(Term).

test('dom_normalized_terms_to_+_-_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized(terms_to(variable(X)), Dom),
  Dom == terms_to(Term).

test('dom_normalized_singleton_+_+_c') :-
  term_normalized(const(3), Term),
  dom_normalized([const(3)], [Term]).

test('dom_normalized_singleton_+_+_v') :-
  term_normalized(variable(X), Term),
  dom_normalized([variable(X)], [Term]).

test('dom_normalized_singleton_+_+_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized([variable(X)], [Term]).

test('dom_normalized_singleton_+_-_c') :-
  term_normalized(const(3), Term),
  dom_normalized([const(3)], Dom),
  Dom == [Term].

test('dom_normalized_singleton_+_-_v') :-
  term_normalized(variable(X), Term),
  dom_normalized([variable(X)], Dom),
  Dom == [Term].

test('dom_normalized_singleton_+_-_v_to_c') :-
  X = 3,
  term_normalized(variable(X), Term),
  dom_normalized([variable(X)], Dom),
  Dom == [Term].

% tests for dom_normalized intervals


% Tests for terms_from_from_intersection
test('terms_from_from_intersection_c_c_>=') :-
  terms_from_from_intersection(const(y), const(x), terms_from(const(y))).

test('terms_from_from_intersection_c_c_<') :-
  terms_from_from_intersection(const(x), const(y), terms_from(const(y))).

test('terms_from_from_intersection_c_v') :-
  findall(Intersection, terms_from_from_intersection(const(x), variable(Y), Intersection), Ans),
  Ans = [terms_from(const(x)), terms_from(variable(Y))].

test('terms_from_from_intersection_v_c') :-
  findall(Intersection, terms_from_from_intersection(variable(X), const(y), Intersection), Ans),
  Ans = [terms_from(variable(X)), terms_from(const(y))].

test('terms_from_from_intersection_v_v') :-
  findall(Intersection, terms_from_from_intersection(variable(X), variable(Y), Intersection), Ans),
  Ans = [terms_from(variable(X)), terms_from(variable(Y))].

% Tests for terms_to_to_intersection
test('terms_to_to_intersection_c_c_>=') :-
  terms_to_to_intersection(const(y), const(x), terms_to(const(x))).

test('terms_to_to_intersection_c_c_<') :-
  terms_to_to_intersection(const(x), const(y), terms_to(const(x))).

test('terms_to_to_intersection_c_v') :-
  findall(Intersection, terms_to_to_intersection(const(x), variable(Y), Intersection), Ans),
  Ans = [terms_to(const(x)), terms_to(variable(Y))].

test('terms_to_to_intersection_v_c') :-
  findall(Intersection, terms_to_to_intersection(variable(X), const(y), Intersection), Ans),
  Ans = [terms_to(variable(X)), terms_to(const(y))].

test('terms_to_to_intersection_v_v') :-
  findall(Intersection, terms_to_to_intersection(variable(X), variable(Y), Intersection), Ans),
  Ans = [terms_to(variable(X)), terms_to(variable(Y))].

% Tests for terms_from_to_intersection
test('terms_from_to_intersection_c_c_=') :-
  terms_from_to_intersection(const(x), const(x), [const(x)]).

test('terms_from_to_intersection_c_c_<') :-
  terms_from_to_intersection(const(x), const(y), [const(x), const(y)]).

test('terms_from_to_intersection_c_c_>') :-
  terms_from_to_intersection(const(y), const(x), []).

test('terms_from_to_intersection_c_v') :-
  findall(Intersection, terms_from_to_intersection(const(x), variable(Y), Intersection), Ans),
  Ans = [[const(x)], [const(x), variable(Y)], []].

test('terms_from_to_intersection_v_c') :-
  findall(Intersection, terms_from_to_intersection(variable(X), const(y), Intersection), Ans),
  Ans = [[variable(X)], [variable(X), const(y)], []].

test('terms_from_to_intersection_v_v') :-
  findall(Intersection, terms_from_to_intersection(variable(X), variable(Y), Intersection), Ans),
  Ans = [[variable(X)], [variable(X), variable(Y)], []].


% Tests for terms_from_int_intersection
% | x R y | x R z | Out                      |
% |   ?   |   ?   |  [y, z]; [x, z]; [z]; [] |
% |   ?   |   <   |  [y, z]; [x, z]          | x
% |   ?   |   =   |  [z]                     | x
% |   ?   |   >   |  []                      | x
% |   <   |   ?   |  [y,z]                   | x
% |   <   |   <   |  [y,z]                   | x
% |   <   |   =   |  ----                    | x
% |   <   |   >   |  ----                    | x
% |   =   |   ?   |  [y,z]; [y]              | x
% |   =   |   <   |  [y,z]                   | x
% |   =   |   =   |  [y]                     | x
% |   =   |   >   |  ----                    | x
% |   >   |   ?   |  [x,z]; [z]; []          | x
% |   >   |   <   |  [x,z]                   | x
% |   >   |   =   |  [z]                     | x
% |   >   |   >   |  []                      | x

test('terms_from_int_intersection_?_?') :-
  findall(
    Intersection,
    terms_from_int_intersection(const(a), [variable(Y), variable(Z)], Intersection),
    Ans),
  Ans = [[variable(Y), variable(Z)], [const(a), variable(Z)], [variable(Z)], []].

test('terms_from_int_intersection_?_<') :-
  findall(
    Intersection,
    terms_from_int_intersection(const(a), [variable(Y), const(c)], Intersection),
    Ans),
  Ans = [[variable(Y), const(c)], [const(a), const(c)]].

test('terms_from_int_intersection_?_=') :-
  terms_from_int_intersection(const(a), [variable(_), const(a)], [const(a)]).

test('terms_from_int_intersection_?_>') :-
  terms_from_int_intersection(const(b), [variable(_), const(a)], []).

test('terms_from_int_intersection_<_?') :-
  terms_from_int_intersection(const(a), [const(b), variable(Z)], [const(b), variable(Z)]).

test('terms_from_int_intersection_<_<') :-
  terms_from_int_intersection(const(a), [const(b), const(c)], [const(b), const(c)]).

test('terms_from_int_intersection_=_?') :-
  findall(
    Intersection,
    terms_from_int_intersection(const(a), [const(a), variable(Z)], Intersection),
    Ans),
  Ans = [[const(a), variable(Z)], [const(a)]].

test('terms_from_int_intersection_=_<') :-
  terms_from_int_intersection(const(a), [const(a), const(b)], [const(a), const(b)]).

test('terms_from_int_intersection_=_=') :-
  terms_from_int_intersection(const(a), [const(a), const(a)], [const(a)]).

test('terms_from_int_intersection_>_?') :-
  findall(
    Intersection,
    terms_from_int_intersection(const(b), [const(a), variable(Z)], Intersection),
    Ans),
  Ans = [[const(b), variable(Z)], [variable(Z)], []].

test('terms_from_int_intersection_>_<_[x,z]') :-
  terms_from_int_intersection(const(b), [const(a), const(c)], [const(b), const(c)]).

test('terms_from_int_intersection_>_=_[z]') :-
  terms_from_int_intersection(const(b), [const(a), const(b)], [const(b)]).

test('terms_from_int_intersection_>_>_[]') :-
  terms_from_int_intersection(const(c), [const(a), const(b)], []).


:- end_tests(reif_utils).

