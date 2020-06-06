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

:- end_tests(reif_utils).

