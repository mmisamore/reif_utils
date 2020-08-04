:- module(reif_utils,
  [ (#<)/3
  , (#>)/3
  , (#=<)/3
  , (#>=)/3
  , (==)/3
  , (\==)/3
  , (@<)/3
  , (@=<)/3
  , (@>)/3
  , (@>=)/3
  , ($<)/3
  , ($=<)/3
  , op(700, xfx, #>)
  , op(700, xfx, #<)
  , op(700, xfx, #>=)
  , op(700, xfx, #=<)
  , op(700, xfx, ==)
  , op(700, xfx, \==)
  , op(700, xfx, @<)
  , op(700, xfx, @=<)
  , op(700, xfx, @>)
  , op(700, xfx, @>=)
  , op(700, xfx, $<)
  , op(700, xfx, $=<)
  ]).

:- use_module(library(clpfd)).

% Integer representations for booleans
bool_rep(true, 1).
bool_rep(false, 0).

%! #<(+X:integer, +Y:integer, +Cond:boolean) is semidet.
%! #<(+X:integer, +Y:integer, -Cond:boolean) is det.
%! #<(+X:integer, -Y:integer, +Cond:boolean) is det.
%! #<(+X:integer, -Y:integer, -Cond:boolean) is multi.
%! #<(-X:integer, +Y:integer, +Cond:boolean) is det.
%! #<(-X:integer, +Y:integer, -Cond:boolean) is multi.
%! #<(-X:integer, -Y:integer, +Cond:boolean) is det.
%! #<(-X:integer, -Y:integer, -Cond:boolean) is multi.
%
% True whenever 1) X is strictly less than Y and Cond is true, or 2) whenever
% X is greater than or equal to Y and Cond is false. Intended to have zero unnecessary
% choice points.
#<(X, Y, Cond) :-
  (X #< Y #<==> B), bool_rep(Cond, B).

%! #>(+X:integer, +Y:integer, +Cond:boolean) is semidet.
%! #>(+X:integer, +Y:integer, -Cond:boolean) is det.
%! #>(+X:integer, -Y:integer, +Cond:boolean) is det.
%! #>(+X:integer, -Y:integer, -Cond:boolean) is multi.
%! #>(-X:integer, +Y:integer, +Cond:boolean) is det.
%! #>(-X:integer, +Y:integer, -Cond:boolean) is multi.
%! #>(-X:integer, -Y:integer, +Cond:boolean) is det.
%! #>(-X:integer, -Y:integer, -Cond:boolean) is multi.
%
% True whenever 1) X is strictly greater than Y and Cond is true, or 2) whenever
% X is less than or equal to Y and Cond is false. Intended to have zero unnecessary
% choice points.
#>(X, Y, Cond) :-
  (X #> Y #<==> B), bool_rep(Cond, B).

%! #=<(+X:integer, +Y:integer, +Cond:boolean) is semidet.
%! #=<(+X:integer, +Y:integer, -Cond:boolean) is det.
%! #=<(+X:integer, -Y:integer, +Cond:boolean) is det.
%! #=<(+X:integer, -Y:integer, -Cond:boolean) is multi.
%! #=<(-X:integer, +Y:integer, +Cond:boolean) is det.
%! #=<(-X:integer, +Y:integer, -Cond:boolean) is multi.
%! #=<(-X:integer, -Y:integer, +Cond:boolean) is det.
%! #=<(-X:integer, -Y:integer, -Cond:boolean) is multi.
%
% True whenever 1) X is less than or equal to Y and Cond is true, or 2) whenever
% X is strictly greater than Y and Cond is false. Intended to have zero unnecessary
% choice points.
#=<(X, Y, Cond) :-
  (X #=< Y #<==> B), bool_rep(Cond, B).

%! #>=(+X:integer, +Y:integer, +Cond:boolean) is semidet.
%! #>=(+X:integer, +Y:integer, -Cond:boolean) is det.
%! #>=(+X:integer, -Y:integer, +Cond:boolean) is det.
%! #>=(+X:integer, -Y:integer, -Cond:boolean) is multi.
%! #>=(-X:integer, +Y:integer, +Cond:boolean) is det.
%! #>=(-X:integer, +Y:integer, -Cond:boolean) is multi.
%! #>=(-X:integer, -Y:integer, +Cond:boolean) is det.
%! #>=(-X:integer, -Y:integer, -Cond:boolean) is multi.
%
% True whenever 1) X is greater than or equal to Y and Cond is true, or 2) whenever
% X is strictly less than Y and Cond is false. Intended to have zero unnecessary
% choice points.
#>=(X, Y, Cond) :-
  (X #>= Y #<==> B), bool_rep(Cond, B).

%! ==(+X, +Y, +Cond:boolean) is semidet.
%! ==(+X, +Y, -Cond:boolean) is det.
%! ==(+X, -Y, +Cond:boolean) is semidet.
%! ==(+X, -Y, -Cond:boolean) is det.
%! ==(-X, +Y, +Cond:boolean) is semidet.
%! ==(-X, +Y, -Cond:boolean) is det.
%! ==(-X, -Y, +Cond:boolean) is semidet.
%! ==(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term (dis)equivalence: this predicate is true whenever 1) X == Y and Cond
% is true or 2) X \== Y and Cond is false. All modes are supported; intended to have zero
% unnecessary choice points. This predicate is not steadfast: `==(X, Y, false), X = a, Y = a`
% is true with solution `X = a, Y = a` but `==(a, a, false)` is false.
==(X, Y, Cond) :-
  (  var(Cond)
  -> ( X == Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X == Y ; Cond = false -> X \== Y)
  ).

%! \==(+X, +Y, +Cond:boolean) is semidet.
%! \==(+X, +Y, -Cond:boolean) is det.
%! \==(+X, -Y, +Cond:boolean) is semidet.
%! \==(+X, -Y, -Cond:boolean) is det.
%! \==(-X, +Y, +Cond:boolean) is semidet.
%! \==(-X, +Y, -Cond:boolean) is det.
%! \==(-X, -Y, +Cond:boolean) is semidet.
%! \==(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term (dis)equivalence: this predicate is true whenever 1) X \== Y and Cond
% is true or 2) X == Y and Cond is false. All modes are supported; intended to have zero
% unnecessary choice points. This predicate is not steadfast: `\==(X, Y, true), X = a, Y = a`
% is true with solution `X = a, Y = a` but `\==(a, a, true)` is false.
\==(X, Y, Cond) :-
  (  var(Cond)
  -> ( X \== Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X \== Y ; Cond = false -> X == Y)
  ).

%! @<(+X, +Y, +Cond:boolean) is semidet.
%! @<(+X, +Y, -Cond:boolean) is det.
%! @<(+X, -Y, +Cond:boolean) is semidet.
%! @<(+X, -Y, -Cond:boolean) is det.
%! @<(-X, +Y, +Cond:boolean) is semidet.
%! @<(-X, +Y, -Cond:boolean) is det.
%! @<(-X, -Y, +Cond:boolean) is semidet.
%! @<(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term comparison: this predicate is true whenever 1) X @< Y and Cond is true or
% 2) X @>= Y and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate is not steadfast: `@<(X, Y, true), X = b, Y = a` is true
% with solution `X = b, Y = a` but `@<(b, a, true)` is false.
@<(X, Y, Cond) :-
  (  var(Cond)
  -> ( X @< Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X @< Y ; Cond = false -> X @>= Y )
  ).

%! @=<(+X, +Y, +Cond:boolean) is semidet.
%! @=<(+X, +Y, -Cond:boolean) is det.
%! @=<(+X, -Y, +Cond:boolean) is semidet.
%! @=<(+X, -Y, -Cond:boolean) is det.
%! @=<(-X, +Y, +Cond:boolean) is semidet.
%! @=<(-X, +Y, -Cond:boolean) is det.
%! @=<(-X, -Y, +Cond:boolean) is semidet.
%! @=<(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term comparison: this predicate is true whenever 1) X @=< Y and Cond is true or
% 2) X @> Y and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points.  This predicate is not steadfast: `@=<(X, Y, true), X = b, Y = a` is true
% with solution `X = b, Y = a` but `@=<(b, a, true)` is false.
@=<(X, Y, Cond) :-
  (  var(Cond)
  -> ( X @=< Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X @=< Y ; Cond = false -> X @> Y )
  ).

%! @>(+X, +Y, +Cond:boolean) is semidet.
%! @>(+X, +Y, -Cond:boolean) is det.
%! @>(+X, -Y, +Cond:boolean) is semidet.
%! @>(+X, -Y, -Cond:boolean) is det.
%! @>(-X, +Y, +Cond:boolean) is semidet.
%! @>(-X, +Y, -Cond:boolean) is det.
%! @>(-X, -Y, +Cond:boolean) is semidet.
%! @>(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term comparison: this predicate is true whenever 1) X @> Y and Cond is true or
% 2) X @=< Y and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate is not steadfast: `@>(X, Y, false), X = b, Y = a` is true
% with solution `X = b, Y = a` but `@>(b, a, false)` is false.
@>(X, Y, Cond) :-
  (  var(Cond)
  -> ( X @> Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X @> Y ; Cond = false -> X @=< Y )
  ).

%! @>=(+X, +Y, +Cond:boolean) is semidet.
%! @>=(+X, +Y, -Cond:boolean) is det.
%! @>=(+X, -Y, +Cond:boolean) is semidet.
%! @>=(+X, -Y, -Cond:boolean) is det.
%! @>=(-X, +Y, +Cond:boolean) is semidet.
%! @>=(-X, +Y, -Cond:boolean) is det.
%! @>=(-X, -Y, +Cond:boolean) is semidet.
%! @>=(-X, -Y, -Cond:boolean) is det.
%
% Impure reified term comparison: this predicate is true whenever 1) X @>= Y and Cond is true or
% 2) X @< Y and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate is not steadfast: `@>=(X, Y, false), X = b, Y = a` is true
% with solution `X = b, Y = a` but `@>=(b, a, false)` is false.
@>=(X, Y, Cond) :-
  (  var(Cond)
  -> ( X @>= Y -> Cond = true ; Cond = false )
  ;  ground(Cond)
  -> ( Cond = true -> X @>= Y ; Cond = false -> X @< Y )
  ).

%! $<(+X, +Y, +Cond:boolean) is semidet.
%! $<(+X, +Y, -Cond:boolean) is det.
%! $<(+X, -Y, +Cond:boolean) is det.
%! $<(+X, -Y, -Cond:boolean) is det.
%! $<(-X, +Y, +Cond:boolean) is det.
%! $<(-X, +Y, -Cond:boolean) is det.
%! $<(-X, -Y, +Cond:boolean) is det.
%! $<(-X, -Y, -Cond:boolean) is det.
%
% Pure reified term comparison: this predicate is true whenever 1) X @< Y upon sufficient
% instantiation of both variables and Cond is true or 2) X @>= Y upon sufficient instantiation
% of both variables and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate guarantees referential transparency by delaying evaluation where
% necessary.
$<(X, Y, Cond) :-
  (  var(Cond)
  -> when(?=(X, Y), ( X @< Y -> Cond = true ; Cond = false ) )
  ;  ground(Cond)
  -> ( Cond = true -> when(?=(X, Y), X @< Y) ; Cond = false -> when(?=(X, Y), X @>= Y) )
  ).

%! $=<(+X, +Y, +Cond:boolean) is semidet.
%! $=<(+X, +Y, -Cond:boolean) is det.
%! $=<(+X, -Y, +Cond:boolean) is det.
%! $=<(+X, -Y, -Cond:boolean) is det.
%! $=<(-X, +Y, +Cond:boolean) is det.
%! $=<(-X, +Y, -Cond:boolean) is det.
%! $=<(-X, -Y, +Cond:boolean) is det.
%! $=<(-X, -Y, -Cond:boolean) is det.
%
% Pure reified term comparison: this predicate is true whenever 1) X @=< Y upon sufficient
% instantiation of both variables and Cond is true or 2) X @> Y upon sufficient instantiation
% of both variables and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate guarantees referential transparency by delaying evaluation where
% necessary.
$=<(X, Y, Cond) :-
  (  var(Cond)
  -> when(?=(X, Y), ( X @=< Y -> Cond = true ; Cond = false ) )
  ;  ground(Cond)
  -> ( Cond = true -> when(?=(X, Y), X @=< Y) ; Cond = false -> when(?=(X, Y), X @> Y) )
  ).

