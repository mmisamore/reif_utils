:- module(reif_utils, [ (#<)/3 
                      , (#>)/3 
                      , (#=<)/3
                      , (#>=)/3
                      , (==)/3 
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
%! ==(+X, -Y, +Cond:boolean) is det.
%! ==(+X, -Y, -Cond:boolean) is multi.
%! ==(-X, +Y, +Cond:boolean) is det.
%! ==(-X, +Y, -Cond:boolean) is multi.
%! ==(-X, -Y, +Cond:boolean) is det.
%! ==(-X, -Y, -Cond:boolean) is multi.
%
% Reified (dis)equivalence: this predicate is true whenever 1) X == Y and Cond is true or 
% 2) X \== Y and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points.
==(X, Y, Cond) :- 
  (  ?=(X, Y)
  -> ( X == Y -> Cond = true ; Cond = false )
  ;  ground(Cond) 
  -> ( Cond = true -> X = Y ; Cond = false -> dif(X, Y) )
  ;  var(Cond)
  -> ( Cond = true, X = Y ; Cond = false, dif(X, Y) )
  ).

