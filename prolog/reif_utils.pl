:- module(reif_utils,
  [ (#<)/3,
    (#>)/3,
    (#=<)/3,
    (#>=)/3,
    (==)/3,
    (\==)/3,
    (@<)/3,
    (@=<)/3,
    (@>)/3,
    (@>=)/3,
    term_indomain/2,
    is_term/1,
    term_normalized/2,
    dom_normalized/2,
    term_at_least/2,
    term_at_most/2,
    terms_from_from_intersection/3,
    terms_to_to_intersection/3,
    terms_from_to_intersection/3,
    terms_from_int_intersection/3,
    terms_to_int_intersection/3,
    terms_int_int_intersection/3,
    ($<)/3,
    ($=<)/3,
    ($>)/3,
    ($>=)/3,
    op(700, xfx, #>),
    op(700, xfx, #<),
    op(700, xfx, #>=),
    op(700, xfx, #=<),
    op(700, xfx, ==),
    op(700, xfx, \==),
    op(700, xfx, @<),
    op(700, xfx, @=<),
    op(700, xfx, @>),
    op(700, xfx, @>=),
    op(700, xfx, $<),
    op(700, xfx, $=<),
    op(700, xfx, $>),
    op(700, xfx, $>=)
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

%! $>(+X, +Y, +Cond:boolean) is semidet.
%! $>(+X, +Y, -Cond:boolean) is det.
%! $>(+X, -Y, +Cond:boolean) is det.
%! $>(+X, -Y, -Cond:boolean) is det.
%! $>(-X, +Y, +Cond:boolean) is det.
%! $>(-X, +Y, -Cond:boolean) is det.
%! $>(-X, -Y, +Cond:boolean) is det.
%! $>(-X, -Y, -Cond:boolean) is det.
%
% Pure reified term comparison: this predicate is true whenever 1) X @> Y upon sufficient
% instantiation of both variables and Cond is true or 2) X @=< Y upon sufficient instantiation
% of both variables and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate guarantees referential transparency by delaying evaluation where
% necessary.
$>(X, Y, Cond) :-
  (  var(Cond)
  -> when(?=(X, Y), ( X @> Y -> Cond = true ; Cond = false ) )
  ;  ground(Cond)
  -> ( Cond = true -> when(?=(X, Y), X @> Y) ; Cond = false -> when(?=(X, Y), X @=< Y) )
  ).

%! $>=(+X, +Y, +Cond:boolean) is semidet.
%! $>=(+X, +Y, -Cond:boolean) is det.
%! $>=(+X, -Y, +Cond:boolean) is det.
%! $>=(+X, -Y, -Cond:boolean) is det.
%! $>=(-X, +Y, +Cond:boolean) is det.
%! $>=(-X, +Y, -Cond:boolean) is det.
%! $>=(-X, -Y, +Cond:boolean) is det.
%! $>=(-X, -Y, -Cond:boolean) is det.
%
% Pure reified term comparison: this predicate is true whenever 1) X @>= Y upon sufficient
% instantiation of both variables and Cond is true or 2) X @< Y upon sufficient instantiation
% of both variables and Cond is false. All modes are supported; intended to have zero unnecessary
% choice points. This predicate guarantees referential transparency by delaying evaluation where
% necessary.
$>=(X, Y, Cond) :-
  (  var(Cond)
  -> when(?=(X, Y), ( X @>= Y -> Cond = true ; Cond = false ) )
  ;  ground(Cond)
  -> ( Cond = true -> when(?=(X, Y), X @>= Y) ; Cond = false -> when(?=(X, Y), X @< Y) )
  ).


% Setup an interval theory for the standard total ordering on terms. The theory consists 
% of the following subset types:
%
% `all_terms` represents the subset of all terms
% `terms_from(X)` represents the half interval `[X, sup]`
% `terms_to(Y)` represents the half interval `[inf, Y]`
% `singleton(X)` represents a singleton set 
% `[X, Y]` represents a closed interval `[X, Y]` that is not a singleton
% `empty` represents the empty subset
%
% The domain theory permits known-constant interval endpoints of the form `const(-)` as 
% well as variable endpoints of the form `variable(-)`. Domain intersections with 
% variable endpoints are non-deterministic.

% term_indomain(-Term, +Dom) is det.
%
% True whenever the term `Term` belongs to the term order domain `Dom`.
term_indomain(Term, Dom) :-
  del_attr(Term, term_order),
  put_attr(Term, term_order, Dom).

% is_term(-Term) is det. 
% 
% True whenever `Term` is a term in the standard ordering for terms.
is_term(Term) :-
  term_indomain(Term, all_terms).

% term_at_least(-Term, +X) is det.
% term_at_least(-Term, -X) is det.
%
% True whenever `Term` is at least as large as `X` in the standard ordering for terms. The term
% `X` may be a variable, in which case the resulting half-line domain has a variable endpoint
% that can be instantiated to a constant later.
term_at_least(Term, X) :-
  (  var(X)
  -> term_indomain(Term, terms_from(variable(X)))
  ;  term_indomain(Term, terms_from(const(X)))
  ).

% term_at_most(-Term, +Y) is det.
% term_at_most(-Term, -Y) is det.
%
% True whenever `Term` is at most as large as `Y` in the standard ordering for terms. The term
% `Y` may be a variable, in which case the resulting half-line domain has a variable endpoint
% that can be instantiated to a constant later.
term_at_most(Term, Y) :-
  (  var(Y)
  -> term_indomain(Term, terms_to(variable(Y)))
  ;  term_indomain(Term, terms_to(const(Y)))
  ).

% term_normalized(+Term0, +Term) is semidet.
% term_normalized(+Term0, -Term) is det.
%
% True whenever `Term` has functor matching `Term0`s current variable status.
term_normalized(Term0, Term) :-
  (  (Term0 = variable(X), nonvar(X))
  -> Term = const(X)
  ;  Term = Term0
  ).

% dom_normalized(+Dom0, +Dom) is semidet.
% dom_normalized(+Dom0, -Dom) is det.
%
% True whenever `Dom` has functors matching `Dom0`s current variable statuses for the relevant
% endpoint variables, where applicable.
dom_normalized(Dom0, Dom) :-
  (  Dom0 = all_terms 
  -> Dom  = Dom0
  ;  Dom0 = terms_from(Term0)
  -> term_normalized(Term0, Term),
     Dom  = terms_from(Term)
  ;  Dom0 = terms_to(Term0)
  -> term_normalized(Term0, Term),
     Dom  = terms_to(Term)
  ;  Dom0 = singleton(Term0)
  -> term_normalized(Term0, Term),
     Dom  = singleton(Term)
  ;  Dom0 = [TermX0, TermY0]
  -> term_normalized(TermX0, TermX),
     term_normalized(TermY0, TermY),
     (  [TermX, TermY] = [const(A), const(A)]
     -> Dom = singleton(TermX)
     ;  Dom  = [TermX, TermY]
     )
  ).

% terms_from_from_intersection(+X, +Y, -Intersection) is multi.
%
% Intersection of two lower-bounded domains. Input domains must be normalized first to yield
% correct answers: see `dom_normalized/2`.
terms_from_from_intersection(X, Y, Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)] 
  -> (  X1 @>= Y1
     -> Intersection = terms_from(const(X1))
     ;  Intersection = terms_from(const(Y1))
     )
  ;  (  Intersection = terms_from(X) ; Intersection = terms_from(Y) )
  ).

% terms_to_to_intersection(+X, +Y, -Intersection) is multi.
%
% Intersection of two upper-bounded domains. Input domains must be normalized first to yield
% correct answers: see `dom_normalized/2`.
terms_to_to_intersection(X, Y, Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)] 
  -> (  X1 @=< Y1
     -> Intersection = terms_to(const(X1))
     ;  Intersection = terms_to(const(Y1))
     )
  ;  (  Intersection = terms_to(X) ; Intersection = terms_to(Y) )
  ).

% terms_from_to_intersection(+X, +Y, -Intersection) is multi.
%
% Intersection of a lower-bounded domain with an upper-bounded domain. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_from_to_intersection(X, Y, Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)]
  -> (  X1 == Y1
     -> Intersection = singleton(X)
     ;  X1 @< Y1
     -> Intersection = [X, Y]
     ;  Intersection = empty 
     )
  ; ( (  X = const(_)
      -> Intersection = singleton(X)
      ;  Intersection = singleton(Y)
      )
      ; Intersection = [X, Y] 
      ; Intersection = empty 
    )
  ).

% terms_from_int_intersection(+X, +Interval:list, -Intersection) is multi.
%
% Intersection of a lower-bounded domain with a closed interval. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_from_int_intersection(X, [Y, Z], Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)] 
  -> (  X1 @< Y1
     -> Intersection = [Y, Z]
     ;  X1 == Y1
     -> (  Z = const(Z1)
        -> (  X1 @< Z1
           -> Intersection = [Y, Z]
           ;  Intersection = singleton(Y)
           )
        ;  ( Intersection = [Y, Z] ; Intersection = singleton(Y) )
        )
     ;  (  Z = const(Z1)
        -> (  X1 @< Z1
           -> Intersection = [X, Z]
           ;  X1 == Z1
           -> Intersection = singleton(Z)
           ;  Intersection = empty 
           )
        ;  ( Intersection = [X, Z] ; Intersection = singleton(X) ; Intersection = empty )
        ) 
     )
  ;  [X, Z] = [const(X1), const(Z1)] 
  -> (  X1 @< Z1
     -> ( Intersection = [Y, Z] ; Intersection = [X, Z] )
     ;  X1 == Z1
     -> Intersection = singleton(Z)
     ;  Intersection = empty 
     )
  ;  ( Intersection = [Y, Z] ; 
       Intersection = [X, Z] ; 
       (  X = const(_)
       -> Intersection = singleton(X)
       ;  Intersection = singleton(Z)
       ) ;
       Intersection = empty )
  ).

% terms_from_int_intersection(+X, +Interval:list, -Intersection) is multi.
%
% Intersection of an upper-bounded domain with a closed interval. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_to_int_intersection(X, [Y, Z], Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)]
  -> (  X1 @< Y1
     -> Intersection = empty 
     ;  X1 == Y1
     -> Intersection = singleton(Y)
     ;  (  Z = const(Z1)
        -> (  X1 @>= Z1
           -> Intersection = [Y, Z]
           ;  Intersection = [Y, X]
           )
        ;  ( Intersection = [Y, X] ; Intersection = [Y, Z] )
        )
     )
  ;  [X, Z] = [const(X1), const(Z1)]
  -> (  X1 @>= Z1
     -> Intersection = [Y, Z]
     ;  ( Intersection = [Y, X] ; Intersection = singleton(X) ; Intersection = empty )
     )
  ;  (  Intersection = [Y, Z] ;
        Intersection = [Y, X] ;
        (  X = const(_)
        -> Intersection = singleton(X)
        ;  Intersection = singleton(Y)
        ) ;
        Intersection = empty )
  ).

% Helper predicate for comparing endpoints
term_order(X, Y, Ord) :-
  (  [X, Y] = [const(X1), const(Y1)]
  -> (  X1 @< Y1
     -> Ord = '<'
     ;  X1 @> Y1
     -> Ord = '>'
     ;  Ord = '='
     )
  ; Ord = '?'
  ).

% Helper predicate for building keys for lookup 
terms_int_int_orderKey(X, Y, Z, W, XrW, YrZ, OrderKey) :-
  term_order(X, Z, XrZ),
  term_order(Y, W, YrW),
  atom_chars(OrderKey, [XrZ, XrW, YrZ, YrW]).

% Lookup table for intersecting two closed intervals 
terms_int_int_lookup(<<><, _, Y, Z, _, [[Z,Y]]).
terms_int_int_lookup(<<>=, _, Y, Z, _, [[Z,Y]]).
terms_int_int_lookup(=<><, _, Y, Z, _, [[Z,Y]]).
terms_int_int_lookup(=<>=, _, Y, Z, _, [[Z,Y]]).
terms_int_int_lookup(<<>>, _, _, Z, W, [[Z,W]]).
terms_int_int_lookup(=<>>, _, _, Z, W, [[Z,W]]).
terms_int_int_lookup(><><, X, Y, _, _, [[X,Y]]).
terms_int_int_lookup(><>=, X, Y, _, _, [[X,Y]]).
terms_int_int_lookup(><>>, X, _, _, W, [[X,W]]).
terms_int_int_lookup(<?>?, _, Y, Z, W, [[Z,W], [Z,Y]]).
terms_int_int_lookup(=?>?, _, Y, Z, W, [[Z,W], [Z,Y]]).
terms_int_int_lookup(=???, _, Y, Z, W, [[Z,W], [Z,Y]]).
terms_int_int_lookup(=<??, _, Y, Z, W, [[Z,W], [Z,Y]]).
terms_int_int_lookup(><??, X, Y, _, W, [[X,W], [X,Y]]).
terms_int_int_lookup(?<?=, X, Y, Z, _, [[Z,Y], [X,Y]]).
terms_int_int_lookup(??><, X, Y, Z, _, [[Z,Y], [X,Y]]).
terms_int_int_lookup(??>=, X, Y, Z, _, [[Z,Y], [X,Y]]).
terms_int_int_lookup(???=, X, Y, Z, _, [[Z,Y], [X,Y]]).
terms_int_int_lookup(?<?>, X, _, Z, W, [[Z,W], [X,W]]).
terms_int_int_lookup(<<??, _, Y, Z, W, [[Z,W], [Z,Y], singleton(Z), empty]).
terms_int_int_lookup(<???, _, Y, Z, W, [[Z,W], [Z,Y], singleton(Z), empty]).
terms_int_int_lookup(>?>?, X, Y, _, W, [[X,W], [X,Y], singleton(X), empty]).
terms_int_int_lookup(>???, X, Y, _, W, [[X,W], [X,Y], singleton(X), empty]).
terms_int_int_lookup(?<?<, X, Y, Z, _, [[Z,Y], [X,Y], singleton(Y), empty]).
terms_int_int_lookup(???<, X, Y, Z, _, [[Z,Y], [X,Y], singleton(Y), empty]).
terms_int_int_lookup(??>>, X, _, Z, W, [[Z,W], [X,W], singleton(W), empty]).
terms_int_int_lookup(???>, X, _, Z, W, [[Z,W], [X,W], singleton(W), empty]).
terms_int_int_lookup(?<??, X, Y, Z, W, [[Z,W], [X,W], [Z,Y], [X,Y], singleton(Y), empty]).
terms_int_int_lookup(??>?, X, Y, Z, W, [[Z,W], [X,W], [Z,Y], [X,Y], singleton(X), empty]).
terms_int_int_lookup(????, X, Y, Z, W, [[Z,W], [X,W], [Z,Y], [X,Y], singleton(X), singleton(Y), empty]).

% terms_int_int_intersection(+Xs:list, +Ys:list, -Intersection) is multi.
%
% Intersection of two closed intervals.  Input domains must be normalized first to yield correct 
% answers: see `dom_normalized/2`.
terms_int_int_intersection([X, Y], [Z, W], Intersection) :-
  term_order(X, W, XrW),
  term_order(Y, Z, YrZ),
  (  YrZ = '<'
  -> Intersection = empty
  ;  XrW = '>'
  -> Intersection = empty
  ;  YrZ = '='
  -> Intersection = singleton(Y)
  ;  XrW = '='
  -> Intersection = singleton(X)
  ;  terms_int_int_orderKey(X, Y, Z, W, XrW, YrZ, OrderKey),
     terms_int_int_lookup(OrderKey, X, Y, Z, W, Intersections),
     member(Intersection, Intersections),
     % Certain variables must be unified in singleton cases
     (  (OrderKey = ?<??, Intersection = singleton(variable(Y1)), Z = variable(Z1))
     -> Y1 = Z1
     ;  (OrderKey = ??>?, Intersection = singleton(variable(X1)), W = variable(W1))
     -> X1 = W1
     ;  OrderKey = ????
     -> ( Intersection = singleton(X), dif(X, Y), X = variable(X1), W = variable(W1), X1 = W1
        ; Intersection = singleton(Y), dif(X, Y), Y = variable(Y1), Z = variable(Z1), Y1 = Z1
        ; dif(Intersection, singleton(X)), dif(Intersection, singleton(Y))
        )
     ;  true 
     )
  ).

% term_dom_intersection(+Dom1, +Dom2, -Intersection) is multi.
%
% Intersection of any two term domains for the standard ordering on terms. Domains are normalized
% first before the intersection is computed.
term_dom_intersection(Dom1, Dom2, Intersection) :-
  (  Dom1 == all_terms
  -> Intersection = Dom2
  ;  Dom2 == all_terms
  -> Intersection = Dom1
  ;  Dom1 = terms_from(X)
  -> (  Dom2 = terms_from(Y)
     -> terms_from_from_intersection(X, Y, Intersection) 
     ;  Dom2 = terms_to(Y)
     -> terms_from_to_intersection(X, Y, Intersection) 
     ;  Dom2 = terms_int([Y, Z])
     -> terms_from_int_intersection(X, [Y, Z], Intersection)
     )
  ;  Dom1 = terms_to(X)
  -> (  Dom2 = terms_from(Y)
     -> terms_from_to_intersection(Y, X, Intersection)
     ;  Dom2 = terms_to(Y)
     -> terms_to_to_intersection(X, Y, Intersection)
     ;  Dom2 = terms_int([Y, Z])
     -> fail
     )
  ;  Dom1 = terms_int([X, Y])
  -> (  Dom2 = terms_from(Z)
     -> terms_from_int_intersection(Z, [X, Y], Intersection) 
     ;  Dom2 = terms_to(Z)
     -> fail
     ;  Dom2 = terms_int([Z, W])
     -> fail
     )
  ).

%attr_unify_hook(Dom1, Term2) :-
  %(  get_attr(Term2, terms_in, Dom2) % Term2 is already attributed
  %-> term_dom_intersection(Dom1, Dom2, NewDom),
     %(  NewDom == []
     %-> fail
     %;  NewDom = [Value]
     %-> Term2 = Value
     %;  put_attr(Term2, terms_in, NewDom)
     %)
   %;  var(Term2)             % Term2 is not already attributed, but is a variable
   %-> put_attr(Term2, terms_in, Dom1)
   %;  (  Dom1 == all_terms   % Term2 is not a variable, so check if it belongs to Dom1
      %-> true
      %;  Dom1 = [X, Y],
         %X @=< Term2,
         %Term2 @=< Y
      %)
  %).

attribute_goals(Term) -->
  { get_attr(Term, term_order, Domain) },
  [term_in(Term, Domain)].

