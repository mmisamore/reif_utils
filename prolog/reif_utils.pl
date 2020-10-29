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
    terms_intersection/3,
    terms_dom_intersection/3,
    attr_unify_hook/2,
    attribute_goals/3,
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
:- discontiguous reif_utils:terms_intersection/3.

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
% of the following subset families:
%
% `all_terms` represents the subset of all terms in the standard ordering
% `empty` represents the empty subset of terms in the standard ordering
% `singleton(X)` represents a singleton set of terms in the standard ordering
% `terms_from(X)` represents the half interval `[X, sup]`
% `terms_to(Y)` represents the half interval `[inf, Y]`
% `[X, Y]` represents a closed interval `[X, Y]` that is not a singleton
%
% The domain theory permits known-constant interval endpoints of the form `const(-)` as 
% well as variable endpoints of the form `variable(-)`. Domain intersections with 
% variable endpoints are non-deterministic.

% term_indomain(-Term, +Dom) is det.
% term_indomain(-Term, -Dom) is det.
%
% True whenever the term `Term` belongs to the term order domain `Dom`. If `Term` is already 
% constrained in the standard ordering of terms it is unified with this new constraint.
term_indomain(Term, Dom) :-
  (  nonvar(Term)
  -> fail
  ;  var(Dom)
  -> get_attr(Term, term_order, Dom)
  ;  (  get_attr(Term, term_order, Dom1)
     -> terms_dom_intersection(Dom, Dom1, NewDom),
        put_attr(Term, term_order, NewDom)
     ;  put_attr(Term, term_order, Dom)
     )
  ).

% is_term(-Term) is det. 
% 
% True whenever `Term` is a term in the standard ordering for terms. If `Term` is already constrained in 
% the standard ordering of terms this new constraint has no effect.
is_term(Term) :-
  term_indomain(Term, all_terms).

% term_at_least(-Term, +X) is det.
% term_at_least(-Term, -X) is det.
%
% True whenever `Term` is at least as large as `X` in the standard ordering for terms. The term
% `X` may be a variable, in which case the resulting half-line domain has a variable endpoint
% that can be instantiated to a constant later. If `Term` is already constrained in the standard ordering
% of terms it is unified with this new constraint.
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
% that can be instantiated to a constant later. If `Term` is already constrained in the standard ordering
% of terms it is unified with this new constraint.
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
% endpoint variables, where applicable. This predicate will return incorrect answers when invalid domains
% (e.g. uninstantiated variables) are passed as arguments.
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

% terms_intersection(terms_from(+X), terms_from(+Y), -Intersection) is multi.
%
% Intersection of two lower-bounded domains. Input domains must be normalized first to yield
% correct answers: see `dom_normalized/2`.
terms_intersection(terms_from(X), terms_from(Y), Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)] 
  -> (  X1 @>= Y1
     -> Intersection = terms_from(const(X1))
     ;  Intersection = terms_from(const(Y1))
     )
  ;  (  Intersection = terms_from(X) ; Intersection = terms_from(Y) )
  ).

% terms_intersection(terms_to(+X), terms_to(+Y), -Intersection) is multi.
%
% Intersection of two upper-bounded domains. Input domains must be normalized first to yield
% correct answers: see `dom_normalized/2`.
terms_intersection(terms_to(X), terms_to(Y), Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)] 
  -> (  X1 @=< Y1
     -> Intersection = terms_to(const(X1))
     ;  Intersection = terms_to(const(Y1))
     )
  ;  (  Intersection = terms_to(X) ; Intersection = terms_to(Y) )
  ).

% terms_intersection(terms_from(+X), terms_to(+Y), -Intersection) is multi.
%
% Intersection of a lower-bounded domain with an upper-bounded domain. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection(terms_from(X), terms_to(Y), Intersection) :-
  (  [X, Y] = [const(X1), const(Y1)]
  -> (  X1 == Y1
     -> Intersection = singleton(X)
     ;  X1 @< Y1
     -> Intersection = [X, Y]
     ;  Intersection = empty
     )
  ;  (  X = const(X1)
     -> ( arg(1, Y, Y1), Y1 = X1, Intersection = singleton(X) )
     ;  X = variable(X1)
     -> ( arg(1, Y, Y1), X1 = Y1, Intersection = singleton(Y) )
     )
  ;  Intersection = [X, Y]
  ;  Intersection = empty
  ).

% terms_intersection(terms_to(+X), terms_from(+Y), -Intersection) is multi.
%
% Intersection of an upper-bounded domain with a lower-bounded domain. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection(terms_to(X), terms_from(Y), Intersection) :-
  terms_intersection(terms_from(Y), terms_to(X), Intersection).

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
terms_orderKey(X, Y, Z, OrderKey) :-
  term_order(X, Y, XrY),
  term_order(X, Z, XrZ),
  atom_chars(OrderKey, [XrY, XrZ]).

% Lookup table for intersecting half-interval with closed interval 
terms_from_int_lookup(??, X, Y, Z, [[Y,Z], [X,Z], singleton(Z), empty]).
terms_from_int_lookup(?<, X, Y, Z, [[Y,Z], [X,Z]]).
terms_from_int_lookup(?=, _, _, Z, [singleton(Z)]).
terms_from_int_lookup(?>, _, _, _, [empty]).
terms_from_int_lookup(<?, _, Y, Z, [[Y,Z]]).
terms_from_int_lookup(<<, _, Y, Z, [[Y,Z]]).
terms_from_int_lookup(=<, _, Y, Z, [[Y,Z]]).
terms_from_int_lookup(>?, X, _, Z, [[X,Z], singleton(Z), empty]).
terms_from_int_lookup(><, X, _, Z, [[X,Z]]).
terms_from_int_lookup(>=, _, _, Z, [singleton(Z)]).
terms_from_int_lookup(>>, _, _, _, [empty]).

% terms_intersection(terms_from(+X), [+Y, +Z], -Intersection) is multi.
%
% Intersection of a lower-bounded domain with a closed interval. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection(terms_from(X), [Y, Z], Intersection) :-
  terms_orderKey(X, Y, Z, OrderKey),
  terms_from_int_lookup(OrderKey, X, Y, Z, Intersections),
  member(Intersection0, Intersections),
  (  member(OrderKey, [??, >?]), Intersection0 = singleton(Z)
  -> arg(1, X, X1), arg(1, Z, Z1), X1 = Z1, dom_normalized(Intersection0, Intersection)
  ;  Intersection = Intersection0
  ).

% terms_intersection([+X, +Y], terms_from(+Z), -Intersection) is multi.
%
% Intersection of a closed interval with a lower-bounded domain. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection([X, Y], terms_from(Z), Intersection) :-
  terms_intersection(terms_from(Z), [X, Y], Intersection).

% Lookup table for intersecting half-interval with closed interval 
terms_to_int_lookup(??, X, Y, Z, [[Y,Z], [Y,X], singleton(Y), empty]).
terms_to_int_lookup(?<, X, Y, _, [[Y,X], singleton(Y), empty]).
terms_to_int_lookup(?=, _, Y, Z, [[Y,Z]]).
terms_to_int_lookup(?>, _, Y, Z, [[Y,Z]]).
terms_to_int_lookup(<?, _, _, _, [empty]).
terms_to_int_lookup(<<, _, _, _, [empty]).
terms_to_int_lookup(=<, _, Y, _, [singleton(Y)]).
terms_to_int_lookup(>?, X, Y, Z, [[Y,X], [Y,Z]]).
terms_to_int_lookup(><, X, Y, _, [[Y,X]]).
terms_to_int_lookup(>=, _, Y, Z, [[Y,Z]]).
terms_to_int_lookup(>>, _, Y, Z, [[Y,Z]]).

% terms_intersection(terms_to(+X), [+Y, +Z], -Intersection) is multi.
%
% Intersection of an upper-bounded domain with a closed interval. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection(terms_to(X), [Y, Z], Intersection) :-
  terms_orderKey(X, Y, Z, OrderKey),
  terms_to_int_lookup(OrderKey, X, Y, Z, Intersections),
  member(Intersection0, Intersections),
  (  member(OrderKey, [??, ?<]), Intersection0 = singleton(Y)
  -> arg(1, X, X1), arg(1, Y, Y1), X1 = Y1, dom_normalized(Intersection0, Intersection)
  ;  Intersection = Intersection0
  ).

% terms_intersection([+X, +Y], terms_to(+Z), -Intersection) is multi.
%
% Intersection of an upper-bounded domain with a closed interval. Input domains must be 
% normalized first to yield correct answers: see `dom_normalized/2`.
terms_intersection([X, Y], terms_to(Z), Intersection) :-
  terms_intersection(terms_to(Z), [X, Y], Intersection).

% Helper predicate for building keys for lookup 
terms_orderKey(X, Y, Z, W, XrW, YrZ, OrderKey) :-
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

% terms_intersection([+X, +Y], [+Z, +W], -Intersection) is multi.
%
% Intersection of two closed intervals.  Input domains must be normalized first to yield correct 
% answers: see `dom_normalized/2`.
terms_intersection([X, Y], [Z, W], Intersection) :-
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
  ;  terms_orderKey(X, Y, Z, W, XrW, YrZ, OrderKey),
     terms_int_int_lookup(OrderKey, X, Y, Z, W, Intersections),
     member(Intersection0, Intersections),
     (  member(OrderKey, [<<??, <???]), Intersection0 = singleton(Z)
     -> arg(1, Y, Y1), arg(1, Z, Z1), Y1 = Z1, dom_normalized(Intersection0, Intersection)
     ;  member(OrderKey, [>?>?, >???, ??>?]), Intersection0 = singleton(X)
     -> arg(1, X, X1), arg(1, W, W1), X1 = W1, dom_normalized(Intersection0, Intersection)
     ;  member(OrderKey, [?<?<, ?<??, ???<]), Intersection0 = singleton(Y)
     -> arg(1, Y, Y1), arg(1, Z, Z1), Y1 = Z1, dom_normalized(Intersection0, Intersection)
     ;  member(OrderKey, [??>>, ???>]), Intersection0 = singleton(W)
     -> arg(1, X, X1), arg(1, W, W1), X1 = W1, dom_normalized(Intersection0, Intersection)
     ;  OrderKey = ????, Intersection0 = singleton(X), dif(X, Y)
     -> arg(1, X, X1), arg(1, W, W1), X1 = W1, dom_normalized(Intersection0, Intersection)
     ;  OrderKey = ????, Intersection0 = singleton(Y)
     -> arg(1, Y, Y1), arg(1, Z, Z1), Y1 = Z1, dom_normalized(Intersection0, Intersection)
     ;  Intersection = Intersection0
     )
  ).

% terms_dom_intersection(+Dom1, +Dom2, -Intersection) is multi.
%
% Intersection of any two term domains for the standard ordering on terms. Domains are normalized
% first before the intersection is computed. This predicate will return incorrect answers 
% when invalid domains (e.g. uninstantiated variables) are passed as arguments.
terms_dom_intersection(Dom1, Dom2, Intersection) :-
  dom_normalized(Dom1, NewDom1),
  dom_normalized(Dom2, NewDom2),
  (  NewDom1 = all_terms
  -> Intersection = NewDom2
  ;  NewDom2 = all_terms
  -> Intersection = NewDom1
  ; terms_intersection(NewDom1, NewDom2, Intersection)
  ).
  
% term_singleton_singleton_intersection(+X, +Y, -Intersection) is det.
%
% Intersection of two singletons.  Input domains must be normalized first to yield correct 
% answers: see `dom_normalized/2`.
%terms_singleton_singleton_intersection(X, Y, Intersection) :-
%  (  ( X = const(X1), (Y = const(Y1) ; Y = variable(Y1)) )
%  -> (  X1 = Y1
%     -> Intersection = singleton(X)
%     ;  Intersection = empty
%     )
%  ;  ( X = variable(X1), (Y = const(Y1) ; Y = variable(Y1)) )
%  -> (  X1 = Y1
%     -> Intersection = singleton(Y)
%     ;  Intersection = empty
%     )
%  ).

% term_singleton_from_intersection(+X, +Y, -Intersection) is det.
%
% Intersection of a singleton with a lower-bounded half-line. Input domains must be normalized 
% first to yield correct answers: see `dom_normalized/2`.
%terms_singleton_from_intersection(X, Y, Intersection) :-
  %(  X = const(X1)
  %-> (  Y = const(Y1)
     %-> (  X1 @< Y1
        %-> Intersection = empty
        %;  Intersection = singleton(X)
        %)
     %;  ( Intersection = singleton(X) ; Intersection = empty )
     %)
  %;  ( Intersection = singleton(X) ; Intersection = empty )
  %).

% term_singleton_to_intersection(+X, +Y, -Intersection) is det.
%
% Intersection of a singleton with an upper-bounded half-line. Input domains must be normalized 
% first to yield correct answers: see `dom_normalized/2`.
%terms_singleton_to_intersection(X, Y, Intersection) :-
  %(  X = const(X1)
  %-> (  Y = const(Y1)
     %-> (  X1 @> Y1
        %-> Intersection = empty
        %;  Intersection = singleton(X)
        %)
     %;  ( Intersection = singleton(X) ; Intersection = empty )
     %)
  %;  ( Intersection = singleton(X) ; Intersection = empty )
  %).

% terms_singleton_int_intersection(+X, [+Y, +Z], -Intersection) is det.
%
% Intersection of a singleton with a closed interval. Input domains must be normalized 
% first to yield correct answers: see `dom_normalized/2`.
%terms_singleton_int_intersection(_, _, _) :-
  %fail.

% Hook for term unification in the new "term_order" domain 
attr_unify_hook(Dom1, Term2) :-
  (  get_attr(Term2, term_order, Dom2)      % Term2 is already attributed
  -> terms_dom_intersection(Dom1, Dom2, NewDom),
     (  NewDom == empty                     % Fail to unify if resulting domain is empty
     -> fail
     ;  NewDom = singleton(Value)           % New domain is a singleton, so delete attribute and unify normally 
     -> arg(1, Value, Value1),
        del_attr(Term2, term_order),
        Term2 = Value1
     ;  put_attr(Term2, term_order, NewDom) % Otherwise, just set the new domain
     )
  %;  var(Term2)                             % Term2 is not already attributed, but is a variable, so
  %-> put_attr(Term2, term_order, Dom1)      % set the domain upon unifying
  %;  (  Dom1 == all_terms                   % Term2 is not a variable, so check if it belongs to Dom1
     %-> true 
     %;  Dom1 = [X, Y],
        %X @=< Term2,
        %Term2 @=< Y
     %)
  ).

attribute_goals(Term) -->
  { get_attr(Term, term_order, Dom0) },
  { dom_normalized(Dom0, Dom1) },
  [term_in(Term, Dom1)].

