%%%%%%%%%%%%% Defined Design Rules %%%%%%%%%%%%%

% Encapsulation
encapsulation_one(Class, Attr,Type, Vis) :-
    attribute(Class, Attr,Type, Vis),
    Vis == private.
encapsulation_two(Class, Method,Type, Vis) :-
    method(Class, Method, Type, Vis),
    Vis == public.

% Single inheritance
single_inheritance(Class, Parents) :-
    findall(P, relationship(P, Class, inheritance), List),
    length(List,1).

% Abstraction
shared_feature(C1, C2, F) :-
    feature(C1, F),
    feature(C2, F),
    C1 \= C2.
common_parent(C1, C2, P) :-
    relationship(P, C1, inheritance),
    relationship(P, C2, inheritance),
    C1 \= C2. 
abstraction_one(C1, C2) :-
    shared_feature(C1, C2, _),
    common_parent(C1, C2, _).
abstraction_two(C1, C2) :-
    common_parent(C1, C2, P), 
    \+ (
        shared_feature(C1, C2, F),
        \+ feature(P, F)
    ).

% Strategy Pattern
concrete_strategy(I, C) :-
    interface(I),
    relationship(I, C, inheritance),
    class(C).
strategy_one(I) :-
    interface(I),
    findall(C, concrete_strategy(I, C), Cs),
    length(Cs, N),
    N >= 2.
strategy_two(Context, Strategy) :-
    class(Context),
    interface(Strategy),
    attribute(Context, _, Strategy, _).
strategy_method(Strategy, Method) :-
    interface(Strategy),
    method(Strategy, Method,_,_).
strategy_three(Context, Strategy) :-
    class(Context),
    interface(Strategy),
    method(Context, Method, Strategy,_ ),
    strategy_method(Strategy, Method).


%%%%%% Rule checks- Violations Report %%%%%%%
violation(encapsulation_one, Class, Attr) :-
    attribute(Class, Attr, Type, Vis),
    \+ encapsulation_one(Class, Attr, Type, Vis).

violation(encapsulation_two, Class, Method) :-
    method(Class, Method, Type, Vis),
    \+ encapsulation_two(Class, Method, Type, Vis).

violation(single_inheritance, Class) :-
    class(Class),
    \+ single_inheritance(Class, _).

violation(abstraction_one, C1, C2) :-
    shared_feature(C1, C2, _),
    \+ abstraction_one(C1, C2).

violation(abstraction_two, C1, C2) :-
    common_parent(C1, C2, _),
    \+ abstraction_two(C1, C2).

violation(strategy_one, Strategy) :-
    interface(Strategy),
    \+ strategy_one(Strategy).

violation(strategy_two, Context, Strategy) :-
    class(Context),
    interface(Strategy),
    \+ strategy_two(Context, Strategy).

violation(strategy_three, Context, Strategy) :-
    class(Context),
    interface(Strategy),
    \+ strategy_three(Context, Strategy).

all_violations(Violations) :-
    findall(
        V,
        violation_term(V),
        Violations
    ).

violation_term(violation(Rule, A)) :-
    violation(Rule, A).

violation_term(violation(Rule, A, B)) :-
    violation(Rule, A, B).

run_violation_checks :-
    all_violations(Vs),
    (   Vs == []
    ->  writeln('✔ All rules satisfied')
    ;   writeln('✘ Violations found:'),
        forall(member(V, Vs), writeln(V))
    ).

%%%%%% Rule checks- Satisfaction Report %%%%%%%
satisfaction(encapsulation_one, Class, Attr) :-
    attribute(Class, Attr, Type, Vis),
     encapsulation_one(Class, Attr, Type, Vis).

satisfaction(encapsulation_two, Class, Method) :-
    method(Class, Method, Type, Vis),
     encapsulation_two(Class, Method, Type, Vis).

satisfaction(single_inheritance, Class) :-
    class(Class),
     single_inheritance(Class, _).

satisfaction(abstraction_one, C1, C2) :-
    shared_feature(C1, C2, _),
     abstraction_one(C1, C2).

satisfaction(abstraction_two, C1, C2) :-
    common_parent(C1, C2, _),
    abstraction_two(C1, C2).

satisfaction(strategy_one, Strategy) :-
    interface(Strategy),
    strategy_one(Strategy).

satisfaction(strategy_two, Context, Strategy) :-
    class(Context),
    interface(Strategy),
    strategy_two(Context, Strategy).

satisfaction(strategy_three, Context, Strategy) :-
    class(Context),
    interface(Strategy),
    strategy_three(Context, Strategy).

all_satisfaction(Satisfaction) :-
    findall(
        S,
        satisfaction_term(S),
        Satisfaction
    ).

satisfaction_term(satisfaction(Rule, A)) :-
    satisfaction(Rule, A).

satisfaction_term(satisfaction(Rule, A, B)) :-
    satisfaction(Rule, A, B).

run_satisfaction_checks :-
    all_satisfaction(Ss),
    (   Ss == []
    ->  writeln('✘ Violations found:')
    ;   writeln('✔ Satisfied rules:'),
        forall(member(S, Ss), writeln(S))
    ).
