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

% Delegation
delegates(A,B) :- relationship(A,B,association).
delegates(A,B) :- relationship(A,B,aggregation).
delegates(A,B) :- relationship(A,B,dependency).
delegation(A,B) :-
    delegates(A,B),
    \+ relationship(A,B,inheritance).

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

% Role-Object Pattern
component_interface(Component) :-
    interface(Component).

component_core(Component, Core) :-
    component_interface(Component),
    class(Core),
    relationship(Component, Core, inheritance).

component_role(Component, Role) :-
    component_interface(Component),
    class(Role),
    relationship(Component, Role, inheritance).

role_object_one(Component, Core, Role) :-
    component_interface(Component),
    component_core(Component, Core),
    component_role(Component, Role).

concrete_role(Role, ComponentRole) :-
    class(Role),
    class(ComponentRole),
    relationship(ComponentRole, Role, inheritance),
    method(Role, _, _, _).

role_object_two(ComponentRole, Role) :-
    component_role(_, ComponentRole),
    concrete_role(Role, ComponentRole).

role_object_three(ComponentCore, ComponentRole) :-
    component_core(_, ComponentCore),
    component_role(_, ComponentRole),
    attribute(ComponentCore, _, ComponentRole, _).

% Factory Pattern

abstract_creator(Creator) :-
    interface(Creator).

concrete_creator(Creator, Concrete) :-
    abstract_creator(Creator),
    class(Concrete),
    relationship(Creator, Concrete, inheritance).

factory_one(Creator) :-
    abstract_creator(Creator),
    concrete_creator(Creator, _).

abstract_product(Product) :-
    interface(Product).

abstract_factory_method(Creator, Method, Product) :-
    abstract_creator(Creator),
    abstract_product(Product),
    method(Creator, Method, Product, _).

factory_two(Creator, Method, Product) :-
    abstract_factory_method(Creator, Method, Product).

product_hierarchy(AbstractProduct, ConcreteProduct) :-
    abstract_product(AbstractProduct),
    class(ConcreteProduct),
    relationship(AbstractProduct, ConcreteProduct, inheritance).

factory_three(AbstractProduct) :-
    abstract_product(AbstractProduct),
    product_hierarchy(AbstractProduct, _).

overrides_factory_method(
    AbstractCreator,
    ConcreteCreator,
    Method,
    AbstractProduct,
    ConcreteProduct
) :-
    abstract_creator(AbstractCreator),
    concrete_creator(AbstractCreator, ConcreteCreator),
    abstract_product(AbstractProduct),
    class(ConcreteProduct),

    product_hierarchy(AbstractProduct, ConcreteProduct),

    method(AbstractCreator, Method, AbstractProduct, _),
    method(ConcreteCreator, Method, ConcreteProduct, _).

factory_four(AbstractCreator, ConcreteCreator, Method) :-
    overrides_factory_method(
        AbstractCreator,
        ConcreteCreator,
        Method,
        _AbstractProduct,
        _ConcreteProduct
    ).


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

violation(delegation, A, B) :-
    \+ delegation(A, B).

violation(factory_one, Creator) :-
    abstract_creator(Creator),
    \+ factory_one(Creator).

violation(factory_two, Creator, Method, Product) :-
    \+ factory_two(Creator, Method, Product).

violation(factory_three, AbstractProduct) :-
    abstract_product(AbstractProduct),
    \+ factory_three(AbstractProduct).

violation(factory_f4_missing_override, ConcreteCreator) :-
    concrete_creator(AbstractCreator, ConcreteCreator),
    \+ factory_four(AbstractCreator, ConcreteCreator, _).

violation(factory_f4_abstract_returns_concrete, AbstractCreator, Method) :-
    abstract_creator(AbstractCreator),
    abstract_product(AbstractProduct),
    product_hierarchy(AbstractProduct, ConcreteProduct),
    method(AbstractCreator, Method, ConcreteProduct, _).

violation(role_object_one, Component) :-
    interface(Component),
    \+ role_object_one(Component, _, _).

violation(role_object_two, Role) :-
    class(Role),
    relationship(ComponentRole, Role, inheritance),
    \+ role_object_two(ComponentRole, Role).

violation(role_object_three, ComponentCore, ComponentRole) :-
    component_core(_, ComponentCore),
    component_role(_, ComponentRole),
    \+ role_object_three(ComponentCore, ComponentRole).

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

violation_term(violation(Rule, A, B, C)) :-
    violation(Rule, A, B, C).

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

satisfaction(delegation, A, B) :-
    delegation(A, B).

satisfaction(factory_one, Creator) :-
    abstract_creator(Creator),
     factory_one(Creator).

satisfaction(factory_two, Creator, Method, Product) :-
     factory_two(Creator, Method, Product).

satisfaction(factory_three, AbstractProduct) :-
    abstract_product(AbstractProduct),
    factory_three(AbstractProduct).

satisfaction(factory_four, ConcreteCreator) :-
    concrete_creator(AbstractCreator, ConcreteCreator),
   factory_four(AbstractCreator, ConcreteCreator, _).

satisfaction(role_object_one, Component) :-
    interface(Component),
    role_object_one(Component, _, _).

satisfaction(role_object_two, Role) :-
    class(Role),
    relationship(ComponentRole, Role, inheritance),
    role_object_two(ComponentRole, Role).

satisfaction(role_object_three, ComponentCore, ComponentRole) :-
    component_core(_, ComponentCore),
    component_role(_, ComponentRole),
    role_object_three(ComponentCore, ComponentRole).


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

satisfaction_term(satisfaction(Rule, A, B, C)) :-
    satisfaction(Rule, A, B, C).

run_satisfaction_checks :-
    all_satisfaction(Ss),
    (   Ss == []
    ->  writeln('✘ Violations found:')
    ;   writeln('✔ Satisfied rules:'),
        forall(member(S, Ss), writeln(S))
    ).



/* check_encapsulation_one :-
    forall(
        attribute(Class, Attr, Type, Vis),
        encapsulation_one(Class, Attr, Type, Vis)
    ).
check_encapsulation_two :-
    forall(
        method(Class, Method, Type, Vis),
        encapsulation_two(Class, Method, Type, Vis)
    ).
check_single_inheritance :-
    forall(
        class(Class),
        single_inheritance(Class, _)
    ).
check_delegation :-
    forall(
        delegation(A, B),
        \+ relationship(A, B, inheritance)
    ).
check_abstraction_one :-
    forall(
        shared_feature(C1, C2, _),
        abstraction_one(C1, C2)
    ).
check_abstraction_two :-
    forall(
        common_parent(C1, C2, _),
        abstraction_two(C1, C2)
    ).
check_strategy_one :-
    forall(
        interface(I),
        strategy_one(I)
    ).
check_strategy_two :-
    forall(
        (
            class(Context),
            interface(Strategy)
        ),
        strategy_two(Context, Strategy)
    ).
check_strategy_three :-
    forall(
        (
            class(Context),
            interface(Strategy)
        ),
        strategy_three(Context, Strategy)
    ).
check_all :-
    check_encapsulation_one,
    check_encapsulation_two,
    check_single_inheritance,
    check_abstraction_one,
    check_abstraction_two,
    check_strategy_one,
    check_strategy_two,
    check_strategy_three.
*/
