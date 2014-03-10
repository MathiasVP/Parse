Parse
=====

Compile time (LR) parser generator for C++

## Syntax

A parser is defined using the LrParser class.
An LrParser type takes a variadic number of template arguments, where each argument is a Production type.
A Production type takes 3 arguments:
- A Left-type, which takes a nonterminal (See "Nonterminals" for more information)
- A Right-type, which takes a variable number of nonterminals and terminals (See "Terminals" for more information)
- A semantic action, which is something that is calleble (See "Semantic Actions" for more information)

### Example
```cpp
	using d = Terminal<0>;
	using c = Terminal<1>;
	using a = Terminal<2>;

	using Parser = LrParser<
		Production<
			Left<Z>,
			Right<d>,
			Rule1
		>,
		Production<
			Left<Z>,
			Right<X, Y, Z>,
			Rule2
		>,
		Production<
			Left<Y>,
			Right<>,
			Rule3
		>,
		Production<
			Left<Y>,
			Right<c>,
			Rule4
		>,
		Production<
			Left<X>,
			Right<Y>,
			Rule5
		>,
		Production<
			Left<X>,
			Right<a>,
			Rule6
		>
	>;
```

## Nonterminals
A Nonterminal class `A` must obey the following:
- `A` be default constructable
- `A` must publically derive from `Nonterminal<T>`

## Terminals
A Terminal must obey the following:
- Must have type `Terminal<n>`, where `n` is an integer that satisfies that two terminals `Terminal<n>` and `Terminal<m>` are equal if and only if `n == m`

## Semantic Actions
A semantic action `R` valid if and only if:
- Given a `Production` of the following form
```cpp
Production<
    Left<A>,
    Right<B1, B2, ..., BN>,
    R
>
```
then an instance of type `R` is callable with arguments `B1(), B2(), ..., BN()` and the result of `R()(B1(), B2(), ..., BN())` is of type `A`.

These requirements are verified at compile-time, with no overhead at runtime.
