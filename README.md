Parse
=====

Compile time (LR) type safe parser generator for C++

## Syntax

A parser is defined using the LrParser class.
An LrParser type takes a variadic number of template arguments, where each argument is a Production type.
A Production type takes 3 arguments:
- A nonterminal (See "Nonterminals" for more information)
- A std::tuple-type, which takes a variable number of nonterminals and terminals
- A semantic action, which is something that is calleble (See "Semantic Actions" for more information)

### Example
```cpp
	using namespace parse;
	using d = Terminal<0>;
	using c = Terminal<1>;
	using a = Terminal<2>;

	using Parser = LrParser<
		Production<
			Z,
			std::tuple<d>,
			Rule1
		>,
		Production<
			Z,
			std::tuple<X, Y, Z>,
			Rule2
		>,
		Production<
			Y,
			std::tuple<>,
			Rule3
		>,
		Production<
			Y,
			std::tuple<c>,
			Rule4
		>,
		Production<
			X,
			std::tuple<Y>,
			Rule5
		>,
		Production<
			X,
			std::tuple<a>,
			Rule6
		>
	>;
```

### Nonterminals
A Nonterminal class `A` must obey the following:
- `A` must publically derive from `Nonterminal<A>`

### Terminals
A Terminal must obey the following:
- Must have type `Terminal<n>`, where `n` is an integer that satisfies that two terminals `Terminal<n>` and `Terminal<m>` are equal if and only if `n == m`. These Terminal-types model token classes returned from some lexer.

### Semantic Actions
A semantic action `R` valid if and only if:
- Given a `Production` of the following form
```cpp
Production<
    A,
    std::tuple<B1, B2, ..., BN>,
    R
>
```
then an instance of type `R` is callable with arguments `B1{}, B2{}, ..., BN{}` and the result of `R{}(B1{}, B2{}, ..., BN{})` is of type `A`.

## Limitations
- The number of rows or columns in the LR parsing table can be no more than `BOOST_PP_LIMIT_REPEAT`, which sadly is a very real limitation when parsing complex languages.
- Due to the amount of template meta-programming used within the library, the compile time (even for toy examples) is almost unbearable.
