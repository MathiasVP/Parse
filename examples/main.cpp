//The dimensions of the LR table, which sadly needs to be set manually.
//A compile-time error will be created if the value is not large enough.
#define PARSE_TABLE_ROWS 75
#define PARSE_TABLE_COLS 11

#include "../parse/parse.h"

#include <iostream>
#include <string>
#include <vector>
#include <utility>

using namespace parse;

/*
	We first define the nonterminals,
	which would be the AST in a more
	complex program.
	In this example they all just hold an
	integer value representing the result
	of some computation.
*/
struct E : public Nonterminal<E> {
private:
	int n;
public:
	E(int n) : n(n) {}
	int value() { return n; }
};

struct T : public Nonterminal<T> {
private:
	int n;
public:
	T(int n) : n(n) {}
	int value() { return n; }
};

struct F : public Nonterminal<F> {
private:
	int n;
public:
	F(int n) : n(n) {}
	int value() { return n; }
};

/*
	We now define the semantic actions, which are
	the actions to execute based on the grammar rules
	that are instantiated in order to parse the input.
	In this example these just compute integer results.
*/
struct Plus {
	E operator()(E e, std::string, T t) {
		return e.value() + t.value();
	}
};

struct Minus {
	E operator()(E e, std::string, T t) {
		return e.value() - t.value();
	}
};

struct Term {
	E operator()(T t) {
		return t.value();
	}
};

struct Times {
	T operator()(T t, std::string, F f) {
		return t.value() * f.value();
	}
};

struct Divide {
	T operator()(T t, std::string, F f) {
		return t.value() / f.value();
	}
};

struct Factor {
	T operator()(F f) {
		return f.value();
	}
};

struct Number {
	F operator()(std::string n) {
		return std::stoi(n);
	}
};

struct Parenthesis {
	F operator()(std::string, E e, std::string) {
		return e.value();
	}
};

int main(int argc, char* argv []) {
	/* We now define the different classes of tokens.
	   These classify all the types of input that can
	   occur in a string within the language we define.
	*/
	using plus = Terminal<0>;
	using minus = Terminal<1>;
	using div = Terminal<2>;
	using mult = Terminal<3>;
	using num = Terminal<4>;
	using lparen = Terminal<5>;
	using rparen = Terminal<6>;

	/* We're now ready to define the parser, and this is done
	   by defining each production by using the Production-type.
	   A production is defined as Production<L, R, A>, where
	   L is a type that derives from Nonterminal<L>,
	   R is of type std::tuple<R1, R2, ... RN>
	   A is a semantic action type.
	*/
	using Parser = LrParser<
		Production< //E -> E + T
			E,
			std::tuple<E, plus, T>,
			Plus
		>,
		Production< //E -> E - T
			E,
			std::tuple<E, minus, T>,
			Minus
		>,
		Production< //E -> T
			E,
			std::tuple<T>,
			Term
		>,
		Production< //T -> T * F
			T,
			std::tuple<T, mult, F>,
			Times
		>,
		Production< //T -> T / F
			T,
			std::tuple<T, div, F>,
			Divide
		>,
		Production< //T -> F
			T,
			std::tuple<F>,
			Factor
		>,
		Production< //F -> [0-9]+ (Anything classified as a Terminal<4>, really)
			F,
			std::tuple<num>,
			Number
		>,
		Production< //F -> ( E )
			F,
			std::tuple<lparen, E, rparen>,
			Parenthesis
		>
	>;

	//A sample input. Note: This is the first code we've written which actually happens at runtime.
	std::vector<std::pair<std::string, int>> tokens = {
		{"2", num::value}, {"*", mult::value}, {"3", num::value}, {"+", plus::value}, {"1", num::value}, {"", -1}
	};

	//Parse the input.
	//Returns boost::optional<E> since E is the first production,
	//and therefor the start-production
	auto res = Parser::parse(tokens.begin(), tokens.end());
	if(res) {
		//Successful parse. It's now safe to call .get() which gives us the object returned
		//from the final semantic action. In this example we computed 2 * 3 + 1 = 7
		std::cout << "Correct input with result: " << res.get().value() << std::endl;
	}
	else {
		std::cout << "Input is malformed" << std::endl;
	}

	return 0;
}
