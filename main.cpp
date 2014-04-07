#include "parse.h"

#include <iostream>
#include <string>
#include <vector>

using namespace parse;

struct S : public Nonterminal<S> {
	
};

struct V : public Nonterminal<V> {

};

struct E : public Nonterminal<E> {

};

struct Rule1 {
	S operator()(V, std::string, E) {
		return S();
	}
};

struct Rule2 {
	S operator()(E) {
		return S();
	}
};

struct Rule3 {
	E operator()(V) {
		return E();
	}
};

struct Rule4 {
	V operator()(std::string) {
		return V();
	}
};

struct Rule5 {
	V operator()(std::string, E) {
		return V();
	}
};

int main(int argc, char* argv []) {
	using x = Terminal<0>;
	using deref = Terminal<1>;
	using assign = Terminal<2>;

	using Parser = LrParser<
		Production<
			Left<S>,
			Right<V, assign, E>,
			Rule1
		>,
		Production<
			Left<S>,
			Right<E>,
			Rule2
		>,
		Production<
			Left<E>,
			Right<V>,
			Rule3
		>,
		Production<
			Left<V>,
			Right<x>,
			Rule4
		>,
		Production<
			Left<V>,
			Right<deref, E>,
			Rule5
		>
	>;

	LrParser<
		Production<
			Left<S>,
			Right<V, assign, E>,
			Rule1
		>,
		Production<
			Left<S>,
			Right<E>,
			Rule2
		>,
		Production<
			Left<E>,
			Right<V>,
			Rule3
		>,
		Production<
			Left<V>,
			Right<x>,
			Rule4
		>,
		Production<
			Left<V>,
			Right<deref, E>,
			Rule5
		>
	>();

	std::vector<int> tokens = { 0, 2, 0, -1 };

	std::cout << "Starting parse..." << std::endl;
	std::cout << Parser::parse(tokens.begin(), tokens.end()) << std::endl;
	std::cout << "Parsing ended..." << std::endl;
	std::cin.get();
	return 0;
}
