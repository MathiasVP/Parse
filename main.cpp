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

	auto table = Parser::myTable;
	int n = 0;
	std::cout << "\t$   x   *   =     S    E    V" << std::endl;
	for (const auto& row : table) {
		std::cout << n++ << "\t";
		for (const auto& elem : row) {
			std::string s;
			switch (elem.first) {
			case parse::detail::Operation::REDUCE:
				s = "r" + std::to_string(elem.second);
				break;
			case parse::detail::Operation::SHIFT:
				s = "s" + std::to_string(elem.second);
				break;
			case parse::detail::Operation::GOTO:
				s = "g" + std::to_string(elem.second);
				break;
			case parse::detail::Operation::ACCEPT:
				s = "a";
				break;
			}
			while (s.size() < 4) {
				s += " ";
			}
			std::cout << s;
		}
		std::cout << std::endl;
	}

	std::vector<int> tokens = {0, 2, 1, 0, -1};

	auto res = Parser::parse(tokens.begin(), tokens.end());

	std::cin.get();
	return 0;
}