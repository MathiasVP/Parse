#define PARSE_TABLE_ROWS 18
#define PARSE_TABLE_COLS 7

#include "parse.h"

#include <iostream>
#include <string>
#include <vector>
#include <utility>

using namespace parse;

struct S;
struct V;
struct E;

struct S : public Nonterminal<S> {
private:
	std::string s;
public:
	S(E e);
	S(V v, std::string s, E e);

	std::string to_string() { return s; }
};

struct V : public Nonterminal<V> {
private:
	std::string s;
public:
	V(std::string s);
	V(std::string s, E e);

	std::string to_string() { return s; }
};

struct E : public Nonterminal<E> {
private:
	std::string s;
public:
	E(V v);

	std::string to_string() { return s; }
};

S::S(E e) : s("E[" + e.to_string() + "]") {}
S::S(V v, std::string s, E e) : s("V[" + v.to_string() + "], " + s + ", E[" + e.to_string() + "]") {}

V::V(std::string s) : s(s) {}
V::V(std::string s, E e) : s(s + ", E[" + e.to_string() + "]") {}

E::E(V v) : s("V[" + v.to_string() + "]") {}

struct Rule1 {
	S operator()(V v, std::string s, E e) {
		return S(v, s, e);
	}
};

struct Rule2 {
	S operator()(E e) {
		return S(e);
	}
};

struct Rule3 {
	E operator()(V v) {
		return E(v);
	}
};

struct Rule4 {
	V operator()(std::string s) {
		return V(s);
	}
};

struct Rule5 {
	V operator()(std::string s, E e) {
		return V(s, e);
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

	std::vector<std::pair<std::string, int>> tokens = { {"x", 0}, {"=", 2}, {"*", 1}, {"*", 1}, {"*", 1}, {"x", 0}, {"", -1}};

	auto res = Parser::parse(tokens.begin(), tokens.end());
	if(res) {
		std::cout << res.get().to_string() << std::endl;
	}
	else {
		std::cout << "Input is malformed" << std::endl;
	}

	return 0;
}
