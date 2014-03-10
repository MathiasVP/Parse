#include "parse.h"

#include <iostream>
#include <string>
#include <fstream>

using namespace parse;

struct X : public Nonterminal<X> {
	std::string s;

	X() {}
	X(std::string s) : s(move(s)) {}
};

struct Y : public Nonterminal<Y> {
	std::string s;

	Y() {}
	Y(std::string s) : s(move(s)) {}
};

struct Z : public Nonterminal<Z> {
	std::string s;

	Z() {}
	Z(std::string s) : s(move(s)) {}
};

struct Rule1 {
	Z operator()(std::string) {
		return Z("");
	}
};

struct Rule2 {
	Z operator()(X, Y, Z) {
		return Z("");
	}
};

struct Rule3 {
	Y operator()() {
		return Y("");
	}
};

struct Rule4 {
	Y operator()(std::string) {
		return Y("");
	}
};

struct Rule5 {
	X operator()(Y) {
		return X("");
	}
};

struct Rule6 {
	X operator()(std::string) {
		return X("");
	}
};

int main(int argc, char* argv []) {
	using d = Terminal<0>;
	using c = Terminal<1>;
	using a = Terminal<2>;

	LrParser<
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
	>();

	return 0;
}