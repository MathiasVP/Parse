#ifndef _PARSE_SYNTAX_H
#define _PARSE_SYNTAX_H

#include <boost/mpl/list.hpp>
#include <string>
#include <tuple>

namespace parse {
	using namespace boost;

	template<int N>
	struct Terminal {
		using type = std::string;
		static const int value;
	};

	template<int N>
	const int Terminal<N>::value = N;

	template<typename T>
	struct Nonterminal {
		using type = T;
	};

	template<typename Left, typename Right, typename Action>
	struct Production {
		static_assert(sizeof(Left) == 0, "Error: Right hand side of production must be of type std::tuple<Ts...>");
	};

	template<typename Left_, typename... Right_, typename Action_>
	struct Production<Left_, std::tuple<Right_...>, Action_> {
		using Left = Left_;
		using Right = typename mpl::list<Right_...>::type;
		using Action = Action_;
	};
}

#endif // !_PARSE_SYNTAX_H
