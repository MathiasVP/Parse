#ifndef _PARSE_SYNTAX_H
#define _PARSE_SYNTAX_H

#include <boost/mpl/list.hpp>
#include <boost/typeof/std/utility.hpp>
#include <string>

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
	struct Left {
		using type = T;
	};

	template<typename... Ts>
	struct Right {
		using type = typename mpl::list<Ts...>::type;
	};

	template<typename T>
	struct Nonterminal {
		using type = T;
	};

	template<typename Left_, typename Right_, typename Action_>
	struct Production {
		using Left = Left_;
		using Right = Right_;
		using Action = Action_;
	};
}

#endif // !_PARSE_SYNTAX_H
