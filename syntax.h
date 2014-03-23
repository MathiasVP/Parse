#ifndef _PARSE_SYNTAX_H
#define _PARSE_SYNTAX_H

#include <boost/mpl/list.hpp>
#include <boost/function_types/result_type.hpp>
#include <boost/function_types/parameter_types.hpp>
#include <boost/function_types/function_arity.hpp>
#include <boost/typeof/std/utility.hpp>
#include <boost/mpl/size.hpp>
#include <string>
#include "verify.h"

namespace parse {
	using namespace boost;

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

	template<typename Left_, typename Right_, typename Action>
	struct Production {
		using Left = Left_;
		using Right = Right_;
		//Verify the production returns the correct type
		using result_type = typename function_types::result_type<typename BOOST_TYPEOF(&Action::operator())>::type;
		static_assert(std::is_same<typename Left::type, result_type>::value,
			"Result type of action must match the left hand side of the production.");

		//Verify formal param types match the right hand side
		using arg_types = typename function_types::parameter_types<typename BOOST_TYPEOF(&Action::operator())>::type;
		static_assert(
			VerifyRightHandSide<
				typename Right::type,
				mpl::size<typename Right::type>::value,
				arg_types,
				mpl::size<arg_types>::value - 1 //We subtract one to remove the this-ptr
			>::value,
			"Invalid formal parameters for action"
			);
	};
}

#endif // !_PARSE_SYNTAX_H