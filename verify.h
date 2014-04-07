#ifndef _PARSE_VERIFY_H
#define _PARSE_VERIFY_H

#include <boost/mpl/at.hpp>
#include <type_traits>

namespace parse {
	using namespace boost;

	template<int N>
	struct Terminal {
		using type = std::string;
		static const int value;
	};

	template<int N>
	const int Terminal<N>::value = N;

	template<typename T, typename U>
	struct IsValidFormalParam : std::is_same<T, U> {

	};

	template<int N>
	struct IsValidFormalParam<Terminal<N>, std::string> {
		static const bool value = true;
	};

	template<typename Vector1, typename Vector2, int i>
	struct VerifyRightHandSideHelper {
		using type = std::integral_constant<
		bool,
		IsValidFormalParam<
		typename mpl::at_c<Vector1, i>::type,
		typename mpl::at_c<Vector2, i + 1>::type
		>::value
		&& VerifyRightHandSideHelper<Vector1, Vector2, i - 1>::type::value
		>;
	};

	template<typename Vector1, typename Vector2>
	struct VerifyRightHandSideHelper<Vector1, Vector2, -1> {
		using type = std::true_type;
	};

	template<typename Vector1, int N, typename Vector2, int M>
	struct VerifyRightHandSide {
		//First verify equal length
		static const bool value = mpl::if_c<
		N == M,
		typename VerifyRightHandSideHelper<Vector1, Vector2, N - 1>::type,
		std::false_type
		>::type::value;
	};

	template<int N, typename Vector, int M>
	struct VerifyRightHandSide<mpl::list0<>, N, Vector, M> {
		//Specialize for deriving the empty string. We expect no parameters
		static const bool value = (M == 0);
	};
}

#endif // !_PARSE_VERIFY_H
