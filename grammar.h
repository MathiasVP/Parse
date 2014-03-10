#ifndef _PARSE_GRAMMAR_H
#define _PARSE_GRAMMAR_H

#include <boost/mpl/insert.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/set.hpp>
#include <boost/mpl/map.hpp>
#include <boost/mpl/fold.hpp>
#include <fstream>
#include "firstfollownullable.h"

namespace parse {
	namespace detail {
		template<typename P, typename... Ps>
		struct LrParserHelper : LrParserHelper<Ps...> {
		private:
			//Ensure that the production is instantiated
			//so that static_asserts are evaluated
			P p;
		};

		template<typename P>
		struct LrParserHelper<P> {
		private:
			P p;
		};
	}

	template<typename... Ps_>
	struct LrParser : detail::LrParserHelper<Ps_...> {
	private:
		template<typename P, typename... Ps>
		struct ConsFollow {
			using type = typename mpl::insert<
			typename ConsFollow<Ps...>::type,
			mpl::pair<
			typename P::left::type,
			mpl::set<>
			>
			>::type;
		};

		template<typename P>
		struct ConsFollow<P> {
			using type = mpl::map<
			mpl::pair<
			typename P::left::type,
			mpl::set<>
			>
			>;
		};

		template<typename Map, typename T>
		struct AddSingleTerminal {
			using type = Map;
		};

		template<typename Map, int N>
		struct AddSingleTerminal<Map, Terminal<N>> {
			using type = typename mpl::if_<
			mpl::has_key<Map, Terminal<N>>,
			Map,
			typename mpl::insert<
			Map,
			mpl::pair<
			Terminal<N>,
			mpl::set<Terminal<N>>
			>
			>::type
			>::type;
		};

		template<typename Map, typename Rhs>
		struct AddTerminals {
		public:
			using type = typename mpl::fold<
				Rhs,
				Map,
				AddSingleTerminal<mpl::_1, mpl::_2>
			>::type;
		};

		template<typename P, typename... Ps>
		struct ConsFirst {
			using type = typename AddTerminals<
			typename ConsFirst<Ps...>::type,
			typename P::right::type
			>::type;
		};

		template<typename P>
		struct ConsFirst<P> {
			using type = typename AddTerminals<
			mpl::map<>,
			typename P::right::type
			>::type;
		};

		using ffn = detail::FirstFollowNullable<
			typename ConsFirst<Ps_...>::type,
			typename ConsFollow<Ps_...>::type,
			mpl::set<>,
			Ps_...>;
		using First = typename ffn::First;
		using Follow = typename ffn::Follow;
		using Nullable = typename ffn::Nullable;
	};
}

#endif // !_PARSE_GRAMMAR_H