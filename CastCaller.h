#ifndef _PARSE_CAST_CALLER
#define _PARSE_CAST_CALLER

#include <vector>
#include <boost/any.hpp>
#include <vector>
#include <boost/mpl/front.hpp>
#include <boost/mpl/pop_front.hpp>
#include <boost/mpl/size.hpp>

namespace parse {
	namespace detail {
		
		template<typename T>
		struct Cast {
			static auto cast(boost::any& a) -> decltype(boost::any_cast<T>(a)) {
				return boost::any_cast<T>(a);
			}
		};

		template<int N>
		struct Cast<parse::Terminal<N>> {
			static auto cast(boost::any& a) -> decltype(boost::any_cast<std::string>(a)) {
				return boost::any_cast<std::string>(a);
			}
		};

		template<typename Func, typename N, typename List, typename... Args>
		struct CastCall_helper {
			void operator()(std::vector<boost::any>& v, Args... args) {
				auto arg = v.at(v.size() - N::value);
				using T = typename mpl::front<List>::type;
				using Ts = typename mpl::pop_front<List>::type;
				auto arg2 = Cast<T>::cast(arg);
				CastCall_helper<Func, typename N::prior, Ts, Args..., decltype(arg2)>()(v, args..., arg2);
			}
		};

		template<typename Func, typename List, typename... Args>
		struct CastCall_helper<Func, mpl::long_<0>, List, Args...> {
			void operator()(std::vector<boost::any>& v, Args... args) {
				//Call the semantic action
				auto res = Func()(args...);
				//Pop all the arguments from the stack
				auto begin = v.begin() + v.size() - sizeof...(Args);
				v.erase(begin, v.end());
				v.push_back(res);
			}
		};

		template<typename Func, typename List>
		struct CastCall {
			void operator()(std::vector<boost::any>& v) {
				CastCall_helper<Func, typename mpl::size<List>::type, List>()(v);
			}
		};
	}
}

#endif
