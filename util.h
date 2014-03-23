#ifndef _PARSE_UTIL_H
#define _PARSE_UTIL_H

#include <fstream>
#include <typeinfo>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/pair.hpp>

namespace parse {
	namespace detail {
		using namespace boost;

		struct map_to_string_helper {
		private:
			std::ostream& out;

		public:
			map_to_string_helper(std::ostream& out) : out(out) {}

			template<typename T, typename U>
			void operator()(mpl::pair<T, U>) {
				out << typeid(T).name() << " -> " << typeid(U).name() << std::endl;
			}

			template<typename T>
			void operator()(T) {}
		};

		template<typename Map>
		void map_to_string(std::ostream& out) {
			mpl::for_each<Map>(map_to_string_helper(out));
		}

		struct set_to_string_helper {
		private:
			std::ostream& out;

		public:
			set_to_string_helper(std::ostream& out) : out(out) {}

			template<typename T>
			void operator()(T) {
				out << typeid(T).name() << std::endl;
			}
		};

		template<typename Set>
		void set_to_string(std::ostream& out) {
			mpl::for_each<Set>(set_to_string_helper(out));
		}

		template<unsigned int n, typename T, typename... Ts>
		struct nth {
			using type = typename nth<n - 1, Ts...>::type;
		};

		template<typename T, typename... Ts>
		struct nth<0, T, Ts...> {
			using type = T;
		};
	}
}

#endif // !_PARSE_UTIL_H
