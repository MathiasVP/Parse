#ifndef _PARSE_FIRST_FOLLOW_NULLABLE_H
#define _PARSE_FIRST_FOLLOW_NULLABLE_H

#include <boost/mpl/list.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/has_key.hpp>
#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/erase_key.hpp>
#include <boost/mpl/erase.hpp>
#include <boost/mpl/advance.hpp>
#include <boost/mpl/begin.hpp>
#include <boost/mpl/end.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/mpl/insert.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/set.hpp>
#include <boost/mpl/empty.hpp>
#include <boost/mpl/pair.hpp>

namespace parse { namespace detail {

	template<typename T, typename U>
	struct binary_mpl_equal : public mpl::equal<T, U> {};

	//Union two sets of type boost::mpl::set
	template<typename Set1_, typename Set2_>
	struct union_ {
		using Set1 = typename mpl::if_<
			std::is_same<typename Set1_::type, mpl::void_>,
			mpl::set0<>,
			typename Set1_::type
		>::type;

		using Set2 = typename mpl::if_<
			std::is_same<typename Set2_::type, mpl::void_>,
			mpl::set0<>,
			typename Set2_::type
		>::type;

		using type = typename mpl::fold<
			Set2,
			Set1,
			mpl::insert<mpl::_1, mpl::_2>
		>::type;
	};

	//Check if each production in Ys is nullable
	template<typename Nullable, typename Ys>
	struct isNullable {
		static const bool value =
			mpl::fold<
				Ys,
				std::true_type,
				mpl::if_<
					mpl::has_key<Nullable, mpl::_2>,
					mpl::_1,
					std::false_type
				>
			>::type::value;
	};

	template<template<class, class...> class F, typename Init, template<class...> class Compare, typename... Args>
	struct Fixpoint {
	private:
		template<typename isDone, typename Result>
		struct Repeat {
		private:
			using NewResult = typename F<Result, Args...>::type;
		public:
			using type = typename Repeat<
				typename Compare<Result, NewResult>::type, NewResult
			>::type;
		};

		template<typename Result>
		struct Repeat<mpl::true_, Result> {
			using type = Result;
		};

	public:
		using type = typename Repeat<mpl::false_, Init>::type;
	};

	template<template<class, class...> class F, typename Init, int N, typename... Args>
	struct Iterate {
	private:
		template<int n, typename Result>
		struct Repeat {
		private:
			using NewResult = typename F<Result, Args...>::type;
		public:
			using type = typename Repeat<n-1, NewResult
			>::type;
		};

		template<typename Result>
		struct Repeat<0, Result> {
			using type = Result;
		};

	public:
		using type = typename Repeat<N, Init>::type;
	};

	template<typename P>
	struct K {
		using type = typename mpl::int_<
		mpl::size<typename P::Right::type>::value
		>;
	};

	template<typename First, typename Nullable, typename P, typename... Ps>
	struct LoopFirst {
	private:
		using k = typename K<P>::type;
		using X = typename P::Left::type;
		using Ys = typename P::Right::type;

		template<typename i, typename First2>
		struct DoLoopFirst {
		private:
			using Yi = typename mpl::at<Ys, i>::type;

			//Y1, ..., Yi-1
			using Ys2 = typename mpl::erase<
				Ys,
				typename mpl::advance<
					typename mpl::begin<Ys>::type,
					i
				>::type,
				typename mpl::end<Ys>::type
			>::type;

			using First3 = typename mpl::eval_if<
				isNullable<Nullable, Ys2>,
				mpl::insert<
					typename mpl::erase_key<First2, X>::type,
					mpl::pair<
						X,
						typename union_<
							mpl::at<First2, X>,
							mpl::at<First2, Yi>
						>::type
					>
				>,
				mpl::identity<First2>
			>::type;

		public:
			using type = typename DoLoopFirst<
				typename i::next, First3
			>::type;
		};

		template<typename First2>
		struct DoLoopFirst<k, First2> {
			using type = First2;
		};

		using First2 = typename DoLoopFirst<mpl::int_<0>, First>::type;

	public:
		using type = typename LoopFirst<First2, Nullable, Ps...>::type;
	};

	template<typename First, typename Nullable>
	struct LoopFirst<First, Nullable, mpl::na> {
		using type = First;
	};

	template<typename Nullable, typename... Ps>
	struct LoopNullable {
	private:

		template<typename Nullable2, typename P2, typename... Ps2>
		struct DoLoopNullable {
		private:
			using X = typename P2::Left::type;
			using Ys = typename P2::Right::type;
			using Nullable3 = typename mpl::if_<
				isNullable<Nullable2, Ys>,
				typename mpl::insert<Nullable2, X>::type,
				Nullable2
			>::type;

		public:
			using type = typename DoLoopNullable<Nullable3, Ps2...>::type;
		};

		template<typename Nullable2>
		struct DoLoopNullable<Nullable2, mpl::na> {
			using type = Nullable2;
		};

		public:
			using type = typename DoLoopNullable<Nullable, Ps...>::type;
		};

		template<typename Follow, typename Nullable, typename First, typename P, typename... Ps>
		struct LoopFollow {
		private:
			using k = typename K<P>::type;
			using X = typename P::Left::type;
			using Ys = typename P::Right::type;

			template<typename i, typename j, typename Follow2>
			struct DoLoopFollow {
			private:
				using Yi = typename mpl::at<Ys, i>::type;
				using Yj = typename mpl::at<Ys, j>::type;

				//Yi+1, ..., Yk
				using Ys2 = typename mpl::erase<
					Ys,
					typename mpl::begin<Ys>::type,
					typename mpl::advance<
						typename mpl::begin<Ys>::type,
						i
					>::type
				>::type;

				//Yi+1, ..., Yj-1
				using Ys3 = typename mpl::erase<
					Ys2,
					typename mpl::advance_c<
						typename mpl::begin<Ys2>::type,
						j::value - i::value
					>::type,
					typename mpl::end<Ys2>::type
				>::type;

				using Follow3 = typename mpl::eval_if<
					isNullable<Nullable, Ys2>,
					mpl::insert<
						typename mpl::erase_key<Follow2, Yi>::type,
						mpl::pair<
							Yi,
							typename union_<
								mpl::at<Follow2, Yi>,
								mpl::at<Follow2, X>
							>::type
						>
					>,
					mpl::identity<Follow2>
				>::type;

				using Follow4 = typename mpl::eval_if<
					isNullable<Nullable, Ys3>,
					mpl::insert<
						typename mpl::erase_key<Follow3, Yi>::type,
						mpl::pair<
							Yi,
							typename union_<
								mpl::at<Follow3, Yi>,
								mpl::at<First, Yj>
							>::type
						>
					>,
					mpl::identity<Follow3>
				>::type;

			public:
				using type = typename DoLoopFollow<
					i, typename j::next, Follow4
				>::type;
			};

			template<typename i, typename Follow2>
			struct DoLoopFollow<i, k, Follow2> {
				using type = typename DoLoopFollow<
					typename i::next, typename i::next::next, Follow2
				>::type;
			};

			template<typename j, typename Follow2>
			struct DoLoopFollow<k, j, Follow2> {
				using type = Follow2;
			};

			using Follow2 = typename DoLoopFollow<
				mpl::int_<0>, mpl::int_<1>, Follow
			>::type;
		public:
			using type = typename LoopFollow<
				Follow2, Nullable, First, Ps...
			>::type;
		};

		template<typename Follow, typename Nullable, typename First>
		struct LoopFollow<Follow, Nullable, First, mpl::na> {
			using type = Follow;
		};

		template<typename First_, typename Follow_, typename Nullable_, typename... Ps_>
		struct FirstFollowNullable {
			using Nullable = typename Fixpoint<
				LoopNullable, Nullable_, binary_mpl_equal, Ps_..., mpl::na
			>::type;
			using First = typename Fixpoint<
				LoopFirst, First_, binary_mpl_equal, Nullable, Ps_..., mpl::na
			>::type;
			using Follow = typename Fixpoint<
				LoopFollow, Follow_, binary_mpl_equal, Nullable, First, Ps_..., mpl::na
			>::type;
		};
	}

	template<typename First, typename Nullable, typename List>
	struct ListFirst {

		template<typename T>
		using Car = typename mpl::front<T>::type;
		template<typename T>
		using Cdr = typename mpl::pop_front<T>::type;

		template<typename Head, typename Tail, typename empty>
		struct Loop {
			using type = typename mpl::eval_if<
				detail::isNullable<Nullable, mpl::list1<Head>>,
				detail::union_<
					mpl::at<First, Head>,
					Loop<
						Car<Tail>,
						Cdr<Tail>,
						typename mpl::empty<Cdr<Tail>>::type
					>
				>,
				mpl::at<First, Head>
			>::type;
		};

		template<typename Head, typename Tail>
		struct Loop<Head, Tail, mpl::true_> {
			using type = mpl::set1<Head>;
		};

		using type = typename mpl::eval_if<
			mpl::empty<List>,
			mpl::list0<>,
			Loop<Car<List>, Cdr<List>, typename mpl::empty<Cdr<List>>::type>
		>::type;
	};


}

#endif // !_PARSE_FIRST_FOLLOW_NULLABLE_H
