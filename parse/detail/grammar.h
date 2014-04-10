#ifndef _PARSE_GRAMMAR_H
#define _PARSE_GRAMMAR_H

#include <boost/mpl/insert.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/set.hpp>
#include <boost/mpl/map.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/insert.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/optional.hpp>
#include <boost/mpl/long.hpp>
#include <boost/mpl/plus.hpp>

#include <boost/preprocessor.hpp>

#include <boost/any.hpp>

#include <string>
#include "firstfollownullable.h"
#include "CastCaller.h"

namespace parse {
	namespace detail {

		enum class Operation {
			SHIFT,
			GOTO,
			REDUCE,
			ACCEPT,
			INVALID
		};

		struct Start : public Nonterminal<Start> {};

		template<typename Lhs_, typename Rhs_, typename Pos_, typename Lookahead_>
		struct ItemT {
			using Left = Lhs_;
			using Right = Rhs_;
			using Pos = Pos_;
			using Lookahead = Lookahead_;
		};

		template<typename From_, typename To_, typename Lookahead_>
		struct ShiftT {
			using From = From_;
			using To = To_;
			using Lookahead = Lookahead_;
		};

		template<typename From_>
		struct AcceptT {
			using From = From_;
			using Lookahead = Terminal<-1>;
		};

		template<typename From_, typename To_, typename Lookahead_>
		struct GotoT {
			using From = From_;
			using To = To_;
			using Lookahead = Lookahead_;
		};

		template<typename From_, typename Left_, typename Right_, typename Lookahead_>
		struct ReduceT {
			using From = From_;
			using Left = Left_;
			using Right = Right_;
			using Lookahead = Lookahead_;
		};

		template<typename From_, typename To_, typename Lookahead_>
		struct EdgeT {
			using type = GotoT< From_, To_, Lookahead_>;
		};

		template<typename From_, typename To_, int N>
		struct EdgeT<From_, To_, Terminal<N>> {
			using type = ShiftT< From_, To_, Terminal<N>>;
		};

		template<typename From_, typename To_>
		struct EdgeT<From_, To_, Terminal<-1>> {
			using type = AcceptT<From_>;
		};

		template<typename T>
		struct Pos : T::Pos {};

		template<typename T>
		struct Left : T::Left {};

		template<typename T>
		struct Right : T::Right {};

		template<typename T>
		struct Lookahead : T::Lookahead {};

		template<typename S>
		struct IsTerminalHelp : mpl::false_ {};

		template<int N>
		struct IsTerminalHelp<Terminal<N>> : mpl::true_{};

		template<typename T>
		struct IsTerminal : IsTerminalHelp<typename T::type> {};

		template<typename List1, typename E>
		struct Append
			: mpl::insert<List1, typename mpl::end<List1>::type, E> {};

		template<typename T, typename U>
		struct LeftIs
			: mpl::bool_<
			std::is_same<T, typename U::Left>::value
			> {};

		template<typename First, typename Productions, typename Nullable, typename Items, typename Item>
		struct ClosureLoop {

			using B = typename mpl::erase<
				typename Item::Right,
				typename mpl::begin<Right<Item>>::type,
				typename mpl::advance<
					typename mpl::begin<typename Item::Right>::type,
					typename Item::Pos::next
				>::type
			>::type;

			using z = typename Item::Lookahead;
			using FirstBz = typename ListFirst<
				First,
				Nullable,
				typename Append<B, z>::type
			>::type;

			using X = typename mpl::at<typename Item::Right, typename Item::Pos>::type;

			template<typename P, typename Items2>
			struct LoopFirsts {
				using type = typename mpl::fold<
					FirstBz,
					Items2,
					mpl::insert<
						mpl::_1,
						ItemT<X, typename Right<P>::type, mpl::int_<0>, mpl::_2>
					>
				>::type;
			};

			using type = typename mpl::fold<
				Productions,
				Items,
				mpl::if_<
					LeftIs<X, mpl::_2>,
					LoopFirsts<mpl::_2, mpl::_1>,
					mpl::_1
				>
			>::type;
		};

		template<typename S>
		struct IsNonterminalAtPos
			: mpl::and_<
				mpl::less<
					typename S::Pos,
					typename mpl::size<typename S::Right>
				>,
				mpl::not_<
					IsTerminal<
						mpl::at<typename S::Right, typename S::Pos>
					>
				>
			>::type {};

		template<typename I, typename First, typename Nullable, typename... Ps>
		struct ClosureRepeat {
			using List = typename mpl::list<Ps...>::type;
			//Loop through each item of the form (A -> a.XB, z)
			using type = typename mpl::fold<
				I,
				I,
				mpl::if_<
					IsNonterminalAtPos<mpl::_2>,
					ClosureLoop<First, List, Nullable, mpl::_1, mpl::_2>,
					mpl::_1
				>
			>::type;

			
		};

		template<typename First, typename Nullable, typename I, typename... Ps>
		struct Closure {
			using type = typename detail::Fixpoint<
				ClosureRepeat,
				I,
				detail::binary_mpl_equal,
				First,
				Nullable,
				Ps...
			>::type;
		};

		template<typename First, typename Nullable, typename Items, typename X, typename... Ps>
		struct Goto {

			template<typename T, typename U>
			struct left_lazy_is_same
				: mpl::bool_<
					std::is_same<typename T::type, U>::value
				>
			{};
			template<typename Item>
			struct isX
				: mpl::and_<
					mpl::less<
						typename Item::Pos,
						mpl::size<typename Item::Right>
					>,
					left_lazy_is_same<
						mpl::at<typename Item::Right, typename Item::Pos>,
						X
					>
				> {};

			template<typename T>
			struct MakeItem {
				using type = ItemT<
					typename T::Left,
					typename T::Right,
					typename mpl::next<typename T::Pos>::type,
					typename T::Lookahead
				>;
			};

			using J = typename mpl::fold<
				Items,
				mpl::set0<>,
				mpl::if_<
					isX<mpl::_2>,
					mpl::insert<
						mpl::_1,
						MakeItem<mpl::_2>
					>,
					mpl::_1
				>
			>::type;
			using type = typename Closure<First, Nullable, J, Ps...>::type;
		};

		template<typename TE_, typename First, typename Nullable, typename... Ps>
		struct ConstructTableLoop {
			
			template<typename TE2, typename I>
			struct InnerLoop {
				template<typename TE, typename Item>
				struct ComputeTE {
					using T = typename mpl::at_c<TE, 0>::type;
					using E = typename mpl::at_c<TE, 1>::type;

					using X = typename mpl::at<typename Item::Right, typename Item::Pos>::type;

					using J = typename Goto<First, Nullable, I, X, Ps...>::type;

					using T2 = typename mpl::eval_if<
						boost::is_same<
							X,
							Terminal<-1>
						>,
						mpl::identity<T>,
						mpl::insert<T, J>
					>::type;
					using E2 = typename mpl::insert<E, typename EdgeT<I, J, X>::type>::type;
					using type = mpl::list2<T2, E2>;
				};
				
				using type = typename mpl::fold<
					I,
					TE2,
					mpl::if_<
						mpl::less<
							Pos<mpl::_2>,
							mpl::size<Right<mpl::_2>>
						>,
						ComputeTE<mpl::_1, mpl::_2>,
						mpl::_1
					>
				>::type;
			};
			
			using T = typename mpl::at_c<TE_, 0>::type;
			using type = typename mpl::fold<
				T,
				TE_,
				InnerLoop<mpl::_1, mpl::_2>
			>::type;
		};

		template<typename Table>
		struct ConstructReduces {
			template<typename R, typename I>
			struct InnerLoop {

				template<typename Item>
				struct MakeReduce {
					using type = ReduceT<
						I,
						typename Item::Left,
						typename Item::Right,
						typename Item::Lookahead
					>;
				};

				using type = typename mpl::fold<
					I,
					R,
					mpl::if_<
						mpl::equal_to<
							Pos<mpl::_2>,
							mpl::size<Right<mpl::_2>>
						>,
						mpl::insert<
							mpl::_1,
							MakeReduce<mpl::_2>
						>,
						mpl::_1
					>
				>::type;		
			};

			using T = typename mpl::at_c<Table, 0>::type;
			using SG = typename mpl::at_c<Table, 1>::type;
			using type = typename mpl::fold<
				T,
				SG,
				InnerLoop<mpl::_1, mpl::_2>
			>::type;
		};

		template<typename T>
		struct IsShift : mpl::false_ {};

		template<typename From, typename To, typename Lookahead>
		struct IsShift<ShiftT<From, To, Lookahead>> : mpl::true_{};

		template<typename NumTerminals, typename NumNonterminals, typename SGR>
		struct CreateIndexableTable {

			template<typename Seq, typename Item>
			struct HasKey
				: mpl::has_key<
					typename mpl::at_c<Seq, 0>::type,
					typename Item::From
				> {};

			template<typename Seq, typename Item>
			struct GetKey
				: mpl::at<
					typename mpl::at_c<Seq, 0>::type,
					typename Item::From
				>{};


			template<typename Map, typename Item>
			struct Insert {
				using Int = typename GetKey<Map, Item>::type;
				using From = typename Item::From;
				using type = mpl::list3<
					typename mpl::insert<
						typename mpl::at_c<Map, 0>::type,
						mpl::pair<
							From,
							Int
						>
					>::type,
					typename mpl::insert<
						typename mpl::at_c<Map, 1>::type,
						mpl::pair<
							Int,
							From
						>
					>::type,
					typename mpl::at_c<Map, 2>::type
				>;
			};

			template<typename Map, typename Item>
			struct InsertAndUpdate {
				using From = typename Item::From;
				using Int = typename mpl::at_c<Map, 2>::type;
				using type = mpl::list3<
					typename mpl::insert<
						typename mpl::at_c<Map, 0>::type,
						mpl::pair<
							From,
							Int
						>
					>::type,
					typename mpl::insert<
						typename mpl::at_c<Map, 1>::type,
						mpl::pair<
							Int,
							From
						>
					>::type,
					typename Int::next
				>;
			};

			using SGR2 = typename mpl::fold<
				SGR,
				mpl::list3<mpl::map0<>, mpl::map0<>, mpl::int_<0>>,
				mpl::if_<
					HasKey<mpl::_1, mpl::_2>,
					Insert<mpl::_1, mpl::_2>,
					InsertAndUpdate<mpl::_1, mpl::_2>
				>
			>::type;

			using RowToInt = typename mpl::at_c<SGR2, 0>::type;
			using IntToRow = typename mpl::at_c<SGR2, 1>::type;
			using NumRows = typename mpl::at_c<SGR2, 2>::type;
		};

		//P is the original starting nonterminal, before we argument the grammar
		template<typename NumTerminals, typename NumNonTerminals, typename First, typename Nullable, typename P, typename... Ps>
		struct ConstructTable {			
			using StartItem = ItemT<
				typename P::Left,
				typename P::Right,
				mpl::int_<0>,
				Terminal<-1>
			>;
			
			using T2 = typename Closure<First, Nullable, mpl::set1<StartItem>, P, Ps...>::type;
			using TableSets = typename detail::Fixpoint<
				ConstructTableLoop,
				mpl::list2<mpl::set1<T2>, mpl::set0<>>,
				detail::binary_mpl_equal,
				First, Nullable, P, Ps...
			>::type;

			//Shifts, gotos and reduces all in one set!
			using Table = typename ConstructReduces<TableSets>::type;

			using result = typename detail::CreateIndexableTable<NumTerminals, NumNonTerminals, Table>;
			using RowToInt = typename result::RowToInt;
			using IntToRow = typename result::IntToRow;
			using NumRows = typename result::NumRows;
		};

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

	template<typename P_, typename... Ps_>
	struct LrParser : detail::LrParserHelper<P_, Ps_...> {
	private:
		template<typename P, typename... Ps>
		struct ConsFollow {
			using type = typename mpl::insert<
			typename ConsFollow<Ps...>::type,
			mpl::pair<
			typename P::Left,
			mpl::set0<>
			>
			>::type;
		};

		template<typename P>
		struct ConsFollow<P> {
			using type = mpl::map1<
				mpl::pair<
					typename P::Left,
					mpl::set0<>
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
						mpl::set1<Terminal<N>>
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
			typename P::Right
			>::type;
		};

		template<typename P>
		struct ConsFirst<P> {
			using type = typename AddTerminals<
			mpl::map0<>,
			typename P::Right
			>::type;
		};

		//Augment the grammar by creating a production: S' -> S $
		using ParseResultType = typename P_::Left;
		struct ParseCompleted {
			detail::Start operator()(ParseResultType, std::string) {
				return detail::Start();
			}
		};
		using StartP = Production<
			detail::Start,
			std::tuple<ParseResultType, Terminal<-1>>,
			ParseCompleted
		>;

		using FirstInit = typename ConsFirst<StartP, P_, Ps_...>::type;
		using FollowInit = typename ConsFollow<StartP, P_, Ps_...>::type;
		using Productions = typename mpl::list<P_, Ps_...>::type;
		//Compute the first, follow and nullable sets
		using ffn = detail::FirstFollowNullable<
			FirstInit,
			FollowInit,
			mpl::set0<>,
			StartP, P_, Ps_...>;
		using First = typename ffn::First;
		using Follow = typename ffn::Follow;
		using Nullable = typename ffn::Nullable;

		using NumTerminals = typename mpl::size<FirstInit>::type;
		using NumNonterminals = typename mpl::size<
			typename mpl::fold<
				typename mpl::list<P_, Ps_...>::type,
				mpl::set0<>,
				mpl::insert<mpl::_1, detail::Left<mpl::_2>>
			>::type
		>::type;

		using TableInfo = typename detail::ConstructTable<
			NumTerminals,
			NumNonterminals,
			First,
			Nullable,
			StartP, P_, Ps_...
		>;

		public:

		template<typename Seq, typename Prod>
		struct InsertAndUpdate {
			using toInt = typename mpl::at_c<Seq, 0>::type;
			using fromInt = typename mpl::at_c<Seq, 1>::type;
			using Value = typename mpl::at_c<Seq, 2>::type;
			using Production = mpl::pair<
				typename Prod::Left,
				typename Prod::Right
			>;
			using type = mpl::list3<
				typename mpl::insert<
					toInt,
					mpl::pair<Production, Value>
				>::type,
				typename mpl::insert<
					fromInt,
					mpl::pair<Value, Production>
				>::type,
				typename Value::next
			>;
		};

		using ProductionMap = typename mpl::fold<
			typename mpl::list<P_, Ps_...>::type,
			mpl::list3<mpl::map0<>, mpl::map0<>, mpl::int_<0>>,
			InsertAndUpdate<mpl::_1, mpl::_2>
		>::type;
		using ProductionToInt = typename mpl::at_c<ProductionMap, 0>::type;
		using IntToProduction = typename mpl::at_c<ProductionMap, 1>::type;
		using Table = typename TableInfo::Table;
		using NumRows = typename TableInfo::NumRows;
		using NumCols = typename mpl::plus<NumTerminals, NumNonterminals>::type;

		template<typename Seq, typename Prod>
		struct HasKey
			: mpl::has_key<
				typename mpl::at_c<Seq, 0>::type,
				typename Prod::Left
			>{};

		template<typename Seq, typename Prod>
		struct InsertLeftAndUpdate {
			using ToInt = typename mpl::at_c<Seq, 0>::type;
			using FromInt = typename mpl::at_c<Seq, 1>::type;
			using Value = typename mpl::at_c<Seq, 2>::type;
			using Left = typename Prod::Left;
			using type = mpl::list3<
				typename mpl::insert<
					ToInt,
					mpl::pair<Left, Value>
				>::type,
				typename mpl::insert<
					FromInt,
					mpl::pair<Value, Left>
				>::type,
				typename Value::next
			>;
		};

		using ColMap = typename mpl::fold<
			typename mpl::list<P_, Ps_...>::type,
			mpl::list3<mpl::map0<>, mpl::map0<>, typename NumTerminals::prior>,
			mpl::if_<
				HasKey<mpl::_1, mpl::_2>,
				mpl::_1,
				InsertLeftAndUpdate<mpl::_1, mpl::_2>
			>
		>::type;
		using ColToInt = typename mpl::at_c<ColMap, 0>::type;
		using IntToCol = typename mpl::at_c<ColMap, 1>::type;
		using IntToRow = typename TableInfo::IntToRow;
		using RowToInt = typename TableInfo::RowToInt;

		template<typename From, typename To, typename Lookahead, typename It, typename T>
		static void tableOperationHelper(detail::ShiftT<From, To, Lookahead>,
			It& begin, It& end, int& state, std::vector<T>& stack,
			T& symbol, std::vector<boost::any>& objStack) {

			if(begin == end) {
				state = -1;
				return;			
			}
			objStack.push_back(symbol.first);
			//stack.emplace_back("", Lookahead::value);
			state = mpl::at<RowToInt, To>::type::value;
			stack.emplace_back("", state);
			symbol = *begin++;
		}

		template<typename From, typename To, typename Lookahead, typename It, typename T>
		static void tableOperationHelper(detail::GotoT<From, To, Lookahead>,
			It& begin, It& end, int& state, std::vector<T>& stack,
			T& symbol, std::vector<boost::any>& objStack) {

			symbol = stack.back();
			stack.pop_back();
			state = mpl::at<RowToInt, To>::type::value;
			stack.emplace_back("", state);
		}

		template<typename From, typename Left, typename Right, typename Lookahead, typename It, typename T>
		static void tableOperationHelper(detail::ReduceT<From, Left, Right, Lookahead>,
			It& begin, It& end, int& state, std::vector<T>& stack,
			T& symbol, std::vector<boost::any>& objStack) {
			for(int i = 0; i < mpl::size<Right>::type::value; ++i) {
				stack.pop_back();
			}
			state = stack.back().second;
			const int n = mpl::at<ProductionToInt, mpl::pair<Left, Right>>::type::value;
			using Action = typename mpl::at_c<Productions, n>::type::Action;
			//Build up action param list and call an instance of Action
			detail::CastCall<Action, Right>()(objStack);
			stack.push_back(symbol);
			symbol = {"", mpl::at<ColToInt, Left>::type::value};
		}

		template<typename From, typename It, typename T>
		static void tableOperationHelper(detail::AcceptT<From>,
			It& begin, It& end, int& state, std::vector<T>& stack,
			T& symbol, std::vector<boost::any>& objStack) {
			state = -2;
		}

		template<typename It, typename T>
		static void tableOperationHelper(mpl::na,
			It& begin, It& end, int& state, std::vector<T>& stack,
			T& symbol, std::vector<boost::any>& objStack) {
			state = -1;
		}

		template<int symbolIndex, int stateIndex, bool symbolOutOfBounds, bool stateOutOfBounds>
		struct TableOperation {
			using Row = typename mpl::at<IntToRow, mpl::int_<stateIndex>>::type;
			using Col = typename mpl::eval_if<
				mpl::has_key<IntToCol, mpl::long_<symbolIndex>>,
				mpl::at<IntToCol, mpl::long_<symbolIndex>>,
				mpl::identity<Terminal<symbolIndex>>
			>::type;

			struct Predicate {
				template<typename Op>
				struct apply {
					using type = typename mpl::and_<
						boost::is_same<typename Op::From, Row>,
						boost::is_same<typename Op::Lookahead, Col>
					>::type;
				};
			};

			using res = typename mpl::find_if<Table, Predicate>::type;
			using res2 = typename mpl::eval_if<
				boost::is_same<
					res,
					typename mpl::end<Table>::type
				>,
				mpl::na,
				mpl::deref<res>
			>::type;

			template<typename It, typename T>
			static void doit(It& begin, It& end, int& state, std::vector<T>& stack,
				T& symbol, std::vector<boost::any>& objStack) {
				tableOperationHelper(res2(), begin, end, state, stack, symbol, objStack);
			}
		};

		template<int symbolIndex, int stateIndex>
		struct TableOperation<symbolIndex, stateIndex, false, false> {
			
			template<typename It, typename T>
			static void doit(It& begin, It& end, int& state, std::vector<T>& stack,
				T& symbol, std::vector<boost::any>& objStack) {}
		};

#define PARSE_SWITCH_CASE_TERMINAL(_, symbolIndex, stateIndex) \
	case symbolIndex-1: TableOperation<symbolIndex-1, stateIndex, symbolIndex-1 < NumCols::value, stateIndex < NumRows::value>::doit(begin, end, state, stack, symbol, objStack); break;

#define PARSE_SWITCH_CASE_STATE(_, stateIndex, __) \
	case stateIndex:\
	switch (symbol.second) {\
		BOOST_PP_REPEAT(PARSE_TABLE_COLS, PARSE_SWITCH_CASE_TERMINAL, stateIndex) \
	} \
	break;

		template<typename It>
		static boost::optional<ParseResultType> parse(It begin, It end) {
			static_assert(PARSE_TABLE_COLS >= NumCols::value, "Error! PARSE_TABLE_COLS not large enough");
			static_assert(PARSE_TABLE_ROWS >= NumRows::value, "Error! PARSE_TABLE_ROWS not large enough");
			std::vector<typename std::iterator_traits<It>::value_type> stack;
			std::vector<boost::any> objStack;
			//I suspect that the last row is always the initial closure set. Not yet 100% sure though
			auto state = NumRows::value-1;
			stack.emplace_back("", state);
			auto symbol = *begin++;
			while (state >= 0) {
				switch (state) {
					BOOST_PP_REPEAT(PARSE_TABLE_ROWS, PARSE_SWITCH_CASE_STATE, _)
				}
			}
			if(state == -2) return boost::any_cast<ParseResultType>(objStack.back());
			else return boost::optional<ParseResultType>();
		}
	};
}



#endif // !_PARSE_GRAMMAR_H
