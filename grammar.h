#ifndef _PARSE_GRAMMAR_H
#define _PARSE_GRAMMAR_H

#include <boost/mpl/insert.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/set.hpp>
#include <boost/mpl/map.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/insert.hpp>
#include <boost/mpl/same_as.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/partition.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/deref.hpp>
#include <fstream>
#include <string>
#include <array>
#include <stack>
#include <map>
#include "firstfollownullable.h"
#include "indices.h"

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

		template<typename From_, typename Left_, typename Right_, typename Lookahead_>
		struct ReduceT {
			using From = From_;
			using Left = Left_;
			using Right = Right_;
			using Lookahead = Lookahead_;
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
			std::is_same<T, typename U::Left::type>::value
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

		template<typename T>
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

			using type = typename mpl::fold<
				T,
				mpl::set0<>,
				InnerLoop<mpl::_1, mpl::_2>
			>::type;
		};

		template<typename T>
		struct IsShift : mpl::false_ {};

		template<typename From, typename To, typename Lookahead>
		struct IsShift<ShiftT<From, To, Lookahead>> : mpl::true_{};

		template<typename NumTerminals, typename NumNonterminals, typename S, typename G, typename R>
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
			struct Insert
				: mpl::list2<
					typename mpl::insert<
						typename mpl::at_c<Map, 0>::type,
						mpl::pair<
							typename Item::From,
							typename GetKey<Map,Item>::type
						>
					>::type,
					typename mpl::at_c<Map, 1>::type
				>::type {};

			template<typename Map, typename Item>
			struct InsertAndUpdate
				: mpl::list2<
					typename mpl::insert<
						typename mpl::at_c<Map, 0>::type,
						mpl::pair<
							typename Item::From,
							typename mpl::at_c<Map, 1>::type
						>
					>::type,
					typename mpl::at_c<Map, 1>::type::next
				>::type {};

			using S2 = typename mpl::fold<
				S,
				mpl::list2<mpl::map0<>, mpl::int_<0>>,
				mpl::if_<
					HasKey<mpl::_1, mpl::_2>,
					Insert<mpl::_1, mpl::_2>,
					InsertAndUpdate<mpl::_1, mpl::_2>
				>
			>::type;

			using G2 = typename mpl::fold<
				G,
				S2,
				mpl::if_<
					HasKey<mpl::_1, mpl::_2>,
					Insert<mpl::_1, mpl::_2>,
					InsertAndUpdate<mpl::_1, mpl::_2>
				>
			>::type;

			using R2 = typename mpl::fold<
				R,
				G2,
				mpl::if_<
					HasKey<mpl::_1, mpl::_2>,
					Insert<mpl::_1, mpl::_2>,
					InsertAndUpdate<mpl::_1, mpl::_2>
				>
			>::type;

			using type = typename mpl::at_c<R2, 0>::type;
			using NumRows = typename mpl::at_c<R2, 1>::type;

			using table = std::array<
				std::array<
					std::pair<Operation, int>,
					NumTerminals::value + NumNonterminals::value
				>,
				NumRows::value
			>;
		};

		//P is the original starting nonterminal, before we argument the grammar
		template<typename NumTerminals, typename NumNonTerminals, typename First, typename Nullable, typename P, typename... Ps>
		struct ConstructTable {			
			using StartItem = ItemT<
				typename P::Left::type,
				typename P::Right::type,
				mpl::int_<0>,
				Terminal<-1>
			>;
			
			using T2 = typename Closure<First, Nullable, mpl::set1<StartItem>, P, Ps...>::type;
			using Table = typename detail::Fixpoint<
				ConstructTableLoop,
				mpl::list2<mpl::set1<T2>, mpl::set0<>>,
				detail::binary_mpl_equal,
				First, Nullable, P, Ps...
			>::type;

			using E = typename mpl::partition<
				typename mpl::at_c<Table, 1>::type,
				detail::IsShift<mpl::_1>,
				mpl::inserter<mpl::set0<>, mpl::insert<mpl::_1, mpl::_2>>,
				mpl::inserter<mpl::set0<>, mpl::insert<mpl::_1, mpl::_2>>
			>::type;
			using S = typename E::first;
			using G = typename E::second;
			using R = typename ConstructReduces<
				typename mpl::at_c<Table, 0>::type
			>::type;

			using result = typename detail::CreateIndexableTable<NumTerminals, NumNonTerminals, S, G, R>;
			using type = typename result::type;
			using table = typename result::table;
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

	template<typename Op, typename ProductionMap, typename RowMap>
	struct ValueForOperation {
		static const detail::Operation tag = detail::Operation::INVALID;
		static const int value = 0;
	};

	template<typename From, typename Left, typename Right, typename Lookahead, typename ProductionMap, typename RowMap>
	struct ValueForOperation<detail::ReduceT<From, Left, Right, Lookahead>, ProductionMap, RowMap> {
		static const detail::Operation tag = detail::Operation::REDUCE;

		static const int value = mpl::at<
			ProductionMap,
			mpl::pair<Left, Right>
		>::type::value;
	};

	template<typename From, typename To, typename Lookahead, typename ProductionMap, typename RowMap>
	struct ValueForOperation<detail::ShiftT<From, To, Lookahead>, ProductionMap, RowMap> {
		static const detail::Operation tag = detail::Operation::SHIFT;
		static const int value = mpl::at<
			RowMap,
			To
		>::type::value;
	};

	template<typename From, typename To, typename Lookahead, typename ProductionMap, typename RowMap>
	struct ValueForOperation<detail::GotoT<From, To, Lookahead>, ProductionMap, RowMap> {
		static const detail::Operation tag = detail::Operation::GOTO;
		static const int value = mpl::at<
			RowMap,
			To
		>::type::value;
	};

	template<typename typename From, typename ProductionMap, typename RowMap>
	struct ValueForOperation<detail::AcceptT<From>, ProductionMap, RowMap> {
		static const detail::Operation tag = detail::Operation::ACCEPT;
		static const int value = 0;
	};

	template<typename P_, typename... Ps_>
	struct LrParser : detail::LrParserHelper<P_, Ps_...> {
	private:
		template<typename P, typename... Ps>
		struct ConsFollow {
			using type = typename mpl::insert<
			typename ConsFollow<Ps...>::type,
			mpl::pair<
			typename P::Left::type,
			mpl::set0<>
			>
			>::type;
		};

		template<typename P>
		struct ConsFollow<P> {
			using type = mpl::map1<
				mpl::pair<
					typename P::Left::type,
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
			typename P::Right::type
			>::type;
		};

		template<typename P>
		struct ConsFirst<P> {
			using type = typename AddTerminals<
			mpl::map0<>,
			typename P::Right::type
			>::type;
		};

		//Augment the grammar by creating a production: S' -> S $
		using ParseResultType = typename P_::Left::type;
		struct ParseCompleted {
			detail::Start operator()(ParseResultType, std::string) {
				return detail::Start();
			}
		};
		using StartP = Production<
			Left<detail::Start>,
			Right<ParseResultType, Terminal<-1>>,
			ParseCompleted
		>;

		using FirstInit = typename ConsFirst<StartP, P_, Ps_...>::type;
		using FollowInit = typename ConsFollow<StartP, P_, Ps_...>::type;

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
		static const typename TableInfo::table myTable;
		static const std::array<std::pair<int, int>, 1 + sizeof...(Ps_)> productions;

		template<typename Seq, typename Prod>
		struct HasKey
			: mpl::has_key<
				typename mpl::at_c<Seq, 0>::type,
				typename Prod::Left::type
			>{};

		template<typename Seq, typename Prod>
		struct InsertLeftAndUpdate {
			using Map = typename mpl::at_c<Seq, 0>::type;
			using Value = typename mpl::at_c<Seq, 1>::type;
			using type = mpl::list2<
				typename mpl::insert<
					Map,
					mpl::pair<
						typename Prod::Left::type,
						Value
					>
				>::type,
				typename Value::next
			>;
		};

		template<typename Seq, typename Prod>
		struct InsertAndUpdate {
			using Map = typename mpl::at_c<Seq, 0>::type;
			using Value = typename mpl::at_c<Seq, 1>::type;
			using type = mpl::list2<
				typename mpl::insert<
					Map,
					mpl::pair<
						mpl::pair<
							typename Prod::Left::type,
							typename Prod::Right::type
						>,
						Value
					>
				>::type,
				typename Value::next
			>;
		};

		using RowMap = typename TableInfo::type;
		using ProductionMap = typename mpl::at_c<
			typename mpl::fold<
				typename mpl::list<P_, Ps_...>::type,
				mpl::list2<mpl::map0<>, mpl::int_<0>>,
				InsertAndUpdate<mpl::_1, mpl::_2>
			>::type,
			0
		>::type;
		using S = typename TableInfo::S;
		using G = typename TableInfo::G;
		using R = typename TableInfo::R;
		using NumRows = typename TableInfo::NumRows;
		using NumCols = typename mpl::plus<NumTerminals, NumNonterminals>::type;

		using ColMap = typename mpl::at_c<
			typename mpl::fold<
				typename mpl::list<P_, Ps_...>::type,
				mpl::list2<mpl::map0<>, typename NumTerminals>,
				mpl::if_<
					HasKey<mpl::_1, mpl::_2>,
					mpl::_1,
					InsertLeftAndUpdate<mpl::_1, mpl::_2>
				>
			>::type,
			0
		>::type;


		LrParser() {
			std::ofstream out("out.txt");
			detail::map_to_string<RowMap>(out);
			out << std::endl;
			detail::map_to_string<ColMap>(out);
			out << std::endl;
			detail::map_to_string<ProductionMap>(out);
			out << std::endl;
			detail::set_to_string<S>(out);
			detail::set_to_string<G>(out);
			detail::set_to_string<R>(out);
			out.close();
		}

		template<typename It>
		static ParseResultType parse(It begin, It end) {
			std::stack<int> stack;
			auto state = 0;
			stack.push(0);
			auto symbol = *begin++;
			auto entry = myTable[state][symbol+1];
			while (entry.first != detail::Operation::ACCEPT) {
				if (entry.first == detail::Operation::SHIFT) {
					stack.push(symbol);
					state = entry.second;
					stack.push(static_cast<int>(state));
					symbol = *begin++;
				}
				else if (entry.first == detail::Operation::REDUCE) {
					auto prod = productions.at(entry.second);
					for (int i = 0; i < 2*prod.second; ++i) {
						stack.pop();
					}
					state = stack.top();
					stack.push(prod.first);
					state = myTable[state][prod.first].second;
					stack.push(state);
				}
				else if (entry.first == detail::Operation::INVALID) {
					throw std::exception();
				}
				entry = myTable[state][symbol+1];
			}
			return ParseResultType();
		}

		private:

		//T is ReduceT, GotoT or ShiftT
		template<typename T, typename Row>
		struct FromEquals {
			using type = typename mpl::equal_to<
				typename mpl::at<RowMap, typename T::From>::type,
				Row
			>::type;
		};

		//T is ReduceT, GotoT or ShiftT
		template<typename T, typename Col>
		struct LookaheadEquals {

			template<typename U>
			struct NontermLookaheadEquals {
				using type = typename mpl::equal_to<
					typename mpl::at<ColMap, typename U::Lookahead>::type,
					Col
				>::type;
			};
			
			template<typename U>
			struct TermLookaheadEquals {
				using type = typename mpl::equal_to<
					typename mpl::int_<U::Lookahead::value>::next,
					Col
				>::type;
			};

			using type = typename mpl::if_<
				detail::IsTerminal<mpl::identity<typename T::Lookahead>>,
				TermLookaheadEquals<T>,
				NontermLookaheadEquals<T>
			>::type;
		};

		template<typename Row, typename Col, typename Map>
		struct FindIf {
			using type = typename mpl::find_if<
				Map,
				mpl::and_<
					FromEquals<mpl::_1, Row>,
					LookaheadEquals<mpl::_1, Col>
				>
		    >::type;
		};

		template<typename Row, typename Col>
		const static std::pair<detail::Operation, int> FillElement() {
			using T1 = typename FindIf<Row, Col, R>::type;
			using T2 = typename mpl::eval_if<
				boost::is_same<
					T1,
					typename mpl::end<R>::type
				>,
				FindIf<Row, Col, G>,
				mpl::identity<T1>
			>::type;

			using T3 = typename mpl::eval_if<
				boost::is_same<
					T2,
					typename mpl::end<G>::type
				>,
				FindIf<Row, Col, S>,
				mpl::identity<T2>
			>::type;
            using T = typename mpl::eval_if<
                boost::is_same<
    				T3,
					typename mpl::end<S>::type
				>,
                mpl::na,
                mpl::deref<T3>
            >::type;

			using U = ValueForOperation<T, ProductionMap, RowMap>;
			return { U::tag, U::value };
		}

		template<std::size_t row, std::size_t... Cols>
		const static typename std::array<std::pair<detail::Operation, int>, NumCols::value> FillInner(detail::indices<Cols...>) {
			return{ { FillElement<mpl::int_<row>, mpl::int_<Cols>>()... } };
		}
		
		template<std::size_t... Rows>
		const static typename TableInfo::table FillOuter(detail::indices<Rows...>) {
			return{ { FillInner<Rows>(detail::build_indices<NumCols::value>())... } };
		}

		const static typename TableInfo::table FillTable() {
			return FillOuter(detail::build_indices<NumRows::value>());
		}

		template<std::size_t Index>
		const static std::pair<int, int> FillProductionMapInner() {
			using P = typename detail::nth<Index, P_, Ps_...>::type;

			return {
				mpl::at<
					ColMap,
					typename P::Left::type
				>::type::value,
				mpl::size<typename P::Right::type>::type::value
			};
		}

		template<std::size_t... Is>
		const static std::array<std::pair<int, int>, 1 + sizeof...(Ps_)> FillProductionMapOuter(detail::indices<Is...>) {
			return { FillProductionMapInner<Is>()... };
		}

		const static std::array<std::pair<int, int>, 1 + sizeof...(Ps_)> FillProductionMap() {
			return FillProductionMapOuter(detail::build_indices<1 + sizeof...(Ps_)>());
		}
	};
	
	template<typename P, typename... Ps>
	const typename LrParser<P, Ps...>::TableInfo::table LrParser<P, Ps...>::myTable =
		LrParser<P, Ps...>::FillTable();
	
	template<typename P, typename... Ps>
	const std::array<std::pair<int, int>, 1 + sizeof...(Ps)> LrParser<P, Ps...>::productions = FillProductionMap();
}



#endif // !_PARSE_GRAMMAR_H