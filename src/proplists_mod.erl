-module(proplists_mod).

-compile(export_all).

modify(Loc, Action, Content) ->
	Data = get_tokens(Content),
	Target = expand_loc(Loc),
	NewData = mod(Target, Action, Data),
	Output = reassemble(NewData),
	{ ok, Output }.

prepare_value(Value) ->
	get_tokens(Value).
%NOTE, for lack of better terminology:
%	"strings" are the tuples returned from erl_scan:string(Value,1,[return,text])

% get_strings_until streams through 'Data' to find the first 'Match'
% is found in the current... scope?... as it igores matches within balanced
% brackets {} or [].
% it returns the 'Match' string, then depleated 'Data', and all
% the accumulated "strings" between (Acc)

% NOTE: get_strings_until ignores strings in balanced {} or [],

get_strings_until(Match, Data) ->
	get_strings_until(Match, Data, []).

get_strings_until(Match, Data, Acc) ->
	get_strings_until(Match, [], Data, Acc).

get_strings_until(Match, [], [ {_, _, Match} = H | Data ], Acc ) ->
	{ok, H, Data, Acc};
get_strings_until(Match, [], [ {Match, _, _} = H | Data ], Acc ) ->
	{ok, H, Data, Acc};
get_strings_until(Match, [], [ {Match, _} = H | Data ], Acc ) ->
	{ok, H, Data, Acc};
get_strings_until(_, _Stack, [], Acc) ->
	%io:format("Stack:~p~n", [Stack]),
	{error, not_found, [], Acc };

get_strings_until(Match, Stack, [ { '{', _ } = H | Data ], Acc ) ->
	get_strings_until(Match, ['}']++Stack, Data, Acc++[H]);
get_strings_until(Match, Stack, [ { '[', _ } = H | Data ], Acc ) ->
	get_strings_until(Match, [']']++Stack, Data, Acc++[H] );
get_strings_until(Match, [ Item | Stack ], [ { Item, _ } = H | Data ], Acc ) ->
	get_strings_until(Match, Stack, Data, Acc++[H] );

get_strings_until(Match, Stack, [H | Data], Acc) ->
	get_strings_until(Match, Stack, Data, Acc++[H]).

expand_loc(Loc) ->
	expand_loc(Loc, []).

expand_loc([], Acc) ->
	lists:reverse(Acc);
expand_loc([H | Loc], Acc) ->
	expand_loc(Loc, [ ',', H, '{', '['] ++ Acc).

get_proplist_loc_value([H|[]], Prop) ->
	proplists:get_value(H, Prop);
get_proplist_loc_value([H|Loc], Prop) ->
	SubProp = proplists:get_value(H, Prop),
	get_proplist_loc_value(Loc, SubProp).

mod(Loc, Value, Data) ->
	mod(Loc, Value, Data, [], []).

mod([], [], [], [], Acc) -> Acc;
mod(Loc, Action, [], Stack, _) -> 
	% got to the end of data without finding the loc,
	% which should have been verified via proplists,
	% so this is bad
	io:format("Error, Data left in: Loc: ~p~n Action: ~p~n Stack: ~p~n ", [Loc, Action, Stack]);
mod([], [], [ H | Data], Stack, Acc ) ->
	% Loc & Val are empty, so you've found everything.. so just
	% finish by adding the rest
	mod([], [], Data, Stack, Acc++[H]);
mod(Loc, Action, [ {white_space, _, _ } = H | Data ], Stack, Acc) ->
	mod(Loc, Action, Data, Stack, Acc++[H]);
mod(Loc, Action, [ { comment, _, _ } = H | Data ], Stack, Acc) ->
	mod(Loc, Action, Data, Stack, Acc++[H]);

mod(Loc, Action, [ { Item, _, _ } = H | Data ], [ Item | Stack ], Acc ) ->
	mod(Loc, Action, Data, Stack, Acc++[H]);
mod(Loc, Action, [ { Item, _ } = H | Data ], [ Item | Stack ], Acc ) ->
	mod(Loc, Action, Data, Stack, Acc++[H]);

mod([ Item | Loc ], Action, [ { Item, _, _ } = H | Data], [], Acc ) ->
	mod(Loc, Action, Data, [], Acc++[H]);
mod([ Item | Loc ], Action, [ { Item, _ } = H | Data], [], Acc ) ->
	mod(Loc, Action, Data, [], Acc++[H]);
mod([ Item | Loc ], Action, [ { _, _, Item } = H | Data], [], Acc ) ->
	mod(Loc, Action, Data, [], Acc++[H]);

mod([], Action, Data, [], Acc ) ->
	{ok, EndTuple, DataTuple, TupleContent} = get_strings_until('}', Data),
	case Action of
		{set, Value} ->
			mod([], [], DataTuple, [], Acc ++ Value ++ [EndTuple]);
		%{add, Name, Value} ->

		{del, [Name]} ->
			% grab *just* the contents of the next list ( proplist )
			% send that to get_until_named_tuple, retrieveing the needed tuple contents
			% don't append tuple contents of tuple which needs to be deleted
			case get_strings_until('[', TupleContent, Acc) of
				{ok, StartList, DataList, AccList } ->
					{ok, EndList, _DataList2, ListContent } = get_strings_until(']', DataList),
					{ok, _TupleTokens, ListContentLeft, Acc3 } = get_until_named_tuple(Name, ListContent, AccList++[StartList]),
					mod([], [], DataTuple, [], Acc3++ListContentLeft++[EndList]);
				{error, not_found, _, _} ->
					{error, tuple_not_found, Name, TupleContent}
			end;
			%% handle comma
			%% Return NewData, NewAcc
		{add, [Name], Value} ->
			case get_strings_until('[', TupleContent, Acc) of
				{ok, StartList, DataList, AccList} ->
					case get_strings_until('{', DataList) of
						{ok, _, _, _} ->
							mod([], [], DataTuple, [], AccList++[StartList]++new_tuple_string(Name, Value)++[symbol_string(',')]++DataList++[EndTuple]);
						{error, not_found, _, _} ->
							mod([], [], DataTuple, [], AccList++[StartList]++new_tuple_string(Name, Value)++DataList++[EndTuple])
					end;
				{error, not_found, _, _} ->
					{error, not_list_of_tuples}
			end
	end;

mod(Loc, Action, [ H | Data ], Stack, Acc ) ->
	mod(Loc, Action, Data, Stack, Acc ++ [H] ).

symbol_string(Symbol) when is_atom(Symbol) ->
	Text = atom_to_list(Symbol),
	{Symbol, [{line,0},{text,Text}]}.

atom_string(Atom) when is_atom(Atom) ->
	Text = atom_to_list(Atom),
	{atom, [{line,0}, {text, Text}], Atom}.

new_tuple_string(Name, ValueStrings) ->
	[ symbol_string('{'), atom_string(Name), symbol_string(',') ] ++ ValueStrings ++ [symbol_string('}')].

newline() ->
	{white_space,[{line,0},{text,"\n"}],"\n"}.

get_until_named_tuple(Name, Data) ->
	get_until_named_tuple(Name, Data, []).

get_until_named_tuple(Name, [], _) ->
	{ error, "Unable to find tuple: ~p~n", [Name]};
get_until_named_tuple(Name, Data, Acc) ->
	{ok, TupleStart, Data2, Acc2} = get_strings_until('{', Data),
	{ok, Atom, Data3, Acc3} = get_strings_until(atom, Data2, Acc2++[TupleStart]),
	{ok, TupleEnd, Data4, Acc4} = get_strings_until('}', Data3, Acc3++[Atom]),
	case Atom of
		{ atom, _, Name } -> 
			{ ok, Acc4++[TupleEnd], Data4, Acc };
		{ atom, _, _ } -> get_until_named_tuple(Name, Data4, Acc++Acc4++[TupleEnd])
	end.

get_strings_text({_,[_,{text,Text}],_}) ->
	Text;
get_strings_text({_,[_,{text,Text}]}) ->
	Text.

text_to_term(Text) ->
	{ok, Tokens, _} = erl_scan:string(Text, 1, [return, text]),
	{ok, Term} = erl_parse:parse_term(Tokens++"."),
	Term.

reassemble(Strings) ->
	reassemble(Strings, []).

reassemble([], Result) ->
	Result;
reassemble([{_,[_,{text,Text}],_} | Rest ], Acc) ->
	reassemble(Rest, Acc++Text);
reassemble([{_,[_,{text,Text}]} | Rest ], Acc) ->
	reassemble(Rest, Acc++Text);
reassemble( [ H | Rest ], Acc ) ->
	io:format("Error: ~p~n", [H]),
	reassemble(Rest, Acc).

get_tokens(Content) ->
	{ ok, Tokens, _ } = erl_scan:string(Content,1,[return,text]),
	Tokens.