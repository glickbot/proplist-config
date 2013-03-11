-module(test1).

-compile(export_all).

%% Just setters right now ( hard part? )
%		- going to use proplist functions for getters/validation/tests

%% expects ./var/app.config to exist ( see appconfig() )
% 		- lame way of doing it, I figure depending on the level
%		- of configuration needed, this module might have it's own
%		- proplist... ( api syntax options, etc )

%% add, change, del commands expect cli-like arguments
%		"period.seperated.names.of.tuples" for the Name
%		"{parsable, [{erlang, "Code"}], 45}", or "54", etc for values

% the goal is to have the CLI module figure out what action the user
% wants to perform based on input, I.E.:
%   # riak_api.pb_port
%   >	10017	( return via "get" )
%   # riak_api.pb_port 10018
%   >   ok		( change via "change" )
%	$ riak_core.http 127.0.0.1 10019
%   >   ok      ( make tuple {"127.0.0.1, 10019"}, add to http)
%   ( won't work now because "127.0.0.1" isn't an atom, need to add
%      funcionality to add un-named tuple values
%			maybe via {add, no-name, [Values]}


change(Name, Value) ->
	change(Name, Value, appconfig()).

change(Name, Value, File) ->
	Target = expand_loc(name_to_loc(Name)),
	ValStrings = val_to_strings(Value),
	Action = { change, ValStrings },
	modify(Target, Action, File).

add(Name) ->
	add(Name, "[]", appconfig()).

add(Name, Value) ->
	add(Name, Value, appconfig()).

add(Name, Value, File) ->
	Loc = name_to_loc(Name),
	{ NewLoc, New } = lists:split(length(Loc) - 1, Loc),
	Target = expand_loc(NewLoc),
	ValStrings = val_to_strings(Value),
	Action = { add, New, ValStrings },
	modify(Target, Action, File).

del(Name) ->
	del(Name, appconfig()).

del(Name, File) ->
	Loc = name_to_loc(Name),
	{ NewLoc, Del } = lists:split(length(Loc) - 1, Loc),
	Target = expand_loc(NewLoc),
	Action = { del, Del },
	modify(Target, Action, File).

%comment() -> %%TODO
%uncomment() -> %%TODO
% note, automating the commenting and uncommenting of content could
% cause proplist files to grow unchecked
% maybe this should only act on specific comments, i.e. %~% or something

modify(Target, Action, File) ->
	Data = get_strings(File),
	NewData = mod(Target, Action, Data),
	Output = reassemble(NewData),
	file:write_file(File++".new", Output).


%NOTE, for lack of better terminology:
%	"strings" are the tuples returned from erl_scan:string(Value,1,[return,text])

% Getters and validations will be added using proplist functions



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

% get("riak_api.db_port").
% get(Name) ->
% 	Loc = name_to_loc(Name),
% 	%[riak_api, db_port]
% 	{ok, [Prop]} = file:consult(appconfig()),
% 	get_proplist_loc_value(Loc, Prop).

name_to_loc(Name) ->
	List = string:tokens(Name, "."),
	[list_to_atom(X) || X <- List].

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
		{change, Value} ->
			mod([], [], DataTuple, [], Acc ++ Value ++ [EndTuple]);
		%{add, Name, Value} ->

		{del, [Name]} ->
			% grab *just* the contents of the next list ( proplist )
			% send that to get_until_named_tuple, retrieveing the needed tuple contents
			% don't append tuple contents of tuple which needs to be deleted
			io:format("TupleContent:~n~p~n", [TupleContent]),
			dump(TupleContent),
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

appconfig() -> "var/app.config".

get_strings(File) ->
	{ok, Bin} = file:read_file(File),
	Content = binary_to_list(Bin),
	{ok, Strings, _} = erl_scan:string(Content,1,[return,text]),
	Strings.

val_to_strings(Value) ->
	{ok, Strings, _} = erl_scan:string(Value,1,[return,text]),
	Strings.

text_to_term(Text) ->
	{ok, Tokens, _} = erl_scan:string(Text, 1, [return, text]),
	{ok, Term} = erl_parse:parse_term(Tokens++"."),
	Term.

dump(Strings) ->
 	%Strings = get_strings(appconfig()),
 	Output = io_lib:format("~p", [Strings]),
 	file:write_file("var/dump.erl", Output).

% test() ->
% 	Strings = get_strings(appconfig()),
% 	Output = reassemble(Strings),
% 	file:write_file("var/app.config.new", Output).

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