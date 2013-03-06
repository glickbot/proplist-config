-module(test1).

-compile(export_all).

-export([appconfig/0]).
-export([get_strings/1, get_strings_until/2]).
-export([get_strings_until/3, get_strings_until/4, mod/3, mod/5]).

modify(Name, Value) ->
	Loc = name_to_loc(Name),
	ExLoc = expand_loc(Loc),
	%% check to make sure value exists
	%% load proplist, validate value, etc
	Strings = get_strings(appconfig()),
	ValStrings = val_to_strings(Value),
	NewStrings = mod(ExLoc, ValStrings, Strings),
	Output = reassemble(NewStrings),
	file:write_file("var/app.config.new", Output).

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

get_strings_until(Match, Stack, [ { '{', _ } = H | Data ], Acc ) ->
	get_strings_until(Match, Stack++['}'], Data, Acc++[H]);
get_strings_until(Match, Stack, [ { '[', _ } = H | Data ], Acc ) ->
	get_strings_until(Match, Stack++[']'], Data, Acc++[H] );
get_strings_until(Match, [ Item | Stack ], [ { Item, _ } = H | Data ], Acc ) ->
	get_strings_until(Match, Stack, Data, Acc++[H] );

get_strings_until(Match, Stack, [H | Data], Acc) ->
	get_strings_until(Match, Stack, Data, Acc++[H]).


%%%NOTES
% get block in tandem with proplist
% when atom matches unused proplist branch ->
%     return text output of block ( first Stack entry until Stack empty )
% when atom matches used proplist branch from Loc ->
% enter 'get block in tandem with proplist' to retrieve value/replacement

%%% get_value_with_comments

% add new item:
%  builds which item the new item should be added to via proplists
%  once that item is itterated over, acts on closing, appends new item
% mod item:
%  validates item via proplists
%  once item is itterated over, acts on clusing, replacing content

% Loc = [ riak_api, pb_port ]
% load Strings
% load PropList
% Stack = []
% 


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
mod(Loc, Val, [], Stack, _) -> 
	% got to the end of data without finding the loc,
	% which should have been verified via proplists,
	% so this is bad
	io:format("Error, Data left in: Loc: ~p~n Val: ~p~n Stack: ~p~n ", [Loc, Val, Stack]);
mod([], [], [ H | Data], Stack, Acc ) ->
	% Loc & Val are empty, so you've found everything.. so just
	% finish by adding the rest
	mod([], [], Data, Stack, Acc++[H]);
mod(Loc, Val, [ {white_space, _, _ } = H | Data ], Stack, Acc) ->
	mod(Loc, Val, Data, Stack, Acc++[H]);
mod(Loc, Val, [ { comment, _, _ } = H | Data ], Stack, Acc) ->
	mod(Loc, Val, Data, Stack, Acc++[H]);

mod(Loc, Val, [ { Item, _, _ } = H | Data ], [ Item | Stack ], Acc ) ->
	mod(Loc, Val, Data, Stack, Acc++[H]);
mod(Loc, Val, [ { Item, _ } = H | Data ], [ Item | Stack ], Acc ) ->
	mod(Loc, Val, Data, Stack, Acc++[H]);

mod([ Item | Loc ], Val, [ { Item, _, _ } = H | Data], [], Acc ) ->
	mod(Loc, Val, Data, [], Acc++[H]);
mod([ Item | Loc ], Val, [ { Item, _ } = H | Data], [], Acc ) ->
	mod(Loc, Val, Data, [], Acc++[H]);
mod([ Item | Loc ], Val, [ { _, _, Item } = H | Data], [], Acc ) ->
	mod(Loc, Val, Data, [], Acc++[H]);

mod([], Val, [ _H | Data ], [], Acc ) ->
	case get_strings_until('}', Data) of
		{ok, Match, NewData, _ } -> 
			%io:format("")
			mod([], [], NewData, [], Acc ++ Val ++ [Match]);
		Anything ->
			io:format("~p", [Anything])
	end;

mod(Loc, Val, [ H | Data ], Stack, Acc ) ->
	mod(Loc, Val, Data, Stack, Acc ++ [H] ).



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

% dump() ->
% 	Strings = get_strings(appconfig()),
% 	Output = io_lib:format("~p", [Strings]),
% 	file:write_file("var/dump.erl", Output).

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