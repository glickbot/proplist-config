-module(test1).

-export([do/0]).

do() ->
	{ok, Bin} = file:read_file("app.config"),
	Str = binary_to_list(Bin),
	{ok, Strings, _} = erl_scan:string(Str,1, [return, text]),
	Output = reassemble(Strings),
	file:write_file("app.config.new", Output).

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