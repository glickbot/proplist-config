-module(config).

-compile(export_all).

%% TODO:
%%  - move file handling functions from proplists_mod.erl to config.erl
%%  - add validations via 'proplists' before sending to proplists_mod
%%  - add getters
%%  - add proper error handling
%%  - add help functions

%% ====================================================================
%% Public API
%% ====================================================================

%% escript Entry point

opt_spec_list() -> [
        { command, undefined, undefined, {string, "help"}, "Command"},
        { target,  undefined, undefined, undefined, "Target"},
        { file, $f, "file", {string, "../var/app.config"}, "app.config file"}
    ].

main(RawArgs) ->
    Args = parse_args(RawArgs),
    %%io:format("Args:~p\n", [Args]).
    case catch(run(Args)) of
        ok ->
            ok;
        Error ->
            io:format("ERROR: Uncaught error in config: ~p\n", [Error]),
            io:format("ERROR: With Args: ~p\n", [Args])
    end.

run({[{command, "help"} | _ ], _ }) ->
    help();
run({Options, Values}) ->
    Target = proplists:get_value(target, Options),
    Loc = target_to_proploc(Target),
    File = proplists:get_value(file, Options),
    Return = case proplists:get_value(command, Options) of
        "set" ->
            Value = tokenize_values(Values),
            Action = { change, Value },
            %% for "set", our location is exactly the item we wish to modify
            proplists_mod:modify(Loc, Action, File);
        "add" ->
            Value = tokenize_values(Values),
            %% for "add", we're actually modifying the parent item of target
            { NewLoc, New } = lists:split(length(Loc) - 1, Loc),
            Action = { add, New, Value },
            proplists_mod:modify(NewLoc, Action, File);
        "del" ->
            %% for "delete", we're also modifying the parent of the target
            { NewLoc, Del } = lists:split(length(Loc) - 1, Loc),
            Action = { del, Del },
            proplists_mod:modify(NewLoc, Action, File);
        Other ->
            io:format("ERROR: Unhandled command: ~s\n", [Other]),
            help(),
            halt(1)
    end,
    case Return of
        { ok, Output } ->
            file:write_file(File++".new", Output);
        { error, Error } ->
            io:format("ERROR: ~s\n", [Error])
    end;
run(Args) ->
    io:format("Invalid arguments: ~p\n", [Args]),
    help().

parse_args(RawArgs) ->
    OptSpecList = opt_spec_list(),
    case getopt:parse(OptSpecList, RawArgs) of
        {ok, Args} ->
            Args;
        {error, {Reason, Data}} ->
            io:format("~p: ~p\n", [Reason, Data]),
            help()
    end.

help() ->
    io:format("help: named.tuple value\n"),
    halt(1).

default_file() ->
	"../var/appconfig".

target_to_proploc(Target) ->
    List = string:tokens(Target, "."),
    [list_to_atom(X) || X <- List].

%% A list of one string item
tokenize_values([Value]) when is_list(Value) ->
    tokenize_value(Value);
%% A list which *is* a string
tokenize_values([Value]) ->
    tokenize_value([Value]);
%% A list of strings
tokenize_values(Values) when length(Values) > 1 ->
    Value = "{"++strings:join(Values, ",")++"}",
    tokenize_value(Value).

tokenize_value(Value) ->
    {ok, Tokens, _} = erl_scan:string(Value,1,[return,text]),
    Tokens.

%%read_with_consult(File) -> 
%%        file:consult(File). 

%%read_with_eval(File) -> 
%%        {ok, B} = file:read_file(File), 
%%        S = binary_to_list(B), 
%%        {ok, Tokens, _} = erl_scan:string(S), 
%%        {ok, [Form]} = erl_parse:parse_exprs(Tokens), 
%%        erl_eval:expr(Form, []). 