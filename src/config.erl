-module(config).

-compile(export_all).


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
run({Options, [Values]}) ->
    Target = proplists:get_value(target, Options),
    File = proplists:get_value(file, Options),
    case proplists:get_value(command, Options) of
        "set" ->
            io:format("Calling change: ~p, ~p, ~p\n", [Target, Values, File]),
            Result = proplists_mod:change(Target, Values, File),
            io:format("Result: ~p\n", [Result]),
            ok
    end;

run([Name | List]) ->
    try
        Tuple = erlang:list_to_tuple(List),
        io:format("Tuple: ~p", [Tuple]),
        Value = io_lib:format("~p", [Tuple]),
        Result = proplists_mod:change(Name, Value),
        io:format("Result: ~p\n", [Result])
    catch
        %%_:_ ->
        %%    help()
        Error:Reason ->
            io:format("Error:[~p]: ~p\n", [Error, Reason]),
            io:format("Stack: ~p\n", [erlang:get_stacktrace()])
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
