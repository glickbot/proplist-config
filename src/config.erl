-module(config).

-compile(export_all).

-define(DEFAULT_FILE, "var/app.conifg").
%% TODO:
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
        { target,  undefined, undefined, undefined, "Target ( i.e. riak_api.pb_port )"},
        { file, $f, "file", {string, ?DEFAULT_FILE}, "Proplist file ( i.e. app.config )"}
    ].

main(RawArgs) ->
    Args = parse_args(RawArgs),
    %%io:format("Args:~p\n", [Args]).
    case catch(process_args(Args)) of
        ok ->
            ok;
        Error ->
            io:format("ERROR: Uncaught error in config: ~p\n", [Error]),
            io:format("ERROR: With Args: ~p\n", [Args])
    end.

help() ->
    getopt:usage(opt_spec_list(), "config"),
    halt(1).

%=============== COMBINE parse_args and process_args =====================%

parse_args(RawArgs) ->
    OptSpecList = opt_spec_list(),
    case getopt:parse(OptSpecList, RawArgs) of
        {ok, Args} ->
            Args;
        {error, {Reason, Data}} ->
            io:format("~p: ~p\n", [Reason, Data]),
            help()
    end.

process_args({[{command, "help"} | _ ], _ }) ->
    help();
process_args({Options, Values}) ->
    Target = proplists:get_value(target, Options),
    Loc = target_to_proploc(Target),
    File = proplists:get_value(file, Options),
    Values = tokenize_values(Values),
    Content = get_file_content(File),
    Proplist = get_proplist(Content),
    Command = case proplists:get_value(command, Options) of
        "set" -> set;
        "add" -> add;
        "del" -> del;
        "test" -> test;
        Other ->
            io:format("Unknown command: ~p\n", [Other]),
            help(),
            halt(1)
    end,
    case run_command({Command, Loc, Values, Content, Proplist}) of
        { ok } -> ok;
        
        { save, Output } ->
            file:write_file(File++".new", Output),
            ok;
        { error, Error } ->
            io:format("ERROR: ~s\n", [Error]),
            { error, Error }
    end;
process_args(Args) ->
    io:format("Invalid arguments: ~p\n", [Args]),
    help().



run_command({set, Loc, Value, Content, _Proplist}) ->
    Action = { set, Value },
    %% for "set", our location is exactly the item we wish to modify
    Output = proplists_mod:modify(Loc, Action, Content),
    { save, Output };

run_command({add, Loc, Values, Content, _Proplist}) ->
    %% for "add", we're actually modifying the parent item of target
    { ParentItem, ItemToAdd } = pop(Loc),
    Action = { add, ItemToAdd, Values },
    Output = proplists_mod:modify(ParentItem, Action, Content),
    { save, Output };

run_command({del, Loc, _Values, Content, _Proplist}) ->
    %% for "delete", we're also modifying the parent of the target
    { ParentItem, ItemToDel } = pop(Loc),
    Action = { del, ItemToDel },
    Output = proplists_mod:modify(ParentItem, Action, Content),
    { save, Output };

run_command({test, _Loc, _Values, _Content, _Proplist}) ->
    { ok }.
% run_command(Unknown) ->
%     io:format("ERROR: Unhandled run_command: ~p\n", [Unknown]),
%     { error, Unknown }.

target_to_proploc(undefined) -> [];
target_to_proploc(Target) ->
    List = string:tokens(Target, "."),
    [list_to_atom(X) || X <- List].

tokenize_values(undefined) -> [];
tokenize_values([]) -> [];
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
    proplists_mod:prepare_value(Value).

get_file_content(File) ->
%get_tokens_and_proplist(File) ->
    Binary = try file:read_file(File) of
        { ok, B } -> B
    catch
        ReadType:ReadError ->
            io:format("Caught ~p: ~p\n", [ ReadType, ReadError ])
    end,
    binary_to_list(Binary).

get_proplist(Content) ->
    try
        %{ ok, Tokens, _ } = erl_scan:string(S,1,[return,text]),
        { ok, Strings, _ } = erl_scan:string(Content),
        { ok, [Form] } = erl_parse:parse_exprs(Strings),
        {value, Proplist, _ } = erl_eval:expr(Form, []),
        Proplist
    catch
        Type:Error ->
            io:format("Caught ~p: ~p\n", [ Type, Error ])
    end.

pop(List) ->
    lists:split(length(List) - 1, List).