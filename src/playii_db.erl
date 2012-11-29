-module(playii_db).

%% API
-export([init/0]). %% init mnesia tables
-export([sync/0]). %% init mnesia nodes' connect
-export([dump/1, clear/1]).
-export([load/1, save/1, delete/1]). 
-export([batch_delete/1]). %% data batch functions

-include_lib("stdlib/include/qlc.hrl").
-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test() ->
    sync(),
    Key = "p_test_1234",
    Val1 = {test1, value1},
    Val2 = {test2, value2},
    save(#backup{key=Key, value=Val1}),
    dump(backup),
    R0 = load({backup, Key}),
    ?debugVal(R0),
    ?assert(Val1 =:= R0#backup.value),
    save(#backup{key=Key, value=Val2}),
    R1 = load({backup, Key}),
    ?debugVal(R1),
    ?assert(Val2 =:= R1#backup.value),
    delete({backup, Key}),
    R2 = load({backup, Key}),
    ?debugVal(R2),
    ?assert(undefined =:= R2),
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

%%----for mnesia init-------------------------------------------------

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic, ok} = mnesia:create_table(
		     backup,
		     [{ram_copies, [node()]},
		      {attributes, record_info(fields, backup)}]),
    {atomic, ok} = mnesia:create_table(
		     config,
		     [{disc_copies, [node()]},
		      {attributes, record_info(fields, config)}]),
    {atomic, ok} = mnesia:create_table(
		     log,
		     [{disc_copies, [node()]},
		      {attributes, record_info(fields, log)}]),
    ok.

%%--------------------------------------------------------------------

sync() ->
    case nodes() of
	[] ->  ok;
	Nodes -> mnesia:change_config(extra_db_nodes, Nodes)
    end,
    ok = mnesia:wait_for_tables([config, backup, log], 5000).

%%--------------------------------------------------------------------

clear(Table) ->
    mnesia:clear_table(Table).

%%--------------------------------------------------------------------

dump(Table) ->
    Pattern = mnesia:table_info(Table, wild_pattern),
    F = fun() ->
		mnesia:match_object(Pattern)
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%%--------------------------------------------------------------------

load({Table, Key}) ->
    ?debugFmt("load(~p,~p)", [Table, Key]),
    F = fun() ->
		mnesia:read({Table, Key})
	end,
    {atomic, Val} = mnesia:transaction(F),
    case Val of
        [] -> undefined;
        [V] -> V
    end.
    %% case catch mnesia:dirty_read(Table, Key) of
    %%     {'EXIT', _} -> undefined;
    %%     [] -> undefined;
    %%     [R] -> R
    %% end.
    %% select max from config where node = Node and service = Service
    %% #config{max=Max} = mnesia:dirty_read(config, {Node, Service}),
    %% L = mnesia:dirty_select(config, [{#config{max='$1', oid={Node,Service}, _='_'}, [], ['$1']}]),
    %% case L of
    %%[] -> 0;
    %%[M] -> M
    %% end.

%%--------------------------------------------------------------------

save(List) when is_list(List) ->
    ?debugFmt("save(~p)", [List]),
    F = fun() ->
		[ mnesia:write(C) || C <- List ]
	end,
    {atomic, _} = mnesia:transaction(F);
save(Record) when is_tuple(Record) ->
    ?debugFmt("save(~p)", [Record]),
    F = fun() ->
		mnesia:write(Record)
	end,
    {atomic, _} = mnesia:transaction(F).

%%--------------------------------------------------------------------

delete(List) when is_list(List) ->
    ?debugFmt("delet(~p)", [List]),
    F = fun() ->
		[ mnesia:delete(C) || C <- List ]
	end,
    {atomic, _} = mnesia:transaction(F);
delete({Table, Key}) ->
    ?debugFmt("delet({~p,~p})", [Table, Key]),
    F = fun() ->
		mnesia:delete({Table, Key})
	end,
    {atomic, _} = mnesia:transaction(F).

batch_delete({Table, Fun}) ->
    D = fun(Record, Acc) ->
		case Fun(Record) of
		    {true, Key} ->
			mnesia:delete({Table, Key}),
			Acc++[Key];
		    _ ->
			Acc
		end
	end,
    F = fun() ->
		mnesia:foldl(D, [], Table)
	end,
    {atomic, _} = mnesia:transaction(F).

%%--------------------------------------------------------------------

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:trasaction(F),
    Val.

%%--------------------------------------------------------------------

