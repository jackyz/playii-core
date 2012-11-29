%%%-------------------------------------------------------------------
%%% File    : playii_config.erl
%%% Author  : jacky zhao <>
%%% Description : config via database
%%% Created : 10 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------
-module(playii_config).

%% API
-export([get_val/3]).
-export([identify/0]).
%% -export([verify/0]). %% test only
%% config
%% -export([dump/0, add_node/2, del_node/1]).

-include("playii.hrl").
-include_lib("public_key/include/public_key.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test() ->
    P1 = get_val(node(), 'player', false),
    ?assert(P1 =:= true),
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

%% get config value
get_val(Node, Key, Default)
  when Key =:= 'player.max' ->
    case verify() of
	true -> try_load(Node, Key, Default);
	_ -> ?EvalMax
    end;
get_val(Node, Key, Default) ->
    try_load(Node, Key, Default).

try_load(Node, Key, Default) ->
    case load(Node, Key) of
	undefined -> Default;
	Any -> parse_val(Any)
    end.

load(Node, Key) ->
    %% db_load(Node, Key)
    fs_load(Node, Key).

identify() ->
    SP = identify(?PublicKey),
    SFN = "license.pub",
    case file:write_file(SFN, SP) of
	ok ->
	    io:format("Identify File Generated: ~p.~n", [SFN]),
	    io:format("Using this file to reqire License.~n", []);
	{error, E} -> io:format("Identify fail: ~p (~p).~n", [E, SFN])
    end.

verify() ->
    GFN = playii_deps:local_path(["priv/license.priv"]),
    case file:read_file(GFN) of
	{ok, GP} ->
	    case verify(?PublicKey, GP) of
		true ->
		    ?debugFmt("Check Grant: true (~p).", [GFN]),
		    true;
		false ->
		    ?debugFmt("Check Grant: false (~p).", [GFN]),
		    false
	    end;
	{error, E} -> ?debugFmt("Check Grant fail: ~p (~p).", [E, GFN])
    end.

%%====================================================================
%% Internal
%%====================================================================

%% File :: Store Config info in config file / single node
%% private

fs_load(Node, Key) ->
    %% !!! only master enabled in filesystem config mode
    case string:str(atom_to_list(Node), "master@") of
	1 -> case application:get_env(playii, Key) of
		 {ok, Val} -> Val;
		 _ -> undefined
	     end;
	_ -> undefined
    end.
	    
%% DB :: Store Config info in mnesia database
%% private

db_load(Node, Key) ->
    case playii_db:load({config, db_key(Node, Key)}) of
        #config{value=Val} -> Val;
	_ -> undefined
    end.

%%====================================================================
%% DB :: Utils
%%====================================================================

db_prefix(Node) ->
    atom_to_list(Node) ++ ":".

db_key(Node, Key) ->
    db_prefix(Node) ++ atom_to_list(Key).

%% join some node into cloud with config
%% playii_core:join(nodename@cloudname, ["type=value", "type=value"]).

dump() ->
    D = playii_db:dump(config),
    io:format("~p~n", [lists:keysort(2, D)]).

%% join some node into cloud with config
%% playii_core:join(nodename@cloudname, ["type=value", "type=value"]).

add_node(Node, ConfigList) ->
    add_node(Node, ConfigList, []).

add_node(_Node, [], Acc) ->
    playii_db:save(Acc);
add_node(Node, [H | T], Acc) ->
    [N, V] = string:tokens(H, ":="),
    add_node(Node, T, [ #config{key=db_key(Node, N), value=V} | Acc ]).

%% quit some node from cloud
%% playii_core:quit(nodename@cloudname).

del_node(Node) ->
    H = db_prefix(Node),
    D = fun(#config{key=Key}) ->
		case string:str(Key, H) of
		    1 -> {true, Key};
		    _ -> false
		end
	end,
    playii_db:batch_delete({config, D}).

%%====================================================================
%% Internal functions
%%====================================================================

%% parse config value

parse_val(V) ->
    chain(V, [fun try_bool/1, fun try_int/1]).

chain({ok, Val}, _) -> Val;
chain(V, []) -> V;
chain(V, [H | T]) -> chain(H(V), T).

try_int(V) when is_integer(V) -> {ok, V};
try_int(V) when is_list(V) ->
    case catch list_to_integer(V) of
	Int when is_integer(Int) -> {ok, Int};
	_ -> V
    end;
try_int(V) -> V.

try_bool(V) when is_atom(V) ->
    case V of
	true -> {ok, true};
	false -> {ok, false};
	_ -> V
    end;
try_bool(V) when is_list(V) ->
    case string:to_lower(V) of
	"true" -> {ok, true};
	"false" -> {ok, false};
	"1" -> {ok, true};
	"0" -> {ok, false};
	"on" -> {ok, true};
	"off" -> {ok, false};
	"yes" -> {ok, true};
	"no" -> {ok, false};
	_ -> V
    end;
try_bool(V) -> V.

%% ----------

%% ---- license logic

identify(PublicKey) ->
    %% composite
    SSP = fingerprint()++"|"++configinfo(),
    %% encrypt
    Enc = crypto:rsa_public_encrypt(SSP, PublicKey, rsa_pkcs1_padding),
    base64:encode_to_string(Enc).

verify(PublicKey, GP) ->
    %% decrypt
    Dgp = base64:decode(GP),
    Dec = crypto:rsa_public_decrypt(Dgp, PublicKey, rsa_pkcs1_padding),
    SGP = binary_to_list(Dec),
    %% parse
    [GFP, Date] = string:tokens(SGP, "|"),
    {ok, [Year, Mon, Day], []} = io_lib:fread("~4d~2d~2d", Date),
    GDays = calendar:date_to_gregorian_days({Year, Mon, Day}),
    %% origin
    FP = fingerprint(),
    Days = calendar:date_to_gregorian_days(date()),
    %% debug
    ?debugVal({FP, GFP}),
    ?debugVal({Days, GDays}),
    %% match
    if
	FP =:= GFP, Days < GDays -> true;
	true -> false
    end.

%% ---- machine identify logic

%% @spec must be a string
fingerprint() ->
    address().

configinfo() ->
    Wa = load(node(), 'web.addr'),
    Wp = load(node(), 'web.port'),
    Wm = load(node(), 'web.max'),
    Pm = load(node(), 'player.max'),
    Sm = load(node(), 'scene.max'),
    io_lib:format(
      "web.addr=~p,web.port=~p,web.max=~p,player.max=~p,scene.max=~p",
      [Wa, Wp, Wm, Pm, Sm]).

%%====================================================================
%% MacAddress functions
%%====================================================================

file_exists(FileName) ->
  case filelib:is_regular(FileName) of
    true ->
      true;
    %% Even if its not a regular file, it might still exist
    %% /dev/null exhibits this behavior
    false ->
      case filelib:last_modified(FileName) of
        0 ->
          false;
        _ ->
          true
      end
  end.

identify_null_file() ->
    case file_exists("/dev/null") of
        true -> "/dev/null";
        false -> "NUL"
    end.

%% Warning: do not export this because it allows arbitrary os command execution
get_interface_info(Cmd) ->
    %% @TODO os:type() may be more useful here than the original approach
    CmdString = Cmd ++ " 2>" ++ identify_null_file(),
    case os:cmd(CmdString) of
        [] -> "";
        {ok, Data} -> Data;
        Data -> Data
    end.

%%% @doc Exported for testability.   Internal use only.
mac_matcher(Line, Acc) ->
  MACRegex = " (?<FOO>([0-9A-F][0-9A-F][:\\-]){5}[0-9A-F][0-9A-F])([^:\\-0-9A-F]|$)",
  case re:run(Line, MACRegex, [caseless, {capture,['FOO']}]) of
    {match, [{Start, Length}]} ->
      MACAddress = string:strip(lists:sublist(Line, Start, Length+1)),
      {ok, StdMACAddress, _} =  regexp:gsub(MACAddress, "-", ":"),
      [StdMACAddress|Acc];
    _ ->
      Acc
  end.

%%% @doc Retrieve list of MAC addresses for machine
address_list() ->
  LinesLists = lists:map(fun get_interface_info/1, ["/sbin/ifconfig", "/bin/ifconfig", "ifconfig", "ipconfig /all"]),
  {ok, ALines} = regexp:split(string:join(LinesLists," "), "[\r\n]"),
  Lines = lists:filter(fun(Elem) ->  Elem /= []  end, ALines),

  Candidates0 = lists:foldl(fun mac_matcher/2, [], Lines),

  %% Length check to avoid some false hits from the regex because re module does not seem to support more complex regex to handle it
  Candidates = lists:reverse(lists:filter(fun(Elem) ->  (Elem /= "00:00:00:00:00:00" andalso
                                                         Elem /= "00-00-00-00-00-00" andalso
                                                         length(Elem) =< 17 ) end, Candidates0)),
  case length(Candidates) of
    0 -> throw({error, {no_mac_address_candidate, "No MAC Address"}});
    _ -> ok
  end,
  lists:usort(Candidates).  % remove duplicates

%%% @doc Retrieve the first MAC addresses for machine
address() ->
    hd(address_list()).
