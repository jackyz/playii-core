%% 常量
-define(PlayerWait, 30000).        % 每个客户端请求的最长保持时间
-define(PlayerIdle, 15000).        % player 转换状态的时间
-define(SceneIdle,  5000).        % scene 转换状态的时间
-define(Logger, playii_log).

-define(PublicKey, [crypto:mpint(35), crypto:mpint(27450365369545658220931628060783091960506826362353763487083280362839043216841657369948164521498304809600833213957828942265406274722939600290214247360692674990311420995190837743802013307712470628389755869544246837977397004345235688292139241866667676583450930727444029322209984153477626998159737327538106124245547499805648867795103477879408868082707023820212151405924064375878934681103762983686416808673477242571596193142426457194408796802381080117854525482390194591680064013647316246080506198242283634648556489351871651374944797515704181521617329903890863521363293618386029951892185817441267704235886496373170135493241)]).

-define(EvalMax, 64).

%% 数据表
-record(backup, {
	  key,     %% backup key : "player_xxxx" or "scene_xxxxx/xxxx"
	  value    %% backup value : the backup state of instance
	 }).
-record(config, {
	  key,     %% config key : "node.service.field"
	  value    %% config value
	 }).
-record(log, {
 	  time,    %% time: the key, the value now()
 	  module,  %% module: the module that produce this log
 	  level,   %% level: debug, info, warn, error, fatal
 	  message  %% message: the message string to be loged
 	 }).

%% 单元测试
-include_lib("eunit/include/eunit.hrl").

%% 状态快照
-ifdef(TEST).
-define(snap, playii_snap_mock).
-else.
-define(snap, playii_snap_mock).   %% TODO
-endif.

%% 用户-场景持久化数据
-ifdef(TEST).
-define(state, playii_state_mock).
-else.
-define(state, playii_state_mock). %% TODO
-endif.

%% 日志
-ifdef(TEST).
-define(log(M, L, S),   %% with line number in log when test mode
	((fun() ->
		 {{YY,MO,DD},{HH,MI,SS}} = calendar:now_to_local_time(now()),
		 ?debugFmt(
		    "~4..0w.~2..0w.~2..0w-~2..0w:~2..0w:~2..0w[~5s][~s]:~s",
		    [YY, MO, DD, HH, MI, SS, L, M, S]),
		 ok
	 end)())).
-else.
-define(log(M, L, S),   %% without line number in log
	((fun() ->
		  gen_server:cast(?Logger, {log, M, L, S}),
		  ok
	  end)())).
-endif.

%% 日志
-define(log_debug(F,A),	(?log(?MODULE, debug, io_lib:format(F, A)))).
-define(log_info(F,A),	(?log(?MODULE, info,  io_lib:format(F, A)))).
-define(log_warn(F,A),	(?log(?MODULE, warn,  io_lib:format(F, A)))).
-define(log_error(F,A),	(?log(?MODULE, error, io_lib:format(F, A)))).
-define(log_fatal(F,A),	(?log(?MODULE, fatal, io_lib:format(F, A)))).

%% 测量
-ifdef(TEST).

-define(mark(Name),
	((fun() ->
              case get(??Name) of
	          undefined -> put(??Name, {0, now()});
	          {T} -> put(??Name, {T, now()})
              end
	  end)())).
-define(pass(Name),
	((fun() ->
              case get(??Name) of
	          undefined -> put(??Name, {0});
	          {T, L} -> put(??Name, {T+timer:now_diff(now(), L)})
              end
	  end)())).
-define(time(Name),
	((fun() ->
              case get(??Name) of
	          undefined -> 0;
	          {T} -> T
              end
	  end)())).

-else.

-define(mark(Name), ok).
-define(pass(Name), ok).
-define(time(Name), ok).

-endif.    
