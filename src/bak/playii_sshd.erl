-module(playii_sshd).

%% Export API
-export([start/0]).
%% Export File_Handler
-export([close/1, delete/1, del_dir/1, get_cwd/0, is_dir/1, list_dir/1, 
	 make_dir/1, make_symlink/2, open/2, position/2, read/2,
	 read_file_info/1, read_link/1, read_link_info/1, rename/2,
	 write/2, write_file_info/2]).

%% ---- API

start() ->
    crypto:start(),
    ssh:start(),
    ssh_cli:listen(
	%% ---- CUSTOM SHELL
        fun shell/2,
        {0,0,0,0}, 10022, 
        [{system_dir, "./priv/ssh"},
	 %% ---- START SFTP THE SAME PORT
	 %% {subsystems, [{"sftp", ssh_sftpd}]},
	 %% ---- CUSTOM SFTP FILE_HANDLER
         %% {file_handler, ?MODULE},
         %% {user_passwords, [{"jack", "jack"}]},
	 %% ---- CUSTOM PAM
         {pwdfun, fun pwd/2},
         {connectfun, fun conn/3},
	 {disconnectfun, fun disconn/1},
	 {infofun, fun fail/2},
	 {failfun, fun fail/2}
        ]),
    ok.


stop(Pid) ->
    ssh_cli:stop(Pid).

%% ---- SHELL functions
shell(User, PeerAddr) ->
    spawn( fun() -> server(User, PeerAddr) end ).

server(User, PeerAddr) ->
    io:fwrite("Hi, ~s [from:~p]. Welcome to Playii Shell.~n", [User, PeerAddr]),
    server_loop(User).

server_loop(User) ->
    Line = io:get_line('$ '),
    case exec(User, string:tokens(Line, " \n")) of
        {ok, eof} ->
            ok;
        {ok, nop} ->
            server_loop(User);
        {ok, Res} when is_list(Res) ->
            io:fwrite("~s~n", [Res]),
            server_loop(User);
        {error, E} ->
            io:fwrite("** ~p **~n", [E]),
            server_loop(User)
    end.
    %% io:fwrite("** bye **~n", []).

exec(_U, []) ->
    {ok, nop};
exec(_U, ["exit"]) ->
    {ok, eof};
exec(_U, ["help"]) ->
    {ok, "help messages..."};
exec(_U, ["error"]) ->
    {error, trigger_error};
exec(_U, [Cmd | Args]) ->
    {ok, io_lib:format("unknown or todo command: ~s ~p", [Cmd, Args])}.

%% ---- PAM functons

%% pwd(User, Pass) ->
%%    true user/pass ok, no pam
%%    | {true, Handle} user/pass ok, with pam
%%    | {false, "Bad password"} user/pass failure
pwd(User, Pass) ->
    io:format("PAM:pwd(~s, ~s)~n", [User, Pass]),
    case {User, Pass} of
        {"jack", "jack"} -> true;
        _ -> {false, "Bad password"}
    end.

%% conn(User, PeerAddr, Method) -> ok
%%    todo log...
conn(User, PeerAddr, Method) ->
    io:format("PAM:conn(~p, ~p, ~p)~n", [User, PeerAddr, Method]),
    ok.

%% disconn(Handle) -> ok
%%    todo finalize the Handle
disconn(Handle) ->
    io:format("PAM:disconn(~p)~n", [Handle]),
    ok.

%% Reason for failfun() is a structured tuple, the following values are 
%% possible

% {passwd, Str}   - if the password was bad 
% {authmethod, none}  - if the client attempted to use none as method
% {authmethod, OtherStr} - if the client tried to use an unknown authmeth
% {error, key_unacceptable} - if a the key sent to us wasn't verifiable
% {error, inconsistent_key} - we got a broken DSA key
% {error, invalid_signature} - again, broken key
% {error, {{openerr, PosixAtom}, {file, Filename}}} - if we failed to open the 
%                                                     file containing the user keys
% {error, nouserdir}   - No dir configured for user which contains keys

%% fail(User, Reason) -> ok
%%    todo log...
fail(User, Reason) ->
    io:format("PAM:fail(~s, ~p)~n", [User, Reason]),
    ok.


%% ---- File_Handler

close(IoDevice) ->
    io:format("SFTP:close(~p)~n", [IoDevice]),
    file:close(IoDevice).

delete(Path) ->
    io:format("SFTP:delete(~p)~n", [Path]),
    file:delete(Path).

del_dir(Path) ->
    io:format("SFTP:del_dir(~p)~n", [Path]),
    file:del_dir(Path).

get_cwd() ->
    io:format("SFTP:get_cwd()~n", []),
    file:get_cwd().

is_dir(AbsPath) ->
    io:format("SFTP:is_dir(~p)~n", [AbsPath]),
    filelib:is_dir(AbsPath).

list_dir(AbsPath) ->
    io:format("SFTP:list_dir(~p)~n", [AbsPath]),
    file:list_dir(AbsPath).
     
make_dir(Dir) ->
    io:format("SFTP:make_dir(~p)~n", [Dir]),
    file:make_dir(Dir).
     
make_symlink(Path2, Path) ->
    io:format("SFTP:make_symlink(~p, ~p)~n", [Path2, Path]),
    file:make_symlink(Path2, Path).

open(Path, Flags) ->
    io:format("SFTP:open(~p, ~p)~n", [Path, Flags]),
    file:open(Path, Flags).
     
position(IoDevice, Offs) ->
    io:format("SFTP:position(~p, ~p)~n", [IoDevice, Offs]),
    file:position(IoDevice, Offs).

read(IoDevice, Len) ->
    io:format("SFTP:read(~p, ~p)~n", [IoDevice, Len]),
    file:read(IoDevice, Len).
          
read_link(Path) ->
    io:format("SFTP:read_link(~p)~n", [Path]),
    file:read_link(Path).

read_link_info(Path) ->
    io:format("SFTP:read_link_info(~p)~n", [Path]),
    file:read_link_info(Path).
     
read_file_info(Path) ->
    io:format("SFTP:read_file_info(~p)~n", [Path]),
    file:read_file_info(Path).

rename(Path, Path2) ->
    io:format("SFTP:rename(~p, ~p)~n", [Path, Path2]),
    file:rename(Path, Path2).

write(IoDevice, Data) ->
    io:format("SFTP:write(~p, ~p)~n", [IoDevice, Data]),
    file:write(IoDevice, Data).
     
write_file_info(Path,Info) ->
    io:format("SFTP:write_file_info(~p, ~p)~n", [Path, Info]),
    file:write_file_info(Path, Info).
