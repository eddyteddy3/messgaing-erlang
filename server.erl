-module(server).
-export([register_user/2, log_in/2, follow/3, send_message/3, get_timeline/2, get_profile/2, get_all_users/1]).

register_user(ServerPid, UserName) ->
    server_centralized:register_user(ServerPid, UserName).

log_in(ServerPid, UserName) ->
    server_centralized:log_in(ServerPid, UserName).

follow(ServerPid, UserName, UserNameToFollow) ->
    server_centralized:follow(ServerPid, UserName, UserNameToFollow).

send_message(ServerPid, UserName, MessageText) ->
    % io:format("Message in server.erl : ~p~n", [MessageText]),
    Timestamp = erlang:system_time(),
    server_centralized:send_message(ServerPid, UserName, MessageText, Timestamp).

get_timeline(ServerPid, UserName) ->
    server_centralized:get_timeline(ServerPid, UserName).

get_profile(ServerPid, UserName) ->
    server_centralized:get_profile(ServerPid, UserName).

get_all_users(ServerPid) ->
    server_centralized:get_all_users(ServerPid).
