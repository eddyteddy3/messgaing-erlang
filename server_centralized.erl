-module(server_centralized).
% -export([initialize/0, register_user/2, log_in/2, follow/3, send_message/4, get_timeline/2, get_profile/2, get_all_users/1]).

-compile(export_all).

initialize() ->
    ServerPID = spawn_link(?MODULE, loop, [dict:new()]),
    register(server_centralized, ServerPID),
    ServerPID.

initialize_with(Users) ->
    ServerPID = spawn_link(?MODULE, loop, [Users]),
    register(server_centralized, ServerPID),
    % io:format("Sending request"),
    % ServerPID ! {restart_with_updated_users, Users, self()},
    % io:format("Sending Sent"),
    % updatedDicted = update_users(Users),
    ServerPID.

% update_users(Dict) ->
%     dict:map(fun (_) ->
%         % Create a new PID for each user
%         % Pid = spawn(fun() -> receive stop -> ok end end),
%         % {user, Name, Pid, Subscriptions, Messages}
%         io:format("IN A MAP: ")
%     end, Dict).

loop(UserProcesses) ->
    receive
        {register_user, UserName, From} ->
            % User = {user, Name, pid, UserPid, sets:from_list(Subscriptions), Messages},
            UserPid = spawn(user_process, start, [UserName]),
            NewUserProcesses = dict:store(UserName, {user, UserName, pid, UserPid, sets:new(), []}, UserProcesses),
            From ! {ok, user_registered},
            loop(NewUserProcesses);
        {restart_with_updated_users, Users, From} ->

            io:format("Received request"),

            % newUserProcess = dict:map(
            %     fun ({user, Name, Subscriptions, Messages}) ->
            %         io:format("IN A MAP: ")
            %     end,
            % Users),

            % io:format("newUserProcess : ~p.", [newUserProcess]),

            From ! {ok, user_updated},

            loop(Users);

        {log_in, UserName, From} ->
            % UserPid = spawn(user_process, start, [UserName])

            handle_user_request(UserProcesses, UserName, {log_in, UserName}, From),
            loop(UserProcesses);

        {follow, UserName, UserNameToFollow, From} ->
            handle_user_request(UserProcesses, UserName, {follow, UserNameToFollow}, From),
            loop(UserProcesses);

        {didFollowedSuccessfully, UpdatedUsers} ->
            self() ! {ok, didFollowedSuccessfully},
            loop(UpdatedUsers);

        % send_message, "Alice", "Helllo", time, "Bob"
        {send_message, UserName, MessageText, Timestamp, From} ->
            % io:format("sending message to : ~p~n", [UserName]),
            % io:format("from : ~p~n", [From]),
            % io:format("Text : ~p~n", [MessageText]),
            handle_user_request(UserProcesses, UserName, {send_message, UserName, MessageText, Timestamp}, From),
            loop(UserProcesses);

        {get_timeline, UserName, From} ->
            handle_user_request(UserProcesses, UserName, get_timeline, From),
            loop(UserProcesses);
        {get_profile, UserName, From} ->
            handle_user_request(UserProcesses, UserName, get_profile, From),
            loop(UserProcesses);
        {get_all_users, From} ->
            UserList = dict:fetch_keys(UserProcesses),
            From ! {ok, UserList},
            loop(UserProcesses)
    end.

handle_user_request(UserProcesses, UserName, Request, From) ->
    case dict:find(UserName, UserProcesses) of
        error ->
            From ! {error, user_not_found};
        {ok, User} ->
            % io:format("User PID in handle user request: ~p.", [User]),
            % io:format("~n"),
            {_, _, _, PID, _, _} = User,
            PID ! {handle_request, Request, From, UserProcesses, UserName}
    end.

register_user(ServerPid, UserName) ->
    ServerPid ! {register_user, UserName, self()},
    receive
        Response -> Response
    end.

log_in(ServerPid, UserName) ->
    ServerPid ! {log_in, UserName, self()},
    receive
        Response -> Response
    end.

follow(ServerPid, UserName, UserNameToFollow) ->
    ServerPid ! {follow, UserName, UserNameToFollow, self()},
    receive
        Response -> Response
    end.

send_message(ServerPid, UserName, MessageText, Timestamp) ->
    % io:format("Message in SC : ~p.", [UserName]),
    ServerPid ! {send_message, UserName, MessageText, Timestamp, self()},
    receive
        Response -> Response
    end.

get_timeline(ServerPid, UserName) ->
    ServerPid ! {get_timeline, UserName, self()},
    receive
        Response -> Response
    end.

get_profile(ServerPid, UserName) ->
    ServerPid ! {get_profile, UserName, self()},
    receive
        Response -> Response
    end.

get_all_users(ServerPid) ->
    ServerPid ! {get_all_users, self()},
    receive
        Response -> Response
    end.
