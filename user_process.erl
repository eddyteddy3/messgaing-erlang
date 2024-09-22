-module(user_process).
% -export([start/1, loop/1]).

-compile(export_all).

start(UserName) ->
    loop(#{"name" => UserName, "messages" => [], "following" => dict:new()}).

loop(State) ->
    receive
        {handle_request, {send_message, Sender, Message, Timestamp}, From, _, _} ->
            NewMessage = {Sender, Message, Timestamp},
            NewState = maps:update_with("messages", fun(Msgs) -> [NewMessage | Msgs] end, [NewMessage], State),
            From ! {ok, message_sent},
            loop(NewState);
        {handle_request, get_timeline, From, Users, UserName} ->
            % io:format("Usernane : ~p", [UserName]),
            % io:format("~n"),
            % io:format("Users : ~p", [Users]),
            % io:format("~n"),
            % Messages = maps:get("messages", State),
            Timeline = timeline(Users, UserName),

            From ! {ok, Timeline},
            loop(State);
        {handle_request, get_profile, From, _, _} ->
            Messages = maps:get("messages", State),
            From ! {ok, Messages},
            loop(State);
        {handle_request, {follow, UserNameToFollow}, From, Users, UserName} ->
            
            NewUsers = follow(Users, UserName, UserNameToFollow),

            Following = maps:get("following", State),
            NewFollowing = dict:store(UserNameToFollow, true, Following),
            NewState = maps:put("following", NewFollowing, State),

            From ! {didFollowedSuccessfully, NewUsers},
            
            loop(NewState)
    end.

follow(Users, UserName, UserNameToFollow) ->
    {user, Name, pid, UserPid, Subscriptions, Messages} = get_user(UserName, Users),
    NewUser = {user, Name, pid, UserPid, sets:add_element(UserNameToFollow, Subscriptions), Messages},
    dict:store(UserName, NewUser, Users).

timeline(Users, UserName) ->
    {user, _, _, _, Subscriptions, _} = get_user(UserName, Users),
    UnsortedMessagesForTimeLine =
        lists:foldl(fun(FollowedUserName, AllMessages) ->
                        AllMessages ++ get_messages(Users, FollowedUserName)
                    end,
                    [],
                    sets:to_list(Subscriptions)),
    sort_messages(UnsortedMessagesForTimeLine).

get_user(UserName, Users) ->
    % io:format("User to find : ~p", [UserName]),
    % io:format("User Groups : ~p", [Users]),
    % io:format("~n"),
    case dict:find(UserName, Users) of
        {ok, User} -> User;
        error -> throw({user_not_found, UserName})
    end.

get_messages(Users, UserName) ->
    {user, _, _, _, _, Messages} = get_user(UserName, Users),
    Messages.

sort_messages(Messages) ->
    % Sort on the 4th element of the message tuple (= timestamp, this uses 1-based
    % indexing), and then reverse to put most recent first.
    lists:reverse(lists:keysort(4, Messages)).