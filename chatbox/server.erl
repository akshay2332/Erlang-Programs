-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
  catch (unregister(server)),
  register(server, self()),
  case whereis(testsuite) of
    undefined -> ok;
    TestSuitePID -> TestSuitePID ! {server_up, self()}
  end,
  loop(
    #serv_st{
      nicks = maps:new(), %% nickname map. client_pid => "nickname"
      registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
      chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
    }
  ).

loop(State) ->
  receive
  %% initial connection
    {ClientPID, connect, ClientNick} ->
      NewState =
        #serv_st{
          nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
          registrations = State#serv_st.registrations,
          chatrooms = State#serv_st.chatrooms
        },
      loop(NewState);
  %% client requests to join a chat
    {ClientPID, Ref, join, ChatName} ->
      NewState = do_join(ChatName, ClientPID, Ref, State),
      loop(NewState);
  %% client requests to join a chat
    {ClientPID, Ref, leave, ChatName} ->
      NewState = do_leave(ChatName, ClientPID, Ref, State),
      loop(NewState);
  %% client requests to register a new nickname
    {ClientPID, Ref, nick, NewNick} ->
      NewState = do_new_nick(State, Ref, ClientPID, NewNick),
      loop(NewState);
  %% client requests to quit
    {ClientPID, Ref, quit} ->
      NewState = do_client_quit(State, Ref, ClientPID),
      loop(NewState);
    {TEST_PID, get_state} ->
      TEST_PID ! {get_state, State},
      loop(State)
  end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
  ChatRooms = State#serv_st.chatrooms,
  ChatRoomsList = maps:keys(ChatRooms),
  DiffList = [ChatName] -- ChatRoomsList,
  if
    DiffList == [] ->
      ChatRoomPid = maps:get(ChatName, ChatRooms);
    true ->
      ChatRoomPid = spawn(chatroom, start_chatroom, [ChatName])

  end,

  ClientNick = maps:get(ClientPID, State#serv_st.nicks),
  ChatRoomPid ! {self(), Ref, register, ClientPID, ClientNick},

  OldRegistration = State#serv_st.registrations,
  Condition = maps:is_key(ChatName, OldRegistration),
  if
    Condition ->
      ListClient = maps:get(ChatName, OldRegistration),
      NewClientList = lists:append([ClientPID], ListClient),
      NewRegistration = maps:put(ChatName, NewClientList, OldRegistration);
    true ->
      NewRegistration = maps:put(ChatName, [ClientPID], OldRegistration)
  end,

  OldChatRooms = State#serv_st.chatrooms,
  NewChatRooms = maps:put(ChatName, ChatRoomPid, OldChatRooms),

  NewState =
    #serv_st{
      nicks = State#serv_st.nicks,
      registrations = NewRegistration,
      chatrooms = NewChatRooms
    },

  NewState.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->

  StateServerChatRoom = State#serv_st.chatrooms,
  ChatRoomPid = maps:get(ChatName, StateServerChatRoom),

  ClientPidList = maps:get(ChatName, State#serv_st.registrations),
  UpdatedRegistration = ClientPidList -- [ClientPID],

  ChatRoomPid ! {self(), Ref, unregister, ClientPID},

  ClientPID ! {self(), Ref, ack_leave},

  NewState =
    #serv_st{
      nicks = State#serv_st.nicks,
      registrations = maps:put(ChatName, UpdatedRegistration, State#serv_st.registrations),
      chatrooms = State#serv_st.chatrooms
    },

  NewState.

send_chatroom_nick_update([], ChatPidMap, Ref, NewNick, ServerPid, ClientPID) ->
  ok;
send_chatroom_nick_update([_H | T], ChatPidMap, Ref, NewNick, ServerPid, ClientPID) ->
  ChatRoomPid = maps:get(_H, ChatPidMap),
  ChatRoomPid ! {ServerPid, Ref, update_nick, ClientPID, NewNick},
  send_chatroom_nick_update(T, ChatPidMap, Ref, NewNick, ServerPid, ClientPID).

create_chatroom_list([], ClientId, List) ->
  List;
create_chatroom_list([_H | T], ClientId, List) ->

  ListTuple = tuple_to_list(_H),
  ChatName = lists:nth(1, ListTuple),
  Clients = lists:nth(2, ListTuple),
  DiffClient = [ClientId] -- Clients,

  if
    DiffClient == [] ->
      NewList = lists:append(List, [ChatName]),
      create_chatroom_list(T, ClientId, NewList);
    true ->
      create_chatroom_list(T, ClientId, List)
  end.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->

  NicknamesMap = State#serv_st.nicks,
  NickNameList = maps:values(NicknamesMap),
  Difference_List = [NewNick] -- NickNameList,
  if
    Difference_List == [] ->
      ClientPID ! {Ref, err_nick_used},
      State;
    true ->
      %%
      %%  Server code for changing name in chatroom
      %%  The server must now update all chatrooms to which the client belongs
      %%  that the nickname of the user has changed
      %%  ,by sending each relevant chatroom the message
      %%  {self(), Ref, update_nick, ClientPID, NewNick} [C] to the chatrooms.

      ClientChatRegistration = State#serv_st.registrations,
      ClientChatRegistrationList = create_chatroom_list(maps:to_list(ClientChatRegistration), ClientPID, []),

      send_chatroom_nick_update(ClientChatRegistrationList, State#serv_st.chatrooms, Ref, NewNick, self(), ClientPID),

      NewState =
        #serv_st{
          nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks),
          registrations = State#serv_st.registrations,
          chatrooms = State#serv_st.chatrooms
        },
      ClientPID ! {self(), Ref, ok_nick},
      NewState
  end.

send_deregistration_request([], ChatPidMap, Ref, ServerPid, ClientPID) ->
  ok;
send_deregistration_request([_H | T], ChatPidMap, Ref, ServerPid, ClientPID) ->
  ChatRoomPid = maps:get(_H, ChatPidMap),
  ChatRoomPid ! {ServerPid, Ref, unregister, ClientPID},
  send_deregistration_request(T, ChatPidMap, Ref, ServerPid, ClientPID).


remove_client_registration([], RegistrationMap, ClientPID) ->
  RegistrationMap;
remove_client_registration([_H | T], RegistrationMap, ClientPID) ->
  ClientPidList = maps:get(_H, RegistrationMap),
  UpdateClientPidList = ClientPidList -- [ClientPID],
  NewRegistrationsMap = maps:put(_H, UpdateClientPidList, RegistrationMap),
  remove_client_registration(T, NewRegistrationsMap, ClientPID).

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->

  NewNicks = maps:remove(ClientPID, State#serv_st.nicks),
  ClientChatRegistrationList = create_chatroom_list(maps:to_list(State#serv_st.registrations), ClientPID, []),

  send_deregistration_request(ClientChatRegistrationList, State#serv_st.chatrooms, Ref, self(), ClientPID),
  NewRegistrationMap = remove_client_registration(ClientChatRegistrationList, State#serv_st.registrations, ClientPID),
  NewState =
    #serv_st{
      nicks = NewNicks,
      registrations = NewRegistrationMap,
      chatrooms = State#serv_st.chatrooms
    },
  ClientPID ! {self(), Ref, ack_quit},
  NewState.
