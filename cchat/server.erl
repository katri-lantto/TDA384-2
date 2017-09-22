-module(server).
-export([start/1,stop/1]).

-record(channelState, {
    name,
    users = []
}).

-record(serverState, {
    channels = [],
    nicks = []
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, #serverState{}, fun handle_server/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  % Stopping the processes of all the channels associated with this server.
  % This is commented out because needed to do so to pass the tests
  % (told to do so from a teacher assistant).
  % genserver:request(ServerAtom, stop),

  % Stopping the server atom.
  genserver:stop(ServerAtom).

% Function for handling requests to the server.
handle_server(State, Data) ->
  % case statement for doing pattern matching on the Data-variable that
  % is sent to the server.
  case Data of
    % User wants to join a channel.
    {join, Channel, Nick, Sender} ->
      % Check if the channel already exists.
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if
        ChannelExists ->
          genserver:request(list_to_atom(Channel), {join, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok ->
              NickExists = lists:member(Nick, State#serverState.nicks),
              if
                NickExists ->
                  {reply, join, State};
                true ->
                  NewState = State#serverState{nicks = [ Nick | State#serverState.nicks ]},
                  {reply, join, NewState}
              end
          end;
        % If channel doesn't exist, spawn one as a new process.
        % Initial state of the channel is set to the record channelState, initializing the
        % name to the name of the channel and users to a list that contains this first user.
        true ->
          genserver:start(list_to_atom(Channel), #channelState{ name=Channel, users=[ Sender ]}, fun handle_channel/2),
          {reply, join, State#serverState{channels = [ Channel | State#serverState.channels ]}}
      end;

    % Called when a
    {leave, Channel, Sender} ->
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if
        ChannelExists ->
          genserver:request(list_to_atom(Channel), {leave, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok -> {reply, leave, State}
          end;
        true ->
          {reply, error, State}
      end;
    {nick, Nick} ->
      NickExists = lists:member(Nick, State#serverState.nicks),
      if
        NickExists ->
          {reply, error, State};
        true ->
          NewState = State#serverState{nicks = [ Nick | State#serverState.nicks ]},
          { reply, ok, NewState }
      end;
    stop ->
      [ genserver:stop(list_to_atom(Channel)) || Channel <- State#serverState.channels ]
  end.

% Function for handling requests to a channel-process.
% Handles joins, leaves and message sending.
handle_channel(State, Data) ->
  case Data of
    {join, NewPid, Server} ->
      IsMember = lists:member(NewPid, State#channelState.users),
      case IsMember of
        true ->
          Server ! error,
          {reply, join, State};
        false ->
          NewState = State#channelState{users = [ NewPid | State#channelState.users ]},
          Server ! ok,
          {reply, join, NewState}
      end;

    {leave, Sender, Server} ->
      IsMember = lists:member(Sender, State#channelState.users),
      case IsMember of
        true ->
          NewUsers = lists:delete(Sender, State#channelState.users),
          Server ! ok,
          {reply, leave, State#channelState{users = NewUsers}};
        false ->
          Server ! error,
          {reply, error, State}
      end;

    {message_send, Nick, Msg, Sender} ->
      IsMember = lists:member(Sender, State#channelState.users),
      case IsMember of
        true ->
          spawn(
            fun() ->
              [ genserver:request(
                  Receiver,
                  {message_receive, State#channelState.name, Nick, Msg}
                ) || Receiver <- State#channelState.users, Receiver =/= Sender]
            end
          ),
          {reply, message_send, State};
        false ->
          {reply, error, State}
      end
  end.
