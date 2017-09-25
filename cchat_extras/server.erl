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
    % genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).

handle_server(State, Data) ->
  AllChannels = State#serverState.channels,

  case Data of
    {join, Channel, Nick, Sender} ->
      ChannelExists = lists:member(Channel, AllChannels),
      if
        ChannelExists ->
          genserver:request(list_to_atom(Channel), {join, Sender, Nick, self()}),
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

        true ->
          genserver:start(list_to_atom(Channel), #channelState{ name=Channel, users=[ Sender ]}, fun handle_channel/2),
          NewState = State#serverState{channels = [ Channel | AllChannels ]},
          {reply, join, NewState}
      end;

    {leave, Channel, Nick, Sender} ->
      ChannelExists = lists:member(Channel, AllChannels),

      if
        ChannelExists ->
          genserver:request(list_to_atom(Channel), {leave, Sender, Nick, self()}),
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

handle_channel(State, Data) ->
  case Data of
    {join, NewPid, Nick, Server} ->
      IsMember = lists:member(NewPid, State#channelState.users),
      case IsMember of
        true ->
          Server ! error,
          {reply, join, State};
        false ->
          NewState = State#channelState{users = [ NewPid | State#channelState.users ]},
          nofify_channel(State#channelState.name, State#channelState.users, Nick, "Joined the channel"),
          Server ! ok,
          {reply, join, NewState}
      end;

    {leave, Sender, Nick, Server} ->
      IsMember = lists:member(Sender, State#channelState.users),
      case IsMember of
        true ->
          OldUsers = State#channelState.users,
          NewUsers = lists:delete(Sender, OldUsers),
          NewState = State#channelState{users = NewUsers},
          nofify_channel(NewState#channelState.name, NewState#channelState.users, Nick, "Left the channel"),
          Server ! ok,
          {reply, leave, NewState};
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

nofify_channel(Channel, Users, Nick, Msg) ->
  spawn(
    fun() ->
      [ genserver:request(
          Receiver,
          {message_receive, Channel, Nick, Msg}
        ) || Receiver <- Users]
    end
  ).
