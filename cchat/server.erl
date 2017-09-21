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
    State = #serverState{},
    Pid = genserver:start(ServerAtom, State, fun handle_server/2),
    io:fwrite("Server created: pid: ~p\n", [Pid]),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).

  get_channel(_, []) ->
    { false, 0 };
  get_channel(Needle, Haystack) ->
    { First, Pid } = hd(Haystack),
    if
      Needle == First -> { true, Pid };
      true -> get_channel(Needle, tl(Haystack))
    end.

get_nick(_, []) ->
  false;
get_nick(Needle, Haystack) ->
  Nick = hd(Haystack),
  if
    Needle == Nick -> true;
    true -> get_nick(Needle, tl(Haystack))
  end.

handle_server(State, Data) ->
  io:fwrite("\n|| handle_server:\nState: ~p\nData: ~p\n", [State, Data]),

  AllChannels = State#serverState.channels,

  case Data of
    {join, Channel, Nick, Sender} ->
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),
      if
        ChannelExists ->
          genserver:request(ChannelPid, {join, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok ->
              NickExists = get_nick(Nick, State#serverState.nicks),
              if
                NickExists == true ->
                  {reply, join, State};
                true ->
                  NewState = #serverState{nicks = [ Nick | State#serverState.nicks ], channels = State#serverState.channels},
                  {reply, join, NewState}
              end
          end;

        true ->
          Pid = genserver:start(list_to_atom(Channel), #channelState{ name=Channel, users=[ Sender ]}, fun handle_channel/2),
          NewState = State#serverState{channels = [ { Channel, Pid } | AllChannels ], nicks = State#serverState.nicks},
          {reply, join, NewState}
      end;

    {leave, Channel, Sender} ->
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),

      if
        ChannelExists ->
          genserver:request(ChannelPid, {leave, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok -> {reply, leave, State}
          end;

        true ->
          {reply, error, State}
      end;

    {message_send, Channel, Nick, Msg, Sender} ->
      % io:fwrite("message_send:\nChannel: ~p\nNick: ~p\nMessage: ~p\n", [Channel, Nick, Msg]),
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),

      if
        ChannelExists ->
          genserver:request(ChannelPid, {message_send, Nick, Msg, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok -> {reply, message_send, State}
          end;

        true ->
          {reply, error, State}
      end;

    {nick, Nick} ->
      NickExists = get_nick(Nick, State#serverState.nicks),
      if
        NickExists == true ->
          {reply, error, State};
        true ->
          NewState = #serverState{nicks = [ Nick | State#serverState.nicks ], channels = State#serverState.channels},
          { reply, ok, NewState }
      end;
    stop ->
      AllChannels = State#serverState.channels,
      [ genserver:stop(list_to_atom(ChannelName)) || { ChannelName, _ } <- AllChannels ],
      io:fwrite("Stop channels")
  end.

% --- This may only be moved to where it is needed, since it's just one row?
send_to_all(Receivers, Channel, Nick, Message, Sender) ->
  spawn(fun() -> [ genserver:request(X, {message_receive, Channel, Nick, Message}) || X <- Receivers, X =/= Sender] end ).



list_remove(_, [], Rest) ->
  Rest;
list_remove(Needle, Haystack, Rest) ->
  User = hd(Haystack),
  if
    Needle == User ->
      list_remove(Needle, tl(Haystack), Rest);
    true ->
      list_remove(Needle, tl(Haystack), [ hd(Haystack) | Rest ])
  end.

user_exists_in_channel(_, []) ->
  false;
user_exists_in_channel(Needle, Haystack) ->
  User = hd(Haystack),
  if
    Needle == User -> true;
    true -> user_exists_in_channel(Needle, tl(Haystack))
  end.

handle_channel(State, Data) ->
  io:fwrite("\n|| handle_channel:\nState: ~p\nData: ~p\n", [State, Data]),
  case Data of
    {join, Sender, Server} ->
      IsMember = user_exists_in_channel(Sender, State#channelState.users),
      case IsMember of
        true ->
          io:fwrite("Server: ~p\n", [Server]),
          Server ! error,
          {reply, join, State};
        false ->
          NewState = State#channelState{users = [ Sender | State#channelState.users ]},
          Server ! ok,
          {reply, join, NewState}
      end;

    {leave, Sender, Server} ->
      IsMember = user_exists_in_channel(Sender, State#channelState.users),
      case IsMember of
        true ->
          OldUsers = State#channelState.users,
          NewUsers = list_remove(Sender, OldUsers, []),
          NewState = State#channelState{users = NewUsers},
          io:fwrite("== leaving channel: ~p\n", [NewState]),
          Server ! ok,
          {reply, leave, NewState};
        false ->
          Server ! error,
          {reply, error, State}
      end;

    {message_send, Nick, Msg, Sender, Server} ->
      IsMember = user_exists_in_channel(Sender, State#channelState.users),
      case IsMember of
        true ->
          send_to_all(State#channelState.users, State#channelState.name, Nick, Msg, Sender),
          Server ! ok,
          {reply, message_send, State};
        false ->
          Server ! error,
          {reply, error, State}
      end
  end.
