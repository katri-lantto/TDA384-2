-module(server).
-export([start/1,stop/1]).

-record(channelState, {
    name,
    users = []
}).

-record(serverState, {
    channels = []
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    State = #serverState{},
    Pid = genserver:start(ServerAtom, State, fun handle_server/2),
    io:fwrite("Server pid: ~p\n", [Pid]),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

get_channel(_, []) ->
  { false, 0 };
get_channel(Needle, Haystack) ->
  { First, Pid } = hd(Haystack),
  if
    Needle == First -> { true, Pid };
    true -> get_channel(Needle, tl(Haystack))
  end.

handle_server(State, Data) ->
  io:fwrite("\n|| handle_server:\nState: ~p\nData: ~p\n", [State, Data]),

  AllChannels = State#serverState.channels,

  case Data of
    {join, Channel, Sender} ->
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),
      if
        ChannelExists ->
          genserver:request(ChannelPid, {join, Sender, self()}),
          receive
            error -> {reply, error, State};
            ok -> {reply, join, State}
          end;

        true ->
          Pid = genserver:start(list_to_atom(Channel), #channelState{ name=Channel, users=[ Sender ]}, fun handle_channel/2),
          NewState = State#serverState{channels = [ { Channel, Pid } | AllChannels ]},
          {reply, join, NewState}
      end;

    {leave, Channel, Sender} ->
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),

      if
        ChannelExists ->
          genserver:request(ChannelPid, {leave, Sender, self()}),
          io:fwrite("--- Leaving : channel exists\n"),
          receive
            error -> io:fwrite("--- Leaving : error\n"), {reply, error, State};
            ok -> io:fwrite("--- Leaving : ok\n"), {reply, leave, State}
          end;

        true ->
          io:fwrite("--- Leaving : channel does not exist\n"),
          {reply, error, State}
      end;

    {message_send, Channel, Nick, Msg, Sender} ->
      io:fwrite("message_send:\nChannel: ~p\nNick: ~p\nMessage: ~p\n", [Channel, Nick, Msg]),
      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),

      if
        ChannelExists ->
          % ChannelPid ! {request, self(), make_ref(), {message_send, Nick, Msg, Sender}},
          genserver:request(ChannelPid, {message_send, Nick, Msg, Sender, self()}),
          % {reply, message_send, State}
          receive
            error -> io:fwrite("--- Message : error\n"), {reply, error, State};
            ok -> io:fwrite("--- Message : ok\n"), {reply, message_send, State}
          end;

        true ->
          io:fwrite("--- Message : channel does not exist\n"),
          {reply, error, State}
      end
  end.

% --- This may only be moved to where it is needed, since it's just one row?
send_to_all(Receivers, Channel, Nick, Message, Sender) ->
  [ genserver:request(X, {message_receive, Channel, Nick, Message}) || X <- Receivers, X =/= Sender].

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

user_exists_in_channel(Needle, []) ->
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
      % io:fwrite("== message_send in handle_channel: ~p\nFrom: ~p\n", [Msg, Sender]),
      % send_to_all(State#channelState.users, State#channelState.name, Nick, Msg, Sender),
      % {reply, message_send, State}
  end.
