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


      % receive
      %   Y -> io:fwrite("Y: ~p\n", [Y])
      % end,
      % Sender ! {receive_message, "#andra", "Meddelandet"},


      % {reply, { error, user_already_joined, "User joined :(" }, NewState};

    {leave, Channel, Sender} ->
      { _, ChannelPid } = get_channel(Channel, AllChannels),

      genserver:request(ChannelPid, {leave, Sender}),

      NewState = State,


      {reply, join, NewState};

    {message_send, Channel, Nick, Msg, Sender} ->
      io:fwrite("message_send:\nChannel: ~p\nMessage: ~p\n", [Channel, Msg]),
      { _, ChannelPid } = get_channel(Channel, AllChannels),
      ChannelPid ! {request, self(), make_ref(), {message_send, Nick, Msg, Sender}},
      {reply, message_send, State}
  end.

send_to_all(Receivers, Channel, Nick, Message, Sender) ->
  [ genserver:request(X, {message_receive, Channel, Nick, Message}) || X <- Receivers, X =/= Sender].

  % FirstReceiver ! {message_receive, Channel, "Nick", Message}.


  % request("n", {message_send, Channel, "String"}),

  % T = hd(Receivers),
  % N = list_to_pid(T),
  % N ! {message_receive, Channel, "Nick", "Msg"}.

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
    {join, Pid, Server} ->
      IsMember = user_exists_in_channel(Pid, State#channelState.users),
      case IsMember of
        true ->
          % {reply, {error, user_already_joined, "E"}, State};
          io:fwrite("Server: ~p\n", [Server]),
          Server ! error,
          {reply, join, State};
        false ->
          NewState = State#channelState{users = [ Pid | State#channelState.users ]},
          Server ! ok,
          {reply, join, NewState}
      end;

    {leave, Pid} ->
      OldUsers = State#channelState.users,
      NewUsers = list_remove(Pid, OldUsers, []),
      NewState = State#channelState{users = NewUsers},
      io:fwrite("== leaving channel: ~p\n", [NewState]),
      {reply, leave, NewState};

    {message_send, Nick, Msg, Sender} ->
      io:fwrite("== message_send in handle_channel: ~p\nFrom: ~p\n", [Msg, Sender]),
      send_to_all(State#channelState.users, State#channelState.name, Nick, Msg, Sender),
      {reply, message_receive, State}
  end.
