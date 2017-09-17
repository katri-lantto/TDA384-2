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
    io:fwrite("Server Namnet: ~p\n", [ServerAtom]),
    State = #serverState{},
    Pid = genserver:start(ServerAtom, State, fun handle_channels/2),
    io:fwrite("Server pid: ~p\n", [Pid]),
    % Pid ! {request, self(), make_ref(), "här är min data!"},

    Pid.
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % not_implemented.

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

handle_channels(State, Data) ->
  AllChannels = State#serverState.channels,

  case Data of
    {join, Channel, Sender} ->
      io:fwrite("Sender ~p joins ~p\n", [Sender, Channel]),

      { ChannelExists, ChannelPid } = get_channel(Channel, AllChannels),
      if
        ChannelExists ->
          ChannelPid ! {request, Sender, make_ref(), {join, Sender}},
          NewState = State;

        true ->
          Pid = genserver:start(list_to_atom(Channel), #channelState{ name=Channel, users=[ Sender ]}, fun handle_messages/2),
          NewState = State#serverState{channels = [ { Channel, Pid } | AllChannels ]}
      end,
      {reply, join, NewState};

    {message_send, Channel, Msg, Sender} ->
      io:fwrite("message_send:\nChannel: ~p\nMessage: ~p\n", [Channel, Msg]),
      { _, ChannelPid } = get_channel(Channel, AllChannels),
      ChannelPid ! {request, self(), make_ref(), {message_send, Msg}},
      {reply, message_send, State}
  end.

request(ClientName, Msg) ->
    io:fwrite("ClientName: ~p\nMessage: ~p\n", [ClientName, Msg]),
    genserver:request(list_to_atom(ClientName), "Msg", 100000).

send_to_all(Receivers, Channel) ->
  io:fwrite("All: ~p\n", [Receivers]),
  io:fwrite("Channel: ~p\n", [Channel]),
  [ io:fwrite("Reciver: ~p\n", [X]) || X <- Receivers ],
  % [ T = list_to_pid(X) || X <- Receivers ],
  request("n", {message_send, Channel, "String"}),

  T = hd(Receivers),
  N = list_to_pid(T),
  N ! {message_receive, Channel, "Nick", "Msg"}.

handle_messages(State, Data) ->
  io:fwrite("\nHANDLE_MESSAGES:\nState: ~p\nData: ~p\n", [State, Data]),
  case Data of
    {message_send, Msg} ->
      send_to_all(State#channelState.users, State#channelState.name),

      {reply, message_receive, State};
    {join, Pid} ->
      NewState = State#channelState{users = [ Pid | State#channelState.users ]},
      io:fwrite("New state: ~p\n", [NewState]),
      {reply, join, NewState}
  end.
