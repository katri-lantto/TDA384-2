-module(server).
-export([start/1,stop/1]).

% Comments about our choice of state
% The servrer holds the state of all channels created and all the nicks
% that are connected to that server. We store all the nicks here to make
% sure that there cannot be two users with the same nick.
% When a user wants to connect to a channel this request goes trough the
% server that first checks if there already is a process for that channel
% and if it isn't the channel is created.
% In the channel state we hold the name of the channel and which users.
% The reason that the channel holds its own name is that we want to send
% this to the client (which it then passed down to the GUI).
% We could solve this by sending the name of the channel in the message_send
% method in the client but with figured this was a more elegant solution.

-record(serverState, {
    channels = [],
    nicks = []
}).

-record(channelState, {
    name,
    users = []
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
    % Check if the channel already exists. If it does, we send a message
    % to the channel that we want to add the user. If it doesn't exist
    % we spawn a new process for that channel and add the user to it in
    % its initial state. Rest of it should be self-explanatory.
    {join, Channel, Nick, Sender} ->
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if ChannelExists ->
          Response = (catch(genserver:request(list_to_atom(Channel), {join, Sender}))),

          case Response of
            error -> {reply, error, State};
            join ->
              % Added a method for adding the nick to the channels state if it's
              % not already there. This was done to prevent a user to change the
              % nick to somebodys initial nick (which is checked in tests).
              NickExists = lists:member(Nick, State#serverState.nicks),
              if NickExists ->
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

    % Called when a user leaves the channel.
    % If the user is in the channel it leaves it, otherwise return an error.
    {leave, Channel, Sender} ->
      ChannelExists = lists:member(Channel, State#serverState.channels),
      if ChannelExists ->
          Response = (catch(genserver:request(list_to_atom(Channel), {leave, Sender}))),

          case Response of
            leave -> {reply, leave, State};
            error -> {reply, error, State}
          end;
        true ->
          {reply, error, State}
      end;

    % Checking if the nick is available. If it is it's added to the server state
    % and an ok is returned to the client, otherwise error.
    {nick, Nick} ->
      NickExists = lists:member(Nick, State#serverState.nicks),
      if NickExists ->
          {reply, error, State};
        true ->
          NewState = State#serverState{nicks = [ Nick | State#serverState.nicks ]},
          { reply, ok, NewState }
      end;

    % Stopping all channels if the server is stopped. This is not used since its
    % not expected to work this way in the tests, just left it here for reference.
    stop ->
      [ genserver:stop(list_to_atom(Channel)) || Channel <- State#serverState.channels ]
  end.

% Function for handling requests to a channel-process.
% Handles joins, leaves and message sending.
handle_channel(State, Data) ->
  case Data of
    % Joining a channel, if the user is already joined an error is returned.
    {join, NewPid} ->
      IsMember = lists:member(NewPid, State#channelState.users),
      if IsMember ->
        {reply, error, State};
      true ->
        {reply, join, State#channelState{users = [ NewPid | State#channelState.users ]}}
      end;

    % User wants to leave the channel. Respond with an error if user is not in channel.
    {leave, Sender} ->
      IsMember = lists:member(Sender, State#channelState.users),
      if IsMember ->
          NewUsers = lists:delete(Sender, State#channelState.users),
          {reply, leave, State#channelState{users = NewUsers}};
        true ->
          {reply, error, State}
      end;

    % Sending a message to all clients connected to the channel.
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
