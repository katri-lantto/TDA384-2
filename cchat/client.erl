-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    Msg = genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()}),

    case Msg of
      join -> {reply, ok, St};
      error -> {reply, {error, user_already_joined, "User already joined"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    Msg = genserver:request(St#client_st.server, {leave, Channel, self()}),

    case Msg of
        leave -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Text}) ->
    Nick = St#client_st.nick,
    Msg = genserver:request(St#client_st.server, {message_send, Channel, Nick, Text, self()}),

    case Msg of
        message_send -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Msg = genserver:request(St#client_st.server, {nick, NewNick}),
    case Msg of
      error -> {reply, {error, nick_taken, "Nick already taken"}, St};
      ok -> {reply, ok, St#client_st{nick = NewNick}}
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    genserver:request(St#client_st.server, {stop, St#client_st.server}),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
