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
    % TODO: Implement this function
    % {reply, ok, St} ;
    % St#client_st.server ! "Channel",
    % io:fwrite("My server is ~p\n", [St#client_st.server]),
    St#client_st.server ! {request, self(), make_ref(), {join, Channel, self()}},
    receive
      X -> io:fwrite("Got back: ~p\n", [X])
    end,

    % {reply, {error, not_implemented, "join not implemented"}, St} ;
    {reply, ok, St};

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    % {reply, {error, not_implemented, "leave not implemented"}, St} ;
    {reply, ok, St} ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    io:fwrite("message send ~p ~p\n", [Channel, Msg]),
    % St#client_st.gui ! {message_receive, Channel, St#client_st.nick, Msg},
    % St#client_st.gui ! {message_receive, Channel, Msg},
    % server:handle(message_send, Channel, Msg),
    St#client_st.server ! {request, self(), make_ref(), {message_send, Channel, Msg, self()}},

    % receive
    %   X -> io:fwrite("Got back in message_send: ~p\n", [X])
    % end,


    % St#client_st.gui ! {message_receive, Channel, Msg},
    % gen_server:call(St#client_st.gui, {message_receive, Channel, "Hej"++"> "++Msg}),

    % {message_send, Channel, Msg},
    % {reply, {error, not_implemented, "message sending not implemented"}, St} ;
    {reply, ok, St};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:fwrite("\nMESSAGE RECIEVED ~p ~p ~p\n\n", [Channel, Nick, Msg]),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
