-module(client).
-export([handle/2, initial_state/3]).

% We chose not to add anything to the client state. Discussed if the client
% should hold a list of all the channels it's connected to but we figured
% this would be redundant. The check that the user is connected to a channel
% which it wish to send a message to is done in the server.
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
%   Sends a request to join a channel, and handles the possible responses:
%   - join : Successful joining a channel
%   - error : User has already joined the channel and cannot join again
%   - EXIT : Exception thrown when the server does not respond for some reason
handle(St, {join, Channel}) ->
    Response = (catch genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
        join -> {reply, ok, St};
        error -> {reply, {error, user_already_joined, "User already joined"}, St}
    end;

% Leave channel
%   Sends a request to leave a channel. All in all, works almost exactly in the
%   same way as when joining a channel, except for the occasional difference in
%   error message or request parameters.
handle(St, {leave, Channel}) ->
    Response = (catch genserver:request(St#client_st.server, {leave, Channel, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
        leave -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% Sending message (from GUI, to channel)
%   Sends a message to a channel directly. As above, works pretty much the same:
%   either the sending of message is successful, or the user is not a member of
%   the channel, or there is a problem with the channel not responding.
handle(St, {message_send, Channel, Text}) ->
    Response = (catch genserver:request(list_to_atom(Channel), {message_send, St#client_st.nick, Text, self()})),

    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Channel does not respond"}, St};
        message_send -> {reply, ok, St};
        error -> {reply, {error, user_not_joined, "User has not joined that channel"}, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick
%   We decided to check for unique nicks, which is accomplished by checking with
%   the server if the chosen nick is occupied or not. As in the other handle/2
%   functions, we check if the changing of nick was successful, or if it was
%   occupied, or if the server is down or having difficulties.
handle(St, {nick, NewNick}) ->
    Response = (catch genserver:request(St#client_st.server, {nick, NewNick})),
    case Response of
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server does not respond"}, St};
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
    catch(genserver:request(St#client_st.server, {stop, St#client_st.server})),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
% Did an _Data instead of Data to supress compiler warning.
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
