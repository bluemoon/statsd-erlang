-module(statsd).
-author("Dominik Liebler").

-export([start/2, start/1, start/0, stop/1]).
-export([increment/2, increment/3, decrement/2, decrement/3, count/3, count/4, timing/3, timing/4]).

-define(STATSD_DEFAULT_PORT, 8125).
-define(STATSD_DEFAULT_HOST, "localhost").

%% holds all the relevant state that must be passed to all functions
-record(state, { 
	port = ?STATSD_DEFAULT_PORT,
	host = ?STATSD_DEFAULT_HOST,
	socket
}).

%% Public: opens the socket
%%
%% returns a #state record containing the socket
start(Host, Port) ->
	State = #state{port = Port, host = Host},
	{ok, Socket} = gen_udp:open(0),
	State#state{socket = Socket}.
start(Host) ->
	start(Host, ?STATSD_DEFAULT_PORT).
start() ->
	start(?STATSD_DEFAULT_HOST).

%% Public: closes the socket connection
%%
%% returns ok (hopefully)
stop(#state{socket=Socket}) ->
	gen_udp:close(Socket).

%% Public: increments a counter by 1
%% 
%% returns ok or {error, Reason}
increment(State, Key, Samplerate) when is_record(State, state) ->
	count(State, Key, 1, Samplerate).
increment(State, Key) when is_record(State, state) ->
	count(State, Key, 1).
	
%% Public: decrements a counter by 1
%% 
%% returns ok or {error, Reason}
decrement(State, Key, Samplerate) when is_record(State, state) ->
	count(State, Key, -1, Samplerate).
decrement(State, Key) when is_record(State, state) ->
	count(State, Key, -1).

%% Public: increments a counter by an arbitrary integer value
%%
%% returns: ok or {error, Reason}
count(State, Key, Value) when is_record(State, state) ->
	send(State, {message, Key, Value, c}).
count(State, Key, Value, Samplerate) when is_record(State, state) ->
	send(State, {message, Key, Value, c, Samplerate}).

%% Public: sends a timing in ms
%%
%% returns: ok or {error, Reason}
timing(State, Key, Value) when is_record(State, state) ->
	send(State, {message, Key, Value, ms}).
timing(State, Key, Value, Samplerate) when is_record(State, state) ->
	send(State, {message, Key, Value, ms, Samplerate}).
	
%% Internal: prepares and sends the messages
%%
%% returns: ok or {error, Reason}
send(State, Message) when is_record(State, state) ->
	send_message(State, build_message(Message)).

%% Internal: builds the message string to be sent
%% 
%% returns: a String	
build_message({message, Key, Value, Type}) ->
	[Key, ":", Value, "|", Type];
build_message({message, Key, Value, Type, Samplerate}) ->
	[build_message({message, Key, Value, Type}), "@", io:format("~.2f", 1.0 / Samplerate)].
		
%% Internal: sends the message over a UDP socket
%% 
%% returns: 
send_message(State, Message) when is_record(State, state) ->
	io:format("sending message ~p~n", [Message]),
	gen_udp:send(State#state.socket, State#state.host, State#state.port, Message).
