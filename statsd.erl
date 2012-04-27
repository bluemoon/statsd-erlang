-module(statsd).
-export([increment/1, increment/2, decrement/1, decrement/2, count/2, count/3, timing/2, timing/3]).

%% Public: increments a counter by 1
%% 
%% returns ok or {error, Reason}
increment(Key, Samplerate) ->
	count(Key, 1, Samplerate).
increment(Key) ->
	count(Key, 1).
	
%% Public: decrements a counter by 1
%% 
%% returns ok or {error, Reason}
decrement(Key, Samplerate) ->
	count(Key, -1, Samplerate).
decrement(Key) ->
	count(Key, -1).

%% Public: increments a counter by an arbitrary integer value
%%
%% returns: ok or {error, Reason}
count(Key, Value) ->
	send({message, Key, Value, c}).
count(Key, Value, Samplerate) ->
	send({message, Key, Value, c, Samplerate}).

%% Public: sends a timing in ms
%%
%% returns: ok or {error, Reason}
timing(Key, Value) ->
	send({message, Key, Value, ms}).
timing(Key, Value, Samplerate) ->
	send({message, Key, Value, ms, Samplerate}).
	
%% Internal: prepares and sends the messages
%%
%% returns: ok or {error, Reason}
send(MT) ->
	Message = build_message(MT),
	send_message(Message).

%% Internal: builds the message string to be sent
%% 
%% returns: a String	
build_message({message, Key, Value, Type}) ->
	Key ++ ":" ++ integer_to_list(Value) ++ "|" ++ atom_to_list(Type);
build_message({message, Key, Value, Type, Samplerate}) ->
	build_message({message, Key, Value, Type}) ++ "@" ++ float_to_list(1 / Samplerate).
	
%% Internal: sends the message over a UDP socket
%% 
%% returns: 	
send_message(Message) ->
	io:format("sending message ~p~n", [Message]),
	
	{ok, Socket} = gen_udp:open(0),
	ok = gen_udp:send(Socket, "localhost", 8125, Message),
	gen_udp:close(Socket).