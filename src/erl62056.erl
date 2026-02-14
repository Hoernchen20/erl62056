-module(erl62056).
-behaviour(gen_statem).

-include("erl62056.hrl").

%% API
-export([
    start_link/2,
    stop/1,
    send_message/2
]).

%% gen_statem callbacks
-export([
    init/1,
    terminate/3,
    code_change/4,
    callback_mode/0,
    handle_event/4
]).

%% Constants

%% Types
-type send_message_ret() ::
    identification_message() | data_message() | {error, not_connected | request_pending | timeout}.

%% Records
-record(data, {
    host :: inet:socket_address() | inet:hostname(),
    port :: inet:port_number(),
    socket :: gen_tcp:socket() | undefined,
    buffer = <<>> :: binary(),
    reply_pid = undefined :: pid() | undefined,
    base_reconnect_ms = 1_000 :: non_neg_integer(),
    reconnect_ms = 1_000 :: non_neg_integer(),
    max_reconnect_ms = 30_000 :: non_neg_integer(),
    request_timeout_ms = 5_000 :: non_neg_integer()
}).

%%-------------------------------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------------------------------
-spec start_link(Host, Port) -> gen_statem:start_ret() when
    Host :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number().
start_link(Host, Port) ->
    gen_statem:start_link(?MODULE, {Host, Port}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

-spec send_message(Pid, Msg) -> send_message_ret() when
    Pid :: pid(),
    Msg :: request_message() | ack_option_select_message().
send_message(Pid, Msg) ->
    gen_statem:call(Pid, {send_msg, Msg}).

%%-------------------------------------------------------------------------------------------------
%% gen_statem callbacks
%%-------------------------------------------------------------------------------------------------
-spec init({Host, Port}) -> {ok, disconnected, Data} when
    Host :: inet:socket_address() | inet:hostname(),
    Port :: inet:port_number(),
    Data :: #data{}.
init({Host, Port}) ->
    Data = #data{
        host = Host,
        port = Port
    },
    {ok, disconnected, Data}.

%TODO spec
terminate(_Reason, _State, #data{socket = undefined}) ->
    ok;
terminate(_Reason, _State, #data{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

%TODO spec
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%TODO spec
callback_mode() ->
    [handle_event_function, state_enter].

%%-------------------------------------------------------------------------------------------------
%% State functions
%%-------------------------------------------------------------------------------------------------
%% disconnected state
handle_event(enter, _OldState, disconnected, #data{reconnect_ms = ReconnectMs} = Data) ->
    CleanData = Data#data{socket = undefined, buffer = <<>>, reply_pid = undefined},
    {keep_state, CleanData, {state_timeout, ReconnectMs, do_connect}};
handle_event(state_timeout, do_connect, disconnected, Data) ->
    #data{
        host = Host,
        port = Port,
        reconnect_ms = ReconnectMs,
        max_reconnect_ms = MaxReconnectMs,
        base_reconnect_ms = BaseReconnectMs
    } = Data,
    case gen_tcp:connect(Host, Port, [binary, {active, once}]) of
        {ok, Socket} ->
            NewData = Data#data{socket = Socket, reconnect_ms = BaseReconnectMs},
            {next_state, connected, NewData};
        {error, _Reason} ->
            NewReconnectMs = min(ReconnectMs * 2, MaxReconnectMs),
            NewData = Data#data{reconnect_ms = NewReconnectMs},
            {keep_state, NewData, {state_timeout, NewReconnectMs, do_connect}}
    end;
handle_event({call, From}, _EventData, disconnected, _Data) ->
    {keep_state_and_data, {reply, From, {error, not_connected}}};
%%-------------------------------------------------------------------------------------------------
%% connected state
handle_event({call, From}, {send_msg, Msg}, connected, #data{socket = Socket} = Data) ->
    Packet = erl62056_parser:encode(Msg),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            NewData = Data#data{reply_pid = From},
            {next_state, request_pending, NewData};
        {error, Reason} ->
            {next_state, disconnected, Data, {reply, From, {error, Reason}}}
    end;
%%-------------------------------------------------------------------------------------------------
%% request pending state
handle_event(enter, connected, request_pending, #data{request_timeout_ms = RequestTimeoutMs}) ->
    {keep_state_and_data, {state_timeout, RequestTimeoutMs, request_timeout}};
handle_event(internal, Message, request_pending, #data{reply_pid = From} = Data) ->
    NewData = Data#data{reply_pid = undefined},
    {next_state, connected, NewData, {reply, From, Message}};
handle_event(state_timeout, request_timeout, request_pending, #data{reply_pid = From} = Data) ->
    NewData = Data#data{reply_pid = undefined},
    {next_state, connected, NewData, {reply, From, {error, timeout}}};
handle_event({call, From}, _EventData, request_pending, _Data) ->
    {keep_state_and_data, {reply, From, {error, request_pending}}};

%%-------------------------------------------------------------------------------------------------
%% handle enter event for all states which have no explicite enter event
handle_event(enter, _OldState, _CurrentState, _Data) ->
    keep_state_and_data;

%%-------------------------------------------------------------------------------------------------
%% handle tcp related messages for all states
handle_event(info, {tcp, Socket, Packet}, _State, #data{socket = Socket, buffer = Buffer} = Data) ->
    % TODO handle tcp data
    io:format("Received raw TCP: ~p~n", [Packet]),
    inet:setopts(Socket, [{active, once}]),
    NewBuffer = <<Buffer/binary, Packet/binary>>,
    {Action, Rest} = decode_packet(NewBuffer),
    NewData = Data#data{buffer = Rest},
    {keep_state, NewData, Action};
handle_event(info, {tcp_closed, Socket}, _State, #data{socket = Socket} = Data) ->
    io:format("tcp connection closed", []),
    {next_state, disconnected, Data}.

%%-------------------------------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------------------------------
decode_packet(Packet) ->
    case erl62056_parser:decode(Packet) of
        {ok, Message, Rest} ->
            Action = {next_event, internal, Message},
            {Action, Rest};
        {error, Reason, Rest} ->
            io:format("Failed to decode packet: ~p~n", [Reason]),
            {[], Rest}
    end.
