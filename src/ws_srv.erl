%! /usr/bin/env escript
%
% main -> listen -> accept -> wait (handshake) -> loop (protocol encapsulation)
%                   -> spawn        -> spawn　
%                   -> accept       -> interact (client)

-module(ws_srv).

-export([start_link/0, main/1]).

-mode(compile).

start_link() ->
    Pid = spawn_link(fun() -> start(fun interact/2, 0) end),
    io:format("~p spawned ~p~n", [self(), Pid]),
    {ok, Pid}.

main(_Args) ->
    start(fun interact/2, 0).

interact(Browser, State) ->
    receive
        {browser, Browser, Str} ->
            Str1 = lists:reverse(unicode:characters_to_list(Str)),
            Msg = unicode:characters_to_binary("out ! " ++ Str1),
            io:format("send: ~ts~n", [Msg]),
            Browser ! {send, Msg},
            interact(Browser, State)
    after 100 ->
        Msg = list_to_binary("clock ! tick " ++ integer_to_list(State)),
        Browser ! {send, Msg},
        interact(Browser, State + 1)
    end.

start(F, State0) ->
    Port = 5000, % list_to_integer(os:getenv("PORT")),
    io:format("listen on ~p~n", [Port]),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    par_connect(Listen, F, State0).

par_connect(Listen, F, State0) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen, F, State0) end),
    wait(Socket, F, State0).

wait(Socket, F, State0) ->
    receive
        {tcp, Socket, Data} ->
            Lines = binary:split(Data, <<"\r\n">>, [global]),
            List = [list_to_tuple(binary:split(Line, <<": ">>)) || Line <- Lines, binary:match(Line, <<": ">>) /= nomatch],
            % parse in text mode
            % List = [list_to_tuple(re:split(Line, ": ", [{return, list}])) || Line <- string:tokens(Data, "\r\n"), string:str(Line, ": ") > 0],
            {_, Key} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, List),
            io:format("Key ~p~n", [Key]),
            Sha1 = crypto:sha([Key, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
            Base64 = base64:encode(Sha1),
            Handshake = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
            gen_tcp:send(Socket, Handshake),
            S = self(),
            Pid = spawn_link(fun() -> F(S, State0) end),
            loop(Socket, Pid);
        Any ->
            io:format("Received: ~p~n", [Any]),
            wait(Socket, F, State0)
    end.

loop(Socket, Pid) ->
    receive
        {tcp, Socket, Data} ->
            handle_data(Data, Socket, Pid);
        {tcp_closed, Socket} ->
            Pid ! {browser_closed, self()};
        {send, Data} ->
            % len >= 126 is not handled
            % fin: yes, _rsv, op: text, mask: no, size, data
            Frame = <<1:1, 0:3, 1:4, 0:1, (size(Data)):7, Data/binary>>,
            % io:format("send ~p~n", [Frame]),
            gen_tcp:send(Socket, Frame),
            loop(Socket, Pid);
        Any ->
            io:format("Received:~p~n", [Any]),
            loop(Socket, Pid)
    end.

unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).

unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    % io:format("acc ~p rest ~w~n", [Acc, Payload]),
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.

handle_data(Data, Socket, Pid) ->
    <<Fin:1, _Rsv:3, Opcode:4, Mask:1, Len:7, Rest/binary>> = Data,
    % len >= 126 is not handled
    <<Masking:4/binary, Payload:Len/binary, Next/binary>> = Rest,
    io:format("fin ~p op ~p len ~p masked ~p key ~w data ~w~n", [Fin, Opcode, Len, Mask, Masking, Payload]),
    Line = unmask(Payload, Masking),
    io:format("unmask ~ts ~w~n", [Line, Line]),
    Pid ! {browser, self(), Line},
    case size(Next) of
        0 -> loop(Socket, Pid);
        _Other -> handle_data(Next, Socket, Pid)
    end.

