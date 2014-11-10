-module(rudymproved_2).
-export([start/0,start/1]).

% Start on port 8080 by default.
start() ->
    start(8080).

% Start the webserver (in a different process just so you can still use the erlang shell)
start(Port) ->
    spawn(fun() ->
        % Start listening on the port.
        case gen_tcp:listen(Port, [{active, false},{reuseaddr, true}]) of 
            {ok, Listen} -> loop(Listen) ;
            {error, E} -> {error,E}
        end
    end).

% Loop for accepting all connexions on the port we listen to.
loop(Listen) ->
    % Accept new incomming connexion.
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            % Handle the newly created socket in a different process.
            Handler = spawn(fun () -> enter_handle(Sock) end),
            % Bind the events on the socket to the process
            gen_tcp:controlling_process(Sock, Handler),
            Handler ! under_control,
            % Keep waiting for new connexions.
            loop(Listen);
        {error, E} -> {error,E}
    end.


% Make sure controlling_process has been called, to avoid a race condition.
% See links below :
%http://stackoverflow.com/questions/11409656/erlang-avoiding-race-condition-with-gen-tcpcontrolling-process
%http://stackoverflow.com/questions/22650774/erlang-to-controlling-process-or-not-to-controlling-process
%http://fullof.bs/a-better-erlang-tcp-listening-pattern-addressingthe-fast-packet-loss-problem/
%http://stackoverflow.com/questions/7828204/erlang-socket-doesnt-receive-until-the-second-setopts-active-once

enter_handle(Sock) ->
    receive
        under_control -> ok
    end,
    handle(Sock).


handle(Sock) ->
    % set soscket options to receive messages directly into itself
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
             % Parse the request for the method and the resource requested.
            [Method, Resource | _ ] = string:tokens(Data," "),
            % Construct the path.
            Path = [".",Resource],
            % We will only serve GET requests.
            case Method of
                "GET" -> Response = serve_get(Path);
                % And return 501 for the rest.
                _ -> Response = response("501 Not Implemented", "501 Not Implemented")
            end,
            % Send the response over the socket
            gen_tcp:send(Sock, Response),
            gen_tcp:close(Sock);
        {tcp_closed, Socket} ->
            io:format("Socket ~p closed~n", [Socket]);
        {tcp_error, Socket, Reason} ->
            io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.

serve_get(Path) ->
    timer:sleep(40),
    response("404 Not Found", "404 Not Found").

% serve_get(Path) ->
%     % Read the requested file.
%     case file:read_file(Path) of
%         {ok, Data} -> Response = response("200 OK", Data) ;
%         {error, _} -> Response = response("404 Not Found", "404 Not Found")
%     end,
%     Response.

response(Code, Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
        io_lib:fwrite(
            "HTTP/1.0 ~s\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
            [Code, size(B), B]
        )
    ).
