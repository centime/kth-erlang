-module(rudymproved).
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
                  Handler = spawn(fun () -> handle(Sock) end),
                  % Keep waiting for new connexions.
                  loop(Listen);
            {error, E} -> {error,E}
      end.

handle(Sock) ->
    % Read the socket.
    case gen_tcp:recv(Sock, 0) of
          {ok, Bin} ->
                % Parse the request for the method and the resource requested.
                [Method, Resource | _ ] = string:tokens(Bin," "),
                % Construct the path.
                Path = [".",Resource],
                % We will only serve GET requests.
                case Method of
                      "GET" -> Response = serve_get(Path);
                      % And return 501 for the rest.
                      _ -> Response = response("501 Not Implemented",Method++" Method Not Implemented")
                end,
                % Send the response over the socket
                gen_tcp:send(Sock, Response),
                gen_tcp:close(Sock);
          {error, E} -> {error,E}
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
         [Code, size(B), B])
    ).
