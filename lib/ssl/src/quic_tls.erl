-module(quic_tls).

-export([connect/4, send/2, setopts/2]).


connect(_Address, _Port,  _SocketOpts, _Timeout) ->
   {ok, self()}.

send(_Socket, _Data) ->
    ok.

setopts(_Socket, _Options) ->
    ok.
