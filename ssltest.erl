-module(ssltest).

-compile(export_all).
-include("lib/ssl/src/ssl_dh_groups.hrl").
-define(PORT, 11032).


server2() ->
    application:load(ssl),
    logger:set_application_level(ssl, debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "server.pem"},
             {keyfile, "server.key"},
             {versions, ['tlsv1.2','tlsv1.3']},
             {log_level, debug}
            ],
    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    {ok, _} = ssl:handshake(CSock).

client2() ->
    application:load(ssl),
    logger:set_application_level(ssl, debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {cacertfile, "ca.pem"},
             {versions, ['tlsv1.2','tlsv1.3']},
             {log_level, debug}
            ],
    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.



server() ->
    %% logger:set_primary_config(level,debug),
    application:load(ssl),
    %% ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "server.pem"}, {keyfile, "server.key"},{versions, ['tlsv1.1','tlsv1.2']}
%% ,{log_level, debug}
            ,{ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
],
    %% LOpts = [{certfile, "server.pem"}, {keyfile, "server.key"},{log_level, debug}],

    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    {ok, _} = ssl:handshake(CSock),
    CSock.

client() ->
    %% logger:set_primary_config(level,debug),
    application:load(ssl),
    %% ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer}, {cacertfile, "ca.pem"},{versions, ['tlsv1.2']}
%% ,{log_level, debug}
            ,{ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            %% ,{signature_algs, [{sha,rsa}]}
            %% ,{signature_algs_cert, [rsa_pkcs1_sha256,ecdsa_sha1]}
            %% ,{signature_algs_cert, [ecdsa_sha1]}
],
    %% COpts = [{verify, verify_peer}, {cacertfile, "ca.pem"},{versions, ['tlsv1.3']},{log_level, debug}],

    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.

server_hs() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "server.pem"}, {keyfile, "server.key"},{versions, ['tlsv1.3']},{log_alert, false},{handshake,hello}],
    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    S=ssl:handshake(CSock),
    S.

ec_client() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {cacertfile, "ecc-ca.pem"},
             {versions, ['tlsv1.2']},
             {log_level, debug}
             %% {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            ],
    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.

ec_server() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "ecc-server.pem"},
             {keyfile, "ecc-server.key"},
             {versions, ['tlsv1.2']},
             {log_level, debug},
             {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            ],

    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    {ok, _} = ssl:handshake(CSock),
    CSock.

ec2_client() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {cacertfile, "ecc-ca.pem"},
             {versions, ['tlsv1.2']},
             {log_level, debug},
             {eccs ,[secp256r1,secp521r1,secp384r1]},
              {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            ],
    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.


ec2_client_cipher_order() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {cacertfile, "ecc-ca.pem"},
             {versions, ['tlsv1.2']},
             {log_level, debug},
             {eccs ,[secp256r1,secp521r1,secp384r1]},
             {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}, {ecdhe_ecdsa,aes_256_cbc,sha}]}
            ],
    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.

ec2_server() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "ecc2-server.pem"},
             {keyfile, "ecc2-server.key"},
             {versions, ['tlsv1.2']},
             {log_level, debug}
             %% {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            ],

    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    ok = ssl:handshake(CSock),
    CSock.

ec3_server() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "ecc3-server.pem"},
             {keyfile, "ecc3-server.key"},
             {versions, ['tlsv1.2']},
             {log_level, debug}
             %% {ciphers,[{ecdhe_rsa,aes_256_cbc,sha}]}
            ],

    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    {ok, _} = ssl:handshake(CSock),
    CSock.





debug() ->
    dbg:tracer(),
    dbg:p(all,c),

    %%dbg:tpl(tls_handshake, get_tls_handshake_aux, x).
    %%dbg:tpl(tls_connection, hello, x).
    %%dbg:tpl(logger_ssl_formatter, format, x).
    %%dbg:tpl(tls_connection, next_record, x),
    %%dbg:tpl(tls_connection, next_event, x).
    %dbg:tpl(string, pad, x).
    %%dbg:tpl(logger_ssl_formatter, calculate_padding, x).
    %%dbg:tpl(ssl_connection, certify, x).
    %% dbg:tpl(ssl_connection, client_certify_and_key_exchange, x).    % NOK
    %% dbg:tpl(ssl_connection, do_client_certify_and_key_exchange, x). % OK
    %% dbg:tpl(ssl_connection, finalize_handshake, x). % NOK
    %% dbg:tpl(ssl_connection, cipher_protocol, x). % OK
    %% dbg:tpl(ssl_record, activate_pending_connection_state, x). % OK
    %% dbg:tpl(ssl_connection, finished, x). % NOK
    %% dbg:tpl(ssl_handshake, finished, x). % OK
    %% dbg:tpl(ssl_connection, save_verify_data, x). % OK
    %% dbg:tpl(tls_connection, send_handshake, x). % NOK
    %% dbg:tpl(tls_connection, send_handshake_flight, x). % OK not called
    %% dbg:tpl(tls_connection, queue_handshake, x). % NOK
    %% dbg:tpl(tls_connection, encode_handshake, x). % NOK
    %% dbg:tpl(tls_handshake, encode_handshake, x). % OK
    %% dbg:tpl(ssl_handshake, update_handshake_history, x). % OK
    %% dbg:tpl(tls_record, encode_handshake, x). % NOK
    %% dbg:tpl(tls_record, split_bin, x). % OK not called
    %% dbg:tpl(tls_record, encode_plain_text, x). % OK
    %% dbg:tpl(tls_record, encode_iolist, x). % OK not called

    %% Misc
    %% dbg:tpl(ssl_cipher, cipher_aead, x). % not visible
    %% dbg:tpl(tls_record, do_encode_plain_text, x). % OK
    %% dbg:tpl(tls_record, encode_tls_cipher_text, x). % OK
    dbg:tpl(ssl, handle_option, x),
    dbg:tpl(ssl_handshake, encode_hello_extension, x).

    
hex(B) ->
    {A, _} = logger_ssl_formatter:convert_to_hex(tls_record, B),
    Comp = "0090 - 00 01 00 02 00 03 00 0f  00 10 00 11 00 0b 00 02    ................\n",
    logger:info(Comp ++ A).

hex2(L0) ->
    Ref = "0090 - 00 01 00 02 00 03 00 0f  00 10 00 11 00 0b 00 02    ................\n",
    L = lists:map(fun input/1, L0),
    logger:info(lists:foldl(fun hex_log/2, Ref, L)).

hex_log(Input, Acc) ->
    {A, _} = logger_ssl_formatter:convert_to_hex(tls_record, Input),
    Acc ++ A.

input(N) ->
    [<< <<X>> || X <- lists:seq(1,N) >>].


sort(L) ->
    Fun = fun ({A,_},{C,_}) when A > C -> true;
              ({A,B},{C,D}) when A =:= C, B > D -> true;
              (_,_) -> false
          end,
    lists:sort(Fun,L).
                   


serverb() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "server.pem"},
             {keyfile, "server.key"},
             {cacertfile, "ca.pem"},
             {log_level, debug},
             {verify, verify_peer},
             {verify_fun, fun ssltest:verify_fail_always/0}],

    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    ok = ssl:handshake(CSock),
    CSock.

clientb() ->
    application:load(ssl),
    ssl:set_log_level(debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {certfile, "server.pem"},
             {keyfile, "server.key"},
             {cacertfile, "ca.pem"},
             {log_level, debug},
             {verify, verify_peer},
             {verify_fun, fun ssltest:verify_fail_always/0}],

    {ok, Sock} = ssl:connect("localhost", Port, COpts),
    Sock.


verify_fail_always(_Certificate, _Event, _State) ->
    {fail, bad_certificate}.

verify_pass_always(_Certificate, _Event, State) ->    
    {valid, State}.



modp2048_prime() ->
    P = "FFFFFFFF" "FFFFFFFF" "C90FDAA2" "2168C234" "C4C6628B" "80DC1CD1"
        "29024E08" "8A67CC74" "020BBEA6" "3B139B22" "514A0879" "8E3404DD"
        "EF9519B3" "CD3A431B" "302B0A6D" "F25F1437" "4FE1356D" "6D51C245"
        "E485B576" "625E7EC6" "F44C42E9" "A637ED6B" "0BFF5CB6" "F406B7ED"
        "EE386BFB" "5A899FA5" "AE9F2411" "7C4B1FE6" "49286651" "ECE45B3D"
        "C2007CB8" "A163BF05" "98DA4836" "1C55D39A" "69163FA8" "FD24CF5F"
        "83655D23" "DCA3AD96" "1C62F356" "208552BB" "9ED52907" "7096966D"
        "670C354E" "4ABC9804" "F1746C08" "CA18217C" "32905E46" "2E36CE3B"
        "E39E772C" "180E8603" "9B2783A2" "EC07A28F" "B5C55DF0" "6F4C52C9"
        "DE2BCBF6" "95581718" "3995497C" "EA956AE5" "15D22618" "98FA0510"
        "15728E5A" "8AACAA68" "FFFFFFFF" "FFFFFFFF",
    list_to_integer(P, 16).







string_to_integer(S) ->
    list_to_integer(lists:flatten(string:replace(S, " ", "", all)),16).


%% prime_default_m() ->
%%     ?DEFAULT_DIFFIE_HELLMAN_PRIME.
    

%% prime_2048() ->
    
%% prime_2048_m() ->
%%     ?DEFAULT_DIFFIE_HELLMAN_PRIME.




ptest() ->
    modp2048_prime() =:= ?DEFAULT_DIFFIE_HELLMAN_PRIME.
    
