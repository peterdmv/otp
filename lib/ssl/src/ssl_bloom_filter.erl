-module(ssl_bloom_filter).

-export([add_elem/2,
         contains/2,
         delete/1,
         new/3]).

-define(E, 2.718281828459045).
-define(DEFAULT_TABLE, ssl_bloom_filter).
-define(NAME, server).

%%--------------------------------------------------------------------
%% API ---------------------------------------------------------------
%%--------------------------------------------------------------------

%% Create new Bloom Filter with k hashes, m bits in the filter
new(Name, K, M) ->
    process_flag(trap_exit, true),
    Size = round(math:ceil(M / 8)),
    BitField = binary:copy(<<0>>, Size),
    case ets:whereis(?DEFAULT_TABLE) of
	undefined ->
	    ets:new(?DEFAULT_TABLE, [public, named_table, ordered_set]),
	    ets:insert(?DEFAULT_TABLE, {Name, K, 0, M, 0, BitField});
	_ ->
	    already_exists
    end.

%% Delete Bloom Filter
delete(Name) ->
    case ets:whereis(Name) of
	undefined ->
	    no_bloom_filter;
	_ ->
	    ets:delete(bloom_filter, Name)
    end.


%% Add new element to Bloom Filter
add_elem(Name, Elem) ->
    case ets:whereis(?DEFAULT_TABLE) of
	undefined ->
	    not_initialized;
	_ ->
	    [{_, K, N, M, _, BitField0}] = ets:lookup(?DEFAULT_TABLE,Name),
	    Hash = hash(Elem, K, M),
	    BitField = set_bits(BitField0, Hash),
	    P = false_positive(K, N + 1, M),
	    ets:insert(?DEFAULT_TABLE, {Name, K, N + 1, M, P, BitField})
    end.


%% Check if Bloom Filter contains element.
%% Returns a tuple {boolean(), <probability of false positive>}
contains(Name, Elem) ->
    case ets:whereis(?DEFAULT_TABLE) of
	undefined ->
	    not_initialized;
	_ ->
	    case ets:lookup(?DEFAULT_TABLE, Name) of
		[] ->
		    bloom_filter_not_exists;
		[{_, K, _, M, P, BitField}] ->
		    Hash = hash(Elem, K, M),
		    Fun = fun (Pos) -> bit_is_set(BitField, Pos) end,
		    {lists:all(Fun, Hash), P}
	    end
    end.


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
bit_is_set(<<1:1,_/bitstring>>, 0) ->
    true;
bit_is_set(BitField, N) ->
    case BitField of
	<<_:N,1:1,_/bitstring>> ->
	    true;
	_ ->
	    false
    end.


set_bits(BitField, []) ->
    BitField;
set_bits(BitField, [H|T]) ->
    set_bits(set_bit(BitField, H), T).
    

set_bit(BitField, 0) ->
    <<_:1,Rest/bitstring>> = BitField,
    <<1:1,Rest/bitstring>>;
set_bit(BitField, B) ->
    <<Front:B,_:1,Rest/bitstring>>  = BitField,
    <<Front:B,1:1,Rest/bitstring>>.
    
   
%% Bloom filter with k hashes, m bits in the filter, and n elements 
%% that have been inserted.
false_positive(K, N, M) ->
    Exp = - K * N / M,
    P0 = 1 - math:pow(?E, Exp),
    math:pow(P0, K).


%% Kirsch-Mitzenmacher-Optimization 
hash(Elem, K, M) ->
    hash(Elem, K, M, []).
%%
hash(_, 0, _, Acc) ->
    Acc;
hash(Elem, K, M, Acc) ->
    H = (erlang:phash2({Elem, 0}, M) + (K - 1) * erlang:phash2({Elem, 1}, M)) rem M,
    hash(Elem, K - 1, M, [H|Acc]).
