-module(pearl_ffi).

-export([strip_prefix/2]).

strip_prefix(String, Prefix) ->
    Prefix_size = byte_size(Prefix),

    case Prefix == binary_part(String, 0, Prefix_size) of
        true ->
            String_size = byte_size(String),
            {ok, binary_part(String, Prefix_size, String_size - Prefix_size)};
        false ->
            {error, nil}
    end.
