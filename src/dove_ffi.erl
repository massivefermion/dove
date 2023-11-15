-module(dove_ffi).

-export([decode_status_line/1, decode_header/1]).

decode_status_line(Binary) ->
    case erlang:decode_packet(http_bin, Binary, []) of
        {ok, {http_response, Version, Status, Reason}, Rest} ->
            {ok, {status_line, {Version, Status, Reason}, Rest}};
        {ok, _, _} ->
            {error, nil};
        {more, _} ->
            {ok, more};
        {error, _} ->
            {error, nil}
    end.

decode_header(Binary) ->
    case erlang:decode_packet(httph_bin, Binary, []) of
        {ok, {http_header, _, Name, _, Value}, Rest} ->
            {ok, {header, {sanitize_header_name(Name), Value}, Rest}};
        {ok, http_eoh, Rest} ->
            {ok, {eoh, Rest}};
        {ok, _, _} ->
            {error, nil};
        {more, _} ->
            {ok, more};
        {error, _} ->
            {error, nil}
    end.

sanitize_header_name(Name) when is_atom(Name) ->
    sanitize_header_name(atom_to_binary(Name));
sanitize_header_name(Name) when is_binary(Name) ->
    string:lowercase(Name).
