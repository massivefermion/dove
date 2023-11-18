-module(dove_ffi).

-export([decode_status_line/1, decode_header/1, gunzip/1, inflate/1]).

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
            {ok, {header, {sanitize(Name), Value}, Rest}};
        {ok, http_eoh, Rest} ->
            {ok, {eoh, Rest}};
        {ok, _, _} ->
            {error, nil};
        {more, _} ->
            {ok, more};
        {error, _} ->
            {error, nil}
    end.

gunzip(Compressed) ->
    try
        {ok, zlib:gunzip(Compressed)}
    catch
        error:Reason -> {error, sanitize(Reason)}
    end.

inflate(Compressed) ->
    Z = zlib:open(),
    try
        ok = zlib:inflateInit(Z),
        [Decompressed] = zlib:inflate(Z, Compressed, [{exception_on_need_dict, false}]),
        {ok, Decompressed}
    catch
        error:Reason -> {error, sanitize(Reason)}
    after
        try
            zlib:inflateEnd(Z)
        catch
            error:FailedEndReason -> {error, sanitize(FailedEndReason)}
        end
    end.

sanitize(Name) when is_atom(Name) ->
    sanitize(atom_to_binary(Name));
sanitize(Name) when is_binary(Name) ->
    string:lowercase(Name).
