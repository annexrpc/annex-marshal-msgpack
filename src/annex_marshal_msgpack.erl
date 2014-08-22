-module(annex_marshal_msgpack).

-export([encode/5]).
-export([decode/1]).

-define(REQ_ENUM(Type), case Type of
                          call -> 0;
                          cast -> 1
                        end).

-define(OPTIONS, [
  {allow_atom, pack},
  {format, map}
]).

%% [type, msgid, method, params]
encode(MsgID, Type, Module, Function, Arguments) ->
  msgpack:pack([?REQ_ENUM(Type), MsgID, Module, Function, Arguments], ?OPTIONS).

%% [type, msgid, result]
decode(Bin) ->
  case msgpack:unpack(Bin, [{format, map}]) of
    {error, _} = Error ->
      Error;
    {ok, [0, MsgID, Result]} ->
      {ok, MsgID, Result};
    {ok, [1, MsgID, Error]} ->
      {error, MsgID, Error};
    {ok, Other} ->
      {error, {unexpected_response, Other}}
  end.
