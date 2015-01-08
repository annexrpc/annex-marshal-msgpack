-module(annex_marshal_msgpack).

-export([encode/3]).
-export([encode/5]).
-export([decode/1]).

-define(REQ_ENUM(Type), case Type of
                          call -> 0;
                          cast -> 1
                        end).

-define(RES_ENUM(Type), case Type of
                          response -> 0;
                          error -> 1
                        end).

-define(OPTIONS, [
  {allow_atom, pack},
  {format, map}
]).

%% [type, msgid, module, function, params]
encode(MsgID, Type, Response) ->
  msgpack:pack([?RES_ENUM(Type), MsgID, Response], ?OPTIONS).

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
    {ok, [0, MsgID, Module, Function, Arguments]} ->
      {call, MsgID, Module, Function, Arguments};
    {ok, [1, MsgID, Module, Function, Arguments]} ->
      {cast, MsgID, Module, Function, Arguments};
    {ok, Other} ->
      {error, {unexpected_response, Other}}
  end.
