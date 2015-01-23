-module(annex_marshal_msgpack).

-export([call/3]).
-export([call/4]).
-export([cast/3]).
-export([cast/4]).
-export([response/2]).
-export([response/3]).
-export([error/2]).
-export([error/3]).

-export([encode/1]).
-export([decode/1]).

-define(OPTIONS, [
  {allow_atom, pack},
  {format, map}
]).

call(MsgID, Action, Arguments) ->
  encode([0, MsgID, Action, Arguments]).
call(MsgID, Action, Arguments, Meta) ->
  encode([0, MsgID, Action, Arguments, Meta]).

cast(MsgID, Action, Arguments) ->
  encode([1, MsgID, Action, Arguments]).
cast(MsgID, Action, Arguments, Meta) ->
  encode([1, MsgID, Action, Arguments, Meta]).

response(MsgID, Response) ->
  encode([2, MsgID, Response]).
response(MsgID, Response, Meta) ->
  encode([2, MsgID, Response, Meta]).

error(MsgID, Code, Message) ->
  encode([3, MsgID, Code, Message]).
error(MsgID, Code, Message, Meta) ->
  encode([3, MsgID, Code, Message, Meta]).

encode(Args) ->
  msgpack:pack(Args, ?OPTIONS).

decode(Bin) ->
  case msgpack:unpack(Bin, [{format, map}]) of
    {error, _} = Error ->
      Error;
    {ok, [0, MsgID, Action, Arguments]} ->
      {ok, {call, MsgID, Action, Arguments}};
    {ok, [0, MsgID, Action, Arguments, Meta]} ->
      {ok, {call, MsgID, Action, Arguments, Meta}};

    {ok, [1, MsgID, Action, Arguments]} ->
      {ok, {cast, MsgID, Action, Arguments}};
    {ok, [1, MsgID, Action, Arguments, Meta]} ->
      {ok, {cast, MsgID, Action, Arguments, Meta}};

    {ok, [2, MsgID, Response]} ->
      {ok, {response, MsgID, Response}};
    {ok, [2, MsgID, Response, Meta]} ->
      {ok, {response, MsgID, Response, Meta}};

    {ok, [3, MsgID, Code, Message]} ->
      {ok, {error, MsgID, Code, Message}};
    {ok, [3, MsgID, Code, Message, Meta]} ->
      {ok, {error, MsgID, Code, Message, Meta}};

    {ok, Other} ->
      {error, {unexpected_response, Other}}
  end.
