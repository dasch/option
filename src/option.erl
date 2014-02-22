-module(option).
-export([default/2, select/1, value/1]).

default({ok, Value}, _Default) -> Value;
default(_None, Default)        -> Default.

select([{ok, Value} | Rest]) -> [Value | select(Rest)];
select([_None | Rest])       -> select(Rest);
select([])                   -> [].

value({ok, Value}) -> Value;
value(None)        -> error({none, None}).
