% Allows dealing with variables that may or may not have a value. An "option"
% is a value that is either "ok" or "none". Only the former has a value. Since
% some functions may or may not return a value, it can be useful to have a set
% of functions that work on these options.
-module(option).

% Public API.
-export([default/2, select/1, value/1]).

% Returns a default value if the specified argument is not ok.
%
% Examples:
%
%   "hello" = default({ok, "hello"}, "world"),
%   "world" = default(something, "world").
%
% Returns the value of the option, or the default value if the option is not
%   ok.
default({ok, Value}, _Default) -> Value;
default(_None, Default)        -> Default.

% Selects the values in a list of options, disregarding any none options.
%
% Examples:
%
%   ["hello", "world"] = option:select([{ok, "hello"}, gorilla, {ok, "world}).
%
% Returns a list containing the values.
select([{ok, Value} | Rest]) -> [Value | select(Rest)];
select([_None | Rest])       -> select(Rest);
select([])                   -> [].

% Extracts the value from an option or raises an error if the option is none.
%
% Examples:
%
%   "lion" = option:value({ok, "lion"}).
%
% Raises {none, Option} if the option is not ok.
% Returns the option's value.
value({ok, Value}) -> Value;
value(None)        -> error({none, None}).
