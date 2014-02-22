-module(option_tests).
-compile(export_all).

default_test() ->
  42 = option:default({ok, 42}, 123),
  123 = option:default(not_ok, 123).

select_test() ->
  [1, 3] = option:select([{ok, 1}, {not_ok, 2}, {ok, 3}]).

value_test() ->
  42 = option:value({ok, 42}),
  try option:value(not_ok) of
    _ -> 1 = 2
  catch
    error:{none, None} -> not_ok = None
  end.
