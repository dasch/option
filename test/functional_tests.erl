-module(functional_tests).
-compile(export_all).

scan_sku_identifier(Text) ->
  Regex = "SKU\\d{3}-\\d{3}",
  case re:run(Text, Regex) of
    {match, [{Start, Length}]} -> {ok, string:substr(Text, Start + 1, Length)};
    nomatch                    -> none
  end.

example_test() ->
  {ok, "SKU123-456"} = scan_sku_identifier("This is SKU123-456, mate"),
  none = scan_sku_identifier("Nothing to see here.").

select_test() ->
  Texts = ["Product: SKU123-456", "Refrigerator", "SKU666-666 item"],
  SkuOptions = lists:map(fun scan_sku_identifier/1, Texts),
  ["SKU123-456", "SKU666-666"] = option:select(SkuOptions).
