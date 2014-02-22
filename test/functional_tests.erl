-module(functional_tests).
-compile(export_all).

scan_sku_identifier([]) ->
  none;
scan_sku_identifier([$S, $K, $U, X1, X2, X3, $-, X4, X5, X6 | _Rest]) ->
  {ok, [$S, $K, $U, X1, X2, X3, $-, X4, X5, X6]};
scan_sku_identifier([_ | Rest]) ->
  scan_sku_identifier(Rest).

example_test() ->
  {ok, "SKU123-456"} = scan_sku_identifier("This is SKU123-456, mate"),
  none = scan_sku_identifier("Nothing to see here.").

select_test() ->
  Texts = ["Product: SKU123-456", "Refrigerator", "SKU666-666 item"],
  ["SKU123-456", "SKU666-666"] = option:select([scan_sku_identifier(X) || X <- Texts]).
