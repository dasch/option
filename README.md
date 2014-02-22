Options in Erlang
=================

Options are structures that contain either one or no value. They're useful if a function may not be able to return a value. As an example, consider a function that parses a string, returning a stock keeping unit (SKU) identifier in the format `SKUXXX-XXX` if one can be found:

```erlang
scan_sku_identifier(Text) ->
  Regex = "SKU\\d{3}-\\d{3}",
  case re:run(Text, Regex) of
    {match, [{Start, Length}]} -> string:substr(Text, Start + 1, Length);
    nomatch                    -> none
  end.
```

This function returns either the SKU identifier, or `none` if no such identifier could be found. The problem is that users of this function have to remember to deal with the `none` case:

```erlang
case scan_sku_identifier(Text) of
  none -> some_error;
  SKU  -> continue_doing_stuff(SKU)
end.
```

Most likely, though, they'll forget it and just write:

```erlang
SKU = scan_sku_identifier(Text).
```

If `Text` doesn't contain a valid SKU, the `SKU` variable will be bound to the atom `none`, and the program will continue. This is obviously bad – you want the program to fail close to the location of the bug.

The solution is to force the user to deal with the fact that the function does not return the SKU itself, but rather the _option_ of a SKU, meaning it might not return any value at all. The updated implementation will look like this:


```erlang
scan_sku_identifier(Text) ->
  Regex = "SKU\\d{3}-\\d{3}",
  case re:run(Text, Regex) of
    {match, [{Start, Length}]} -> {ok, string:substr(Text, Start + 1, Length)};
    nomatch                    -> none
  end.
```

The user can now choose to deal with the error explicitly or not. If she expects there to always be a SKU in the string, she can write the code thus:

```erlang
{ok, SKU} = scan_sku_identifier(Text).
```

If, due to some bug, there isn't one anyway, an error will be raised at the location of the bug. Additional error handling can then be added to handle the case more gracefully:

```erlang
case scan_sku_identifier(Text) of
  {ok, SKU} -> continue_doing_stuff(SKU);
  none      -> deal_with_the_error()
end.
```

The point being that the bug was much easier to find.

This is a fairly common pattern in Erlang. The point of this library is to provide some high level functions for dealing with options. For instance, say you wish to extract the SKUs from a list of strings, disregarding the strings that contain no SKUs. Using the `option` module, it's trivial:

```erlang
Texts = ["Product code: SKU123-456.", "Refrigerator", "This is SKU666-666!"],
SkuOptions = lists:map(fun scan_sku_identifier/1, Texts),
["SKU123-456", "SKU666-666"] = option:select(SkuOptions).
```

`option:select` takes care of extracting the values and removing the none options.

Another common pattern is when, if an option is none, a default value should be used. `option:default/2` makes this easy:

```erlang
"SKU123-456" = option:default(scan_sku_identifier("Product: SKU123-456"), "N/A"),
"N/A" = option:default(scan_sku_identifier("Microwave oven"), "N/A").
```


Usage
-----

Any tuple `{ok, X}` will be considered an ok option with the value `X` – anything else is a none option.

- `option:select/1` extracts the ok values from a list of options.
- `option:value/1` extracts the ok value from a single argument, raising an error if it is none.
- `option:default/2` extracts the ok value from a single argument, returning a default value if the argument is none.


Copyright
---------

Copyright (c) 2014 Daniel Schierbeck

Licensed under the [MIT license](LICENSE).
