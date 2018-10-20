## Index

This file is for testing.

```satysfi
@require: standalone

let-block ctx +p it =
  let ib = read-inline ctx it in
  let ib = inline-fil ++ ib ++ inline-fil in
  line-break false false ctx ib
in
standalone '<
%% BEGIN
+p {
  Testing!
}
%% END
>
```

Yay!

Another one...

```satysfi
@require: standalone
@require: math

%% BEGIN
standalone '<
  +math (${
    ax^2 + bx + c = 0
  });
>
```

This one is not compiled:

```{.satysfi eval="no"}
@require: stdjareport

document (|
  title = {test};
  author = {nekketsuuu};
|) '<
  +p {
    Testing!
  }
>
```

This one will cause an error, but it will be collected and displayed:

```{.satysfi eval="error"}
@require: stdjabook

document (|
  title = {error test};
  author = {nekketsuuu};
|) '<
  +p {
    Testing!
  }
>
```

This one is compilied, but a result image is not embedded.

```{.satysfi eval="type-check-only"}
let-rec fact n =
  if n <= 1 then 1
  else fact (n - 1)
```

Here is [another page](./child.html).
