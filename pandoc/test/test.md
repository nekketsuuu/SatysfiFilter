## Test

This file is for testing.

```satysfi
@require: stdjareport

%% BEGIN
document (|
  title = {test};
  author = {nekketsuuu};
  show-title = false;
  show-toc = false;
|) '<
  +p {
    Testing!
  }
>
```

Yay!

Another one...

```satysfi
@require: standalone

let-block ctx +body it =
  let ib = read-inline ctx it in
  let ib = inline-fil ++ ib ++ inline-fil in
  line-break false false ctx ib
in
%% BEGIN
standalone '<
  +body {
    ${ax^2 + bx + c = 0}
  }
>
```
