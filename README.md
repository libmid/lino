`Lino` is a general purpose, compiled, "zero bs" programming language.
Lino uses `.li` as its extension and [QBE](https://c9x.me/compile/) as its backend.

Note: Currently the language is glued together by hopes and dreams. It's so early in its developement that even core features don't work.

Here's how you do hello world in Lino.

```python
import std.io.println;

def main() {
    println("Hello World!\n");
}
```

Checkout the language documentation [here](/docs/).

## Installation

Currently lino needs to be manually built using `cargo`.
Install the rust toolchain if you dont have it already.
Next build using `cargo build --release`.
You can find the compiler executible in `target/release/lino`.

Add the **standard library** to path or provide it manually through the `--stdlib=path/` option.

## Some more examples

A more exhaustive list can be found in the [examples](/examples) directory.

TODO: Add more examples.