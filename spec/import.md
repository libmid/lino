## Imports

Code can only be imported from other `.lat` files.

The basic syntax is:

```
import std;
```

Qualified imports is supported:

```
import std.fmt.println;
```

So is Selective imports:

```
import std.fmt.{println, format};
```

Arbitary import is supported aswell:

```
import std.dbg.*;
```

And nesting them is possible:

```
import std.{fmt.{println, format}, testing.{assert}, net, net.*};
```

