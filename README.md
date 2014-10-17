libuv bindings for Common Lisp
==============================
Please note that these bindings aren't fully documented, and unless someone
actually wants to take on this task, they will most likely stay that way as
they were built/generated as a means of driving [cl-async](https://github.com/orthecreedence/cl-async).

Conventions
-----------
Who needs documentation when you follow simple function-naming conventions?

- The package prefix is `uv:`
- Underscores become dashes

That's actually it. For a reference on libuv itself, see the [libuv
docs](http://docs.libuv.org/en/latest/index.html).

### Example
```c
// TODO
```

Becomes:

```common-lisp
;; TODO
```

(re)Generating
--------------
If a new version of libuv comes out, you can regenerate these bindings by
doing the following (if you have [swig](http://www.swig.org/) installed):

```bash
cd /path/to/cl-libuv
vim scripts/bindings-posix.i      # update "%include" paths to point at your libuv headers
./scripts/generate                # must be run in cl-libuv folder
```

This will generate new bindings in their entirety (it's fully automated).

Notes
-----
As mentioned, these bindings were made specifically to be the backend for
[cl-async](https://github.com/orthecreedence/cl-async), and because of this,
they do not (nor will they ever) have a higher-level interface. They are meant
to be an extremely thin layer between Lisp and c/libuv.

MIT Licensed.


