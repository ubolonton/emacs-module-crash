Segfault examples in Emacs dynamic module.

Tested on Emacs 25.3.1, macOS 10.13.4, Ubuntu 16.04.

- Run this to see the non-segfault cases
    ``` bash
    ./build.sh && ./test.sh
    ```
- Uncomment a crashing test in [test.el](./test.el) and re-run the above to get a segfault.
