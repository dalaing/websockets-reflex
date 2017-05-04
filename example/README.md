An example of using `reflex` to use websockets, both in the frontend and the backend.

All credit for the project layout / nix magic goes to the author of [this project](https://github.com/srhb/reflex-servant-scaffold).

You should be able to do
```
nix-build .
./result/bin/serve
```
to get things started.

At the moment this is using git submodules, but I'll shuffle some of that into nix expressions soon enough.
