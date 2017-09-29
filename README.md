# Tom's `yi` config
This is my configuration for [`yi-editor`](https://github.com/yi-editor/yi)

I use [`nix`](https://nixos.org/nix/) to manage it, though it will likely build using `cabal` too.

## Building
Use `update_yi_nix.sh` to generate a `yi.nix` file from `yi.cabal`.

Run `nix-build` to build `yi`. Nix will give you a symlink called `result`, so `./result/bin/yi` will be the `yi` that you just built.

## Installing
It's probably a good idea to make sure it builds first, especially since that won't really waste time or make a mess.

If you feel like skipping that part, you still need a `yi.nix`, so do that part.

add the following to your `.config/nixpkgs/config.nix`:

```nix
{
  packageOverrides = pkgs: {
    yi = pkgs.callPackage path/to/this/folder/default.nix {};
  };
}
```

Now you can `nix-env -i yi`, but get this config instead of the default `nix` provides.