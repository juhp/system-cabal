# system-cabal

A small Haskell build tool over the Cabal library.

```
$ scbl --help
system-cabal package build tool

Usage: scbl [--version] COMMAND
  Use system Haskell library to build Haskell packages

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  config                   Configure a package
  build                    Build a package
  install                  Install a package
```

## Usage
`scbl config` runs `Setup configure --user --prefix=~/.local`.

`scbl build` and `scbl install` will initialize building by
running `Setup configure` if `dist/` does not exist,
and then run `Setup build`.

Further `scbl install` also runs `Setup install`.

## Limitations
Currently projects can only be built from their source tree directly,
downloading source from Hackage etc is planned.
