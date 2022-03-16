# system-cabal

A small Haskell build tool over the Cabal library.

```
$ scbl --version
0.1.0
$ scbl --help
system-cabal package build tool

Usage: scbl [--version] COMMAND
  Use system Haskell library to build Haskell packages

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  config                   Configure a package
  configure                alias for config
  build                    Build a package
  run                      Run a package
  install                  Install a package
  test                     Test a package
  haddock                  Build documentation
  repl                     Run interpreter
  clean                    clean dist/
  help                     Cabal help output
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
