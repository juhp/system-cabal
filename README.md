# system-cabal

A small Haskell build tool over the Cabal library,
which tries to use v1 commands when possible otherwise it falls back to v2.
Unlike cabal-install, it can also be run from a project subdirectory.

`$ scbl --version`

```
0.1.0
```

`$ scbl --help`

```
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

If dependencies are available in the system, then
`scbl build` and `scbl install` will initialize building by
running `Setup configure` if `dist/` does not exist,
and then run `Setup build`.
Further `scbl install` also runs `Setup install`.

Otherwise a v2 build will be done.

## Limitations
Currently projects can only be built from their source tree directly,
eventually downloading source from Hackage may be supported.
