# 🏗️ INSTALL

How to download, test and run.

## Download

You need a client of `git`:
```bash
git clone https://github.com/helvm/helpa.git
cd helpa
```

## Compile

To compile you need `cabal`:
```bash
cabal update
cabal clean && cabal build && cabal test
cabal new-test --test-show-details=streaming
```

## Run

You can run Helpa by `cabal` or directly:
```bash
cabal run helpa assembler_file library_dictionary
dist-newstyle/build/x86_64-osx/ghc-8.10.1/helpa-0.1.0.0/x/helpa/build/helpa/helpa assembler_file library_dictionary
```

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).
