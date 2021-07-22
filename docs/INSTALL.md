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
cabal new-clean && cabal new-build && cabal new-test
cabal new-test --test-show-details=streaming
```

## Run

You can run Helpa by `cabal` or directly:
```bash
cabal run helpa assembler_file library_dictionary
```

or after build:
```bash
dist-newstyle/build/x86_64-linux/ghc-8.10.5/helpa-0.3.5.0/build/helpa/helpa assembler_file library_dictionary
```

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).

## 🌈 ❤️ 💛 💚 💙 🤍 🖤 🦄
