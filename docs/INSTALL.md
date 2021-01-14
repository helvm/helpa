# INSTALL

How to download, test and run.

## Download

```bash
git clone https://github.com/helvm/helpa.git
cd helpa
```

## HLint

```bash
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
```

## Cabal

Compile and run with `cabal`:
```bash
cabal update 
cabal clean && cabal build && cabal test
cabal new-test --test-show-details=streaming
cabal run helpa
```

## Etlas

Compile and run with `etlas`:
```bash
etlas clean && etlas build && etlas test
etlas run helpa
```

## Gradle

Compile and run with `gradlew`:
```bash
./gradlew clean -PetaSendMetrics=true
./gradlew compileEta
./gradlew compileTestEta
./gradlew test
./gradlew run
./gradlew shadowJar
```

## Tools

<!-- 

alias haskell-formatter='/Users/kamil.zabinski/.local/bin/haskell-formatter'
 
-->

```bash
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
```

https://github.com/evolutics/haskell-formatter#readme
```bash

find src/ -name '*.hs' -type f -print0 | xargs -0 -I {} -n 1 haskell-formatter --force --input {} --output {} --style .haskell-formatter.yaml
```

https://github.com/jaspervdj/stylish-haskell#readme

```bash
curl -sL https://raw.github.com/jaspervdj/stylish-haskell/master/scripts/latest.sh | sh -s .
```


<!-- https://github.com/mihaimaruseac/hindent#readme -->

<!--
https://github.com/lspitzner/brittany#readme

```bash
brittany --write-mode=inplace *.hs
```
-->

<!--
https://github.com/danstiner/hfmt#readme

```bash
hfmt -w
```
-->

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).
