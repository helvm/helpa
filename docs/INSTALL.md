# INSTALL

How to download, test and run.

## Download

```bash
git clone https://github.com/helvm/helpa.git
cd helpa
```

## Cabal

Compile and run with `cabal`:
```bash
cabal clean && cabal build && cabal test
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

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).
