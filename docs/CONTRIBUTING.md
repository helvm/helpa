# 💗 CONTRIBUTING

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.

For more see [ROADMAP](ROADMAP.md) and [COC](CODE_OF_CONDUCT.md).

## HLint

We use `HLint`
```bash
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
```

## Another way to compilation

### Etlas

Compile and run with `etlas`:
```bash
etlas clean && etlas build && etlas test
etlas run helpa
```

### Gradle

Compile and run with `gradlew`:
```bash
./gradlew clean -PetaSendMetrics=true
./gradlew compileEta
./gradlew compileTestEta
./gradlew test
./gradlew run
./gradlew shadowJar
```

