_default:
  @just --choose

run:
  @cabal run --verbose=1

watch-test:
  @watchexec --project-origin . --clear --restart \
    --watch src --watch test --exts hs \
    'cabal run tests -v0'
