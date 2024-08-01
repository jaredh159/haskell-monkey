_default:
  @just --choose

run *args:
  @cabal run haskell-monkey --verbose=1 -- {{args}}

watch-test:
  @watchexec --project-origin . --clear --restart \
    --watch src --watch test --exts hs \
    'cabal run tests -v0'
