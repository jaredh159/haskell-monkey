_default:
  @just --choose

watch-test:
  @watchexec --project-origin . --clear --restart \
    --watch src --watch tests --exts hs \
    'cabal run tests -v0'
