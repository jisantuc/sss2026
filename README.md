# Space Software Summit 2026 sim code

This repo contains simulation code for how difficult it is to debug software once the problem might live in space.
All probabilities are made up, and inferences shouldn't be drawn from any particular values present here. I had to
pick values to make plots, and I had to make plots to make slides, and I had to make slides to have something to stand
in front of while I say words.

## Quick start

```shell
$ nix develop --command cabal repl
ghci> allPlots 10000
```

## Dev environment

This project uses [`nix`](https://nixos.org/). Your best bet for getting set up is `nix develop` or
[`direnv`](https://direnv.net/). You could install Haskell tooling with `ghcup`, but you'll end up with different
versions of things from what works in the flake. Just try `nix`, maybe you'll love it. Version bounds are included
in the `.cabal` file if you really want to use the manual setup.

## Simulation code

### One-shot

Plots for the presentation were produced with the Haskell project described in [`sss2026.cabal`](./sss2026.cabal).
Snapshots are included in `plots/`. If you want to regenerate them, you can fire up a repl with `cabal repl` and
run `allPlots n` with however many samples you want to evaluate.

### Web app

Still to come, will use `elm-stat`, which
[supports](https://package.elm-lang.org/packages/jxxcarlson/elm-stat/latest/StatRandom) all the needed distributions
and has some nice [svg plotting](https://package.elm-lang.org/packages/jxxcarlson/elm-stat/latest/StatChart).
