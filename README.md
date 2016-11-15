[![CircleCI](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master.svg?style=svg)](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master) [![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/rafaelfiume/tictactoerl/blob/master/LICENSE)
# tictactoerl

Heavilly inspered on https://howistart.org/posts/erlang/1

    λ → rebar3 compile
    λ → rebar3 release
    λ → rebar3 ct
    λ → rebar3 dialyzer
    λ → ./_build/default/rel/tictactoerl/bin/tictactoerl

## TODO List

* ~~Board creation~~
* Player wins with vertical line
* Player wins with horizontal line
* Player wins with diagonal line
* Game with diagonal line
* Play the game in BOT mode
* Validate user input
* Implement GUI
* Play against computer AI
* Play online against remote opponent
* App runs with a double click
* Provide an installer for the app

# Vagrant

Frequent used commands:

     λ → vagrant up [--provision]
     λ → vagrant reload
     λ → vagrant ssh

Find the synched folder at `/tictactoerl`.

## TODO

- Add rebar3 to the path. (Currently running rebar3 with `../home/ubuntu/rebar3/rebar3`)
