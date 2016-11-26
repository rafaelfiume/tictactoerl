[![CircleCI](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master.svg?style=svg)](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master) [![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/rafaelfiume/tictactoerl/blob/master/LICENSE)
# tictactoerl

A simple Tic-Tac-Toe console game implemented with Erlang. 

* A Kotlin version is available [here](https://github.com/rafaelfiume/tictactoe).

Heavilly inspered on [Erlang With Fred Hebert](https://howistart.org/posts/erlang/1) to deal with input/output, including how to mock and capture input/output in common tests, among other things. 

## How to Play It

To mark a position in the grid, imagine a telephone dial:

    1 | 2 | 3
    ---+---+---
    4 | 5 | 6
    ---+---+---
    7 | 8 | 9

## Running the Application Locally

Build and run it with:

    λ → rebar3 release
    λ → ./_build/default/rel/tictactoerl/bin/tictactoerl

Other useful commands are:

    λ → rebar3 compile
    λ → rebar3 ct
    λ → rebar3 eunit [--module=console_input_reader_test]
    λ → rebar3 dialyzer

### BOT Mode

To play TicTacToe[rlang] in Bot Mode, change the "botmode" env var from `off` to `on`.


# [Vagrant](#vagrant)

You can use [Vagrant](https://www.vagrantup.com) to build, run tests and execute this program. To do that:

1. Go to ${PROJECT_DIR}/environemnt
2. `vagrant up`, then
3. `vagrant ssh`
4. `cd /tictactoerl` (this is the [synced folder](https://www.vagrantup.com/docs/synced-folders/))
5. `rebar3 eunit`

### Install

You may need to install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads) first if you haven't done that yet.

Out of curiosity, the following commands are frequently used when configuring a new environement:

     λ → vagrant init
     λ → vagrant destroy
     λ → vagrant reload

Other frequent Vagrant commands are:

     λ → vagrant up [--provision] [--debug]
     λ → vagrant ssh
     λ → vagrant halt
     λ → vagrant global-status

### Troubleshooting 1

You may see the following error when trying to `vagrant up`:

> The box 'ubuntu/xenial64' could not be found or
could not be accessed in the remote catalog. If this is a private
box on HashiCorp's Atlas, please verify you're logged in via
`vagrant login`. Also, please double-check the name. The expanded
URL and error message are shown below:
>
> URL: ["https://atlas.hashicorp.com/ubuntu/xenial64"]
Error: 

This happens due to a problem with the curl shipped with Vagrant, and it's solved replacing it with a curl that works:

1. Move the embedded curl somewhere:

```shell-script
sudo mv /opt/vagrant/embedded/bin/curl /opt/vagrant/embedded/bin/curl_orig
sudo mv /opt/vagrant/embedded/bin/curl-config /opt/vagrant/embedded/bin/curl-config_orig
```

2. Symlink a curl that works:

```shell-script
sudo ln -s /usr/bin/curl /opt/vagrant/embedded/bin/curl
sudo ln -s /usr/bin/curl-config /opt/vagrant/embedded/bin/curl-config
```

(Original solution [here](https://github.com/mitchellh/vagrant/issues/5016).)

### Troubleshooting 2

If you are having problems compiling or executing the tests after rebasing changes, even if the [build](https://circleci.com/gh/rafaelfiume/tictactoerl) clearly says there's nothing wrong, try this:

     λ → git clean -dfx --force

or just:

     λ →  rm -r _build/


# TODO List

### ~~Playing TicTacToe using a terminal (Parent Story)~~
* ~~Board creation~~
* ~~Player X wins with vertical line~~
* ~~Player O wins with horizontal line~~
* ~~Player X wins with diagonal line~~
* ~~Game ends with a draw~~
* ~~Play the game in BOT mode~~
* ~~Validate player input (player selects an already marked cell in the board)~~
* ~~Validate player input (player selects unknown position)~~
