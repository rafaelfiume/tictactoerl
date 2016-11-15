[![CircleCI](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master.svg?style=svg)](https://circleci.com/gh/rafaelfiume/tictactoerl/tree/master) [![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/rafaelfiume/tictactoerl/blob/master/LICENSE)
# tictactoerl

Heavilly inspered on https://howistart.org/posts/erlang/1

    λ → rebar3 compile
    λ → rebar3 release
    λ → rebar3 ct
    λ → rebar3 dialyzer
    λ → ./_build/default/rel/tictactoerl/bin/tictactoerl


# Vagrant

Frequent used commands:

     λ → vagrant up [--provision] [--debug]
     λ → vagrant ssh

The [synced folder](https://www.vagrantup.com/docs/synced-folders/) for this project is `/tictactoerl`.

### Install

You may need to install [Vagrant](https://www.vagrantup.com/downloads.html) and [VirtualBox](https://www.virtualbox.org/wiki/Downloads) first if you haven't done that yet.

Out of curiosity, the following commands are frequently used when configuring a new environement:

     λ → vagrant init
     λ → vagrant destroy
     λ → vagrant reload

### Troubleshooting

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

# TODO List

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
