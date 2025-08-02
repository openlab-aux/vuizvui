# Installing a machine in Vuizvui {#part-install}

The easiest way to get started is if the machine is already in Vuizvui so
there is a channel available.

You can have a look at [list of channels][hydra-channel-list]
to check whether a channel exists for the machine you want to install.

<!-- FIXME(sterni): machine no longer exists -->
So let's say you want to install the machine `schnurrkadse` which
has the channel attribute `channels.machines.sternenseemann.schnurrkadse`.

First you need to add the channel for the `root` user of your current system
using the following commands:

```ShellSession
# nix-channel --add https://hydra.build/channel/custom/openlab/vuizvui/channels.machines.sternenseemann.schnurrkadse vuizvui
# nix-channel --remove nixos  # otherwise it will interfere with the rebuild
# nix-channel --update
```

Notice the `vuizvui` argument at the end of the first
command. This makes the channel available as
`<vuizvui>` in the search path of the current system.

For the first installation the {env}`NIX_PATH` isn't correctly set
and will be set to include the `vuizvui` channel after
you've switched to the configuration for the first time.

Next put the following in your
{file}`/etc/nixos/configuration.nix`:

```nix
(import <vuizvui/machines>).sternenseemann.schnurrkadse.config
```

Of course you need to replace `sternenseemann.schnurrkadse` with the
attribute of your machine.

Now in order to do the first build and activation of the configuration, you
need to issue the following command as root:

<!-- FIXME: This WON'T work because of wrong NIX_PATH and missing binary
            cache public key! -->
<!-- TODO: create a bootsrap script that does this automatically -->
```ShellSession
# nixos-rebuild \
    -I nixpkgs=/nix/var/nix/profiles/per-user/root/channels/vuizvui/nixpkgs \
    --option binary-cache-public-keys "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg=" \
    switch
```

We redefine `<nixpkgs>` here, because vuizvui brings its own nixpkgs that gets build on the hydra, using it we get to download from the binary cache. Additionally, we need to manually specify the public key for the `hydra.build` hydra (`headcounter.org/hydra` was the old URL, hence the key name).

[hydra-channel-list]: https://hydra.build/jobset/openlab/vuizvui#tabs-channels
