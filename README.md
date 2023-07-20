# NoGardenOnline
Port of [NoGardenPuzzle](https://github.com/J0J0/games-poc/tree/main/NoGardenPuzzle)
to the [reflex(-dom)](https://hackage.haskell.org/package/reflex-dom)
[FRP](https://wiki.haskell.org/Functional_Reactive_Programming) framework
for running directly in the web browser.

## Game rules
The goal is to fill the rectangular game field with horizontal and vertical lines,
such that each line starts and ends "outside" of the field. A started line extends
in a straight manner until it hits a blocked tile or the outside. In the former
case, the line must be continued in either possible direction.

## Build instructions
The project is built with `reflex-platform`'s Nix toolchain.
Choose the appropriate section:

### Nix and reflex-platform already set up
Just run `make`.

For deployment, you might want to optimize the huge javascript file.
To do this with google's `closure-compiler`, you can run `make minify`.

### Nix installed, but new to reflex-platform
You almost certainly want to use the Nix binary cache at `reflex-frp.org`.
In order to do so, either add the cache and key manually as described
[here (only step 2)](https://github.com/obsidiansystems/obelisk#installing-obelisk),
or let the `reflex-platform/try-reflex` script patch your Nix config (see also [next section](#What-is-Nix)).

Then simply run `make`.

### What is Nix?!
> Nix is a tool that takes a unique approach to package management and system configuration. Learn how to make reproducible, declarative and reliable systems.

— from its homepage <https://nixos.org/>.

If you can get Nix through the package manager of your operating system, you
might prefer to do so and continue with the [previous section](#Nix-installed-but-new-to-reflex-platform).

Otherwise, you can have `reflex-platform` install Nix for you.

0. _Warning:_ The following requires root permissions and installs Nix system wide. If you
don't want this, you may [consider alternatives](https://nixos.wiki/wiki/Nix_Installation_Guide#Installing_without_root_permissions).

1. Clone this repo with `--recurse-submodules` or – if you already cloned it without –
initialize the `reflex-platform` submodule via `git submodule update --init`.

2. Run `reflex-platform/try-reflex`, which should offer you to install Nix.
It also sets up Nix binary caches (or asks if it should in case you already
have Nix installed).

3. After it finished, quit the new shell it opened.

Now, you can [build this project](#Nix-and-reflex-platform-already-set-up).
