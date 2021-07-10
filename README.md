# Rath - civilized Game Boy Advance development from the comfort of your own editor

## what is it

Rath is an interactive development environment for the Gameboy Advance using the
Forth programming language. This means that you can send code and assets from
your editor to your GBA while it is running. Either by typing on an interactive
terminal (REPL), or by sending snippets of code straight from your
files. Besides this you can of course compile whole GBA binaries as well, which
you can run on a real GBA or in an emulator.

The main programming language is Forth (Pandaforth), but you can also call from
Forth into C, or whatever language can interface with the Arm ABI. Forth is a
pretty awesome low-level programming language. This implementation/flavor is
currently about 2000 lines of Arm assembly, including an interactive shell with
which you can poke the environment and create new language constructs like
functions and arrays, etc on the fly. As 2000 lines is not that much, you can
feel pretty confident you can actually be in full control of your programming
language.

Using the [forth-mode Emacs
package](https://github.com/larsbrinkhoff/forth-mode), you can send commands,
files or file snippets straight from Emacs, never having to leave your editor
ever again to repeat that pesky slow and soul-draining cycle of compiling,
loading binaries on pesky flash carts, turning on your GBA and seeing things go
up in flames yet again (your mileage may vary).

[Youtube demo video](https://www.youtube.com/watch?v=tLI-5SVOY5A):

[![youtube demo
vid](https://img.youtube.com/vi/tLI-5SVOY5A/sddefault.jpg)](https://www.youtube.com/watch?v=tLI-5SVOY5A)

## history

This is a fork of a Pandaforth repo I found online which is an unmodified
version of the sources Torlus published in 2005. Which itself is a port of
Camelforth for the Z80, first published by Bradford J. Rodriguez in 1994.

For the original Pandaforth readme, which contains interesting technical
information, see the readme.txt file in this repo.

## example game

I created an [entry](https://klomp.itch.io/covid-adventure) for the [itch.io GBA Jam
2021](https://itch.io/jam/gbajam21), which is a very simple top-down 2D tile
background based intro to a possible adventure/RPG.

[Youtube video](https://www.youtube.com/watch?v=sxgEoEmLS8s):

[![youtube demo
vid](https://img.youtube.com/vi/sxgEoEmLS8s/sddefault.jpg)](https://www.youtube.com/watch?v=sxgEoEmLS8s)


The main code is in `<root>/forth/to-asm/d-lib-constants.fth` file. All the game
logic is in Forth. We use C for system bootstrap, serial communication and for
glue code between Forth, the music engine and the interrupt routines. The latter
two are written in assembly.

## features

### library

There's a decent amount of library code:
- constants for memory locations and IO registers
- shadow OAM that updates the OAM data on vblank
- key press detection
- abstract sprite object that tracks various properties
- player movement
- sprite direction logic
- text boxes
- collision detection
- 'things of interest' map overlay that can be queried at tile granularity
- layer blending
- interrupts, which are handled by tonclib, interfaced through C
- sound: Apex Audio System

The big issue currently with the library is that it's quite intertwined with the
game that I wrote it for. Hopefully I will find some time to disentangle the
two. Which for the majority of the library code shouldn't be too hard.

The library code is still a far cry from general and flexible. The only way it
will move towards this ideal is if it will actually need to evolve because of
real-world demand. But I must say that Forth is lends itself quite well to
refactoring if need be.


### serial communication

Back in 2005, computers still came standard with serial ports, and the prevaling
methods to connect to your GBA were mbv2 and Xboo cables. It turns out you can
also use USB UART cables. I made a repo with code and a tutorial on how to make
one: [gba-serial-adventures](https://github.com/stuij/gba-serial-adventures)

I concocted a (very simple) custom communication protocol between computer and
GBA that does checksums of data the computer sends. Also the GBA receives data
async in a ring buffer so we can blast at 115200 baud, without spinning when
waiting on data while waiting on input (perhaps Xboo and MBv2 did this too, I
have no idea). In any case, this makes sending binary data at reasonable speeds
possible, without having to worry if we dropped a bit somewhere.

I've added a (hopefully cross-platform) Python shell script to interface with
the GBA from a computer.

### some modernizations and breaking changes from the original Pandaforth

- builds expect a modern devkitPro.
- switched from libgba to libtonc
- converted all Forth words to lowercase
- removed mbv2 and Xboo support, added own serial protocol
- added halfword memory access words
- for speed added byte access words that use ldrb instead of ldrh
- for speed rewrote `move` and `fill` functions in assembly.
- moved Forth base system from ewram to iwram

For the game I needed a cross-compiler, as compiling code at runtime is way to
slow for a game. Very quickly I was running up against compile times of half a
minute. What I have now is something quite hackish and simplistic. We can
compile but we don't really understand the Forth code. The cross-compiler isn't
strong enough to handle `immediate mode`. Some immediate words like loop
constructs are handled by specialized Python code. Ideally we would rewrite the
compiler to interpret all the Forth primitives in some abstract way. Hopefully
I'll find the time for this some day.

## how to build/use

This repo contains submodules, so when you pull from Github, make sure to pull
the submodules as well:

    git clone --recurse-submodules git@github.com:stuij/rath.git

Install devkitARM, libtonc, and make sure the binaries are in your exec
path. Also make sure the $(DEVKITARM) env variable is set to your devkitARM
folder.

Run `make` in the root of the repo. This creates two binaries called `rath.gba`,
and `covid-adventures.gba`. The former is the Forth base system, which you can
interact with through a serial cable. The latter is the demo game.

For interactive development, flash the binary on a cart, put the cart in a Game
Boy Advance, and start it. `rath.gba` will put you in repl
mode. `covid-adventures.gba` should start a game loop with a little sprite you
can control with the direction pad. To jump out of the game loop and into the
repl, press select. Currently, I temporarily broke the interactivity in
`covid-adventures.gba` as I ran up against a deadline of a game jam. I hope to
reinstate it soon.

To connect to said binary with a UART cable:
`<this-repo-root>/shell/shell.py --gbaser /dev/ttyUSB0`

For a simple shell.py help text:
`<this-repo-root>/shell/shell.py --help`

And then type Forth code, one line at a time.

To load files into the GBA from the REPL, type:
`include <path/to/filename>`

To use Rath with Emacs (see video above), use the [forth-mode Emacs
package](https://github.com/larsbrinkhoff/forth-mode). It looked like the
package doesn't allow arguments to the Forth program it asks for, so I've
wrapped the above cmdline invocation in a one-liner script.

You can also run the PFdemo.gba file in an emulator, if you want to move the
little sprite around. Not too exciting to all, but I think it's quite cool :D


## attribution and licensing

Forth programming language:

The underlying Forth system was originally written by Bradford J. Rodriguez in
1994 for the Z80. This Forth flavor is known as CamelForth.

- license: see <root>/source/cam80-12/README.Z80 in this repo
- site: http://www.camelforth.com

Camelforth was ported to armv4 by Torlus, with no explicit copyright statement
other than a (c) after their name in <root>/README.txt in this repo.

I myself have made some modifications to the Forth sources, and I've added a
simplistic cross compiler.

For the original code by Torlus, see the first commit in this repo, which was
put on Github by user `iansharkey`: https://github.com/iansharkey/pandaforth


Assets used for the example game:

intro/continue screen Covid virus impression:
- credit: Alissa Eckert, MSMI; Dan Higgins, MAMS
- license: public domain
- site: https://phil.cdc.gov/Details.aspx?pid=23311

ring tone: "phone ringing.wav"
- credit: `Tomlija`, on freesound.org
- license: Creative Commons, Attribution
  https://creativecommons.org/licenses/by/3.0/
- site: https://freesound.org/s/98023

piano note: pianos/roland_grand_piano/C6.WAV
- credit: the WaveWorld sample library
- license: public domain
- site: https://modarchive.org/forums/index.php?topic=2406.0

The font came with Pandaforth.

For the IO register naming conventions and the key input logic, I've adapted the
code in the headers of [libtonc](https://github.com/devkitPro/libtonc).

The rest of the assets and code for the example game was made by me:
- pixelart for apartment, phone and player sprite-work
- dialog, text
- music
- programming


The license for any modifications, additions to existing work and also all the
original work by me in any medium falls under the repo-wide license, which can
be found in <root>/LICENSE.

Please let me know if I've overlooked anything asset or license related.
