# Rath - civilized Game Boy Advance development from the comfort of your own editor

## what is it

Rath is an interactive development environment for the Gameboy Advance using the Forth programming language. This
means that you can send code and assets from your editor to your GBA while it is
running. Either by typing on an interactive terminal (REPL), or by sending
snippets of code straight from your files. Besides this you can of course
compile whole GBA binaries as well, which you can run on a real GBA or in an
emulator.

The main programming language is Forth (Pandaforth), but you can also call from Forth into C,
or whatever language can interface with the Arm ABI. Forth is a pretty awesome low-level programming language. This implementation/flavor is currently about 2000 lines of Arm assembly, including an interactive shell with which you can poke the environment and create new language constructs like functions and arrays, etc on the fly. As 2000 lines is not that much, you can feel pretty confident you can actually be in full control of your programming language.

Using the [forth-mode Emacs package](https://github.com/larsbrinkhoff/forth-mode), you can send commands, files or file snippets straight from Emacs, never having to leave your editor ever again to repeat that pesky slow and soul-draining cycle of compiling, loading binaries on pesky flash carts, turning on your GBA and seeing things go up in flames yet again (your mileage may vary).

[Youtube demo video](https://www.youtube.com/watch?v=tLI-5SVOY5A):

[![youtube demo vid](https://img.youtube.com/vi/tLI-5SVOY5A/sddefault.jpg)](https://www.youtube.com/watch?v=tLI-5SVOY5A)

## history

This is a fork of a Pandaforth repo I found online which is an unmodified
version of the sources Torlus published in 2005. Which itself is a port of
Camelforth for the Z80, first published by Bradford J. Rodriguez in 1994.

For the original Pandaforth readme, which contains interesting technical
information, see the readme.txt file in this repo.

## current enhancements

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

The Forth implementation now runs +- 3x faster. Previously it was executed from
EWRAM, which is not ideal, but especially not for Arm-mode assembly. It's more
than small enough to run from IWRAM.

I've added a (hopefully cross-platform) Python shell script to interface with
the GBA.

We can now build with a current devkitPro.

We're now using libtonc instead of libgba.

I've deleted common build tools, binaries and libraries that were included in
the repo: libgba, gbafix, test roms, etc..

Forth now plays nice with unix linebreaks. So we can now handle just line feed
instead of cr + line feed.

Xboo and MBv2 support has been removed. I would like to have a conversation with
people that can still run that setup in this day and age.

Converted all Forth words to lowercase. I don't like my programming language
screaming at me, and life is too short to press the capslock button all the
time.

## how to build/use

Install devkitARM, libtonc, and make sure the binaries are in your exec
path. Also make sure the $(DEVKITARM) env variable is set to your devkitARM
folder.

Run `make` in the root of the repo.

To make a demo binary, run `build.sh`.

To connect to said binary with a UART cable:
`<this-repo-root>/shell/shell.py --gbaser /dev/ttyUSB0`

For a simple shell.py help text:
`<this-repo-root>/shell/shell.py --help`

And then type Forth code, one line at a time.

To load files into the GBA from the REPL, type:
`include <filename>`

To use Rath with Emacs (see video above), use the [forth-mode Emacs
package](https://github.com/larsbrinkhoff/forth-mode). It looked like the
package doesn't allow arguments to the Forth program it asks for, so I've
wrapped the above cmdline invocation in a one-liner script.

## future

Rath is meant to be a bona-fide development environment. Of course there's a big
chance this passion project will stumble right after it's first release on
Github. As most projects do.

But hopefully it will some day be featureful enough to be able to make some
apps/games with. It should have some library code to handle the basics like key
presses, background modes, sprites, music, etc (pretty much non-existent for now). The music engine will come from
an existing C/Asm library like Apex Audio System or Maxmod. In addition to that,
there are some quality of life development improvements to be done, like easy
inclusion of assets, interactive asset testing, better IDE integration, etc.

Beyond that one can think of heaps of improvements: swapping out Forth modules
in and out of IWRAM (A lot of Forth implementations make this relatively easy),
test framework, optimized graphics routines, 3d engine, neural engine, etc.. But
baby steps.
