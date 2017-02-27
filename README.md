Qweyboard
=========

Qweyboard aims to be a Velotype/Veyboard like experience on a regular standard
computer keyboard.

The Velotype/Veyboard is an orthographic, stenographic keyboard which lets you
type really fast by exploiting language features. It's primarily used with
European languages such as Swedish and Dutch, and it works great for English
too.


Try it!
-------

Currently Qweyboard only supports Linux/X11 systems, as that is what I have and
use.

You can download and run it by typing

    $ git clone https://github.com/kqr/qweyboard
    $ cd qweyboard
    $ make
    $ ./bin/qweyboard

While the program is running, you can do all your regular typing using the
Qweyboard.

Quit the program by typing CTRL-C.


Dependencies
------------

If it doesn't want to run or build (undefined symbol, linker errors or
whatnot), install the prerequisite dependencies:

 * GNAT (Ada compiler; Ada is compiled with GCC but the gnatmake frontend helps)
 * Xlib (X11 client programming library)
 * XInput2 (support for more advanced input in X11)
 * XTest (extension that enables clients to create input events as if from a real device)

On Debian, these are installed with

    $ sudo gnat apt-get install libx11-6 libx11-dev libxtst-6 libxtst-dev libxi-6 libxi-dev


Key map
-------

The following picture illustrates a regular Qwerty keyboard, except with the
Qweyboard layout on it.

![Qweyboard layout image](https://raw.githubusercontent.com/kqr/qweyboard/master/qweyboard_standard.png)

The colourful circles are where you put your fingers in their neutral position.

An empty key denotes a key that is not yet used by Qweyboard. If you press these
keys, generally nothing happens at all. However, there is currently an exception
in place for the ,./ set of keys, since they seem important and there's no way –
at the moment – to produce those symbols on the Qweyboard.

All key combinations retain their normal meaning on the computer. eys not used by
Qweyboard retain their normal meaning on the computer – and so do all key
combinations!This means that someting like "ctrl-c" is still in it's regular
place. Only normal typing is different on the Qweyboard, not keyboard
shortcuts.

You'll notice that the Qweyboard uses the uppermost row (the "QWERTY numbers
row") for letters. This is indeed correct.

The letters in the middle of the keys are produced by default when you press
them. The letters in the upper right are produced by also pressing the J key on
the same side. The letters in the lower right are produced by also pressing the
R key on the same side. This is indicated with the tiny blips on those keys. The
Q and X letters are special cases.

To learn more, read on about typing on the Qweyboard!


Typing on the Qweyboard
-----------------------

If we shift the letter of the Qweyboard to look more like the original
Veyboard, you'll notice it is largely symmetric in its layout.

      _  _  P  K  I  _  O  K  P  _  _  _   BS
      _  F  T  J  O  U  I  J  T  F  _  _    _
      Z  S  C  R  E  A  E  R  C  S  Z     RET
         _  _  L  N  Y  N  L  _  _  _ 
                  NOSPACE

We can split the keyboard into a left half, a middle and a right half. The
division looks like so:

                    | M |
                    | I |
                    | D |
        L E F T     | D |    R I G H T
        H A L F     | L |    H A L F
                    | E |
                    |   |
      _  _  P  K  I | _ | O  K  P  _  _  _   BS
      _  F  T  J  O | U | I  J  T  F  _  _    _
      Z  S  C  R  E | A | E  R  C  S  Z     RET
         _  _  L  N | Y | N  L  _  _  _ 
                  NOSPACE

When typing on the Qweyboard, you will be typing what is practically "one
syllable at a time". So, to write the word "tan", you press the left T-key, the
middle A key, and the right N key.  These keys can be pressed in any order; the
output will be the order in which they appear on the Qweyboard. You can even
press all three keys simultaneously, if your physical keyboard supports it!

A more complicated word like "snails" follows a similar principle. Divide the
word up according to left/middle/right halves, and you get

    Word:   S N |    A   | I L S
                |        |
    Half:  Left | Middle | Right
                |        |
    Keys:   S N |    A   | I L S

Again, these keys can be pressed in any order, or even simultaneously. What
matters is that you press the L on the right side of the Qweyboard – or else
you'll see it output "slnais".

If you try this, you'll notice that the Qweyboard automatically puts a space
after the word. With the Qweyboard, you don't have to type space yourself. It
automatically inserts them after a syllable.

If you want to type a two-syllable word, you'll have to press (or hold down) the
space key on your keyboard when you type the first syllable. By pressing the
space key (or the NOSPACE key as it is known on the Qweyboard), you suppress the
automatic space output after a word.

You'll notice that the symmetry of the keyboard means there are a few letters
of the alphabet missing. These are achieved in combination with the J and R keys
on each half. If you hold down the J keys, they key layout looks like

     _  _  B  _  _  _  _  _  B  _  _  _   BS
      _  _  D  #  _  _  _  #  D  _  _  _   _
       _  _  G  _  _  _  _  _  G  _  _   RET
        _  _  H  W  _  W  H  _  _  _ 
                 NOSPACE

Here, the hash mark indicates that we're holding down the J key, and if you type
any of the other keys with letters, you get that letter instead. So, for
example, the J+C combination gives you the letter G. If you want to type a
word like "deer", you'll have to break it down into

    Word:     D  E | E R
                   |
    Half:     Left | Right
                   |
    Keys:    J T E | E R
    
where, as you know, the J+T combination produces the letter D, and the other
keys simply produce what's on their label.

There's a similar thing going on with the R keys to produce some additional
letters. In their case, the layout will look like

     _  _  _  _  _  _  _  _  _  _  _  _   BS
      _  _  _  _  _  _  _  _  _  _  _  _   _
       _  _  _  #  _  _  _  #  _  _  _   RET
        _  _  V  M  _  M  V  _  _  _ 
                 NOSPACE

There are two remaining letters though: Q is produced by C+F and X is produced
by K+Z.

The Qweyboard is designed to be used by striking as many keys as possible
simultaneously. Ideally, you should type most syllables with just a single
"chord", or "stroke". In reality, you'll find two problems with that:

1. Many syllables need too many keys to realistically be typed in a single chord, and
2. Many keyboards don't allow you to hold down very many keys simultaneously anyway.

The Qweyboard approach to this is to simply pretend that any combination of keys
pressed in rapid enough succession counts as a single chord or stroke. Note that
this means it is literally impossible, in a sense, to type slowly on the
Qweyboard. You need to figure out all keys for a syllable first, then hit them
rapidly, and then you can take a break to figure out the keys for the next
syllable.

Fortunately for learning, what counts as a "rapid succession" is a parameter
that can be altered when you start the Qweyboard program, allowing slow typists
to take their time, and fast typists to type faster than what the software would
otherwise allow them to.

Remember to use your thumbs! On a regular QWERTY keyboard, they sort of assume
the sole responsibility of pressing space. With Qweyboard, they can be much more
useful than that.


Language Variants
-----------------

At the moment, only Swedish exists as a variation. The way this is done is very
ad-hoc and unresearched, by simply replacing left I, O and E with Å, Ä and Ö in
that order. I have no idea if this is good or bad, nor do I know how it's done
in the actual Velotype/Veyboard. The reason I replaced the left side letters is
because I and E are fairly common letters, so I wanted to keep them on the
"stronger" right hand.


Qweyboard vs. Plover
-------------------

Plover, which is the most visible part of the Open Steno Project, is a much more
mature project. However, it's based on a phonetic stenography system, which
means that if you want to use it, you'll need a full system with a stenographic
dictionary and all. If you only type in English, then this is not a problem and
Plover is likely what you really want.

If you sometimes type in languages other than English, you may want an
orthographic keyboard like the Qweyboard, which (much like a regular keyboard)
still lets you type the letters you want. This turns out to work better for
languages where you don't have a stenographic dictionary available.


Disclaimer
----------

Most of this, both text and software and theory, is very improvised. The
Velotype/Veyboard is a proprietary, protected, hidden and very expensive
technology. I have never been near the hardware. I have only seen pictures
and read short descriptions on how it's used.

Still, I want to bring something like it to the public, in an open source shape.


Todo
----

These are roughly in order of importance, I guess. It's mostly a loose
collection of personal notes and if anyone needs help interpreting any of them
I'm glad to assist.

* `[ ]` shift = sssssymbol keys? actually trickier than I had thought...

* `[ ]` Some sort of expansion dictionaries? "LF LS LL MA RJ RC => FSLAG =>
  förslag" (also tis -> this, waj -> way, typf -> type)

* `[ ]` do something about memory leaks

* `[ ]` Recognise "new sentence" (double tap space?) and allow
  auto-capitalisation

* `[ ]` Actually read layout files...

* `[ ]` Escape shortcut should be configurable with something like `-e <escape shortcut>`?

* `[ ]` Get a better picture of key presses and releases (to be able to detect
  things such as double tapping?)

* `[ ]` Decide what to do about important bits of information I had missed:

    * reverse order of keys left/right? so left = down to up, and right = up to down
    * For english, the following might be surprising:
        * a -> LO RI                     ; LO+RI = a
        * this -> LT RI RS               ; LT = th
        * way -> LJ LN LO RI RJ          ; RJ = y
        * type -> LT MY RP RF            ; RF = -e
        * whole -> CAP+ LO LR LL RL RF   ; ???
