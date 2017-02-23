Qweyboard
=========

Qweyboard aims to be a Velotype/Veyboard like experience on a regular standard
computer keyboard.

The Velotype/Veyboard is an orthographic, stenographic keyboard which lets you
type really fast by exploiting language features. It's primarily used with
European languages such as Swedish and Dutch, but it works great for English
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


Dependencies
------------

If it doesn't want to run or build (undefined symbol, linker errors or
whatnot), install the prerequisite dependencies:

 * GNAT (Ada compiler)
 * Xlib (X11 client programming library)
 * XInput2 (support for more advanced input in X11)
 * XTest (extension that enables clients to create input events as if from a real device)

On Debian, these are installed with

    $ sudo gnat apt-get install libx11-6 libx11-dev libxtst-6 libxtst-dev libxi-6 libxi-dev


Key map
-------

If a standard Qwerty keyboard looks something like this

     1  2  3  4  5  6  7  8  9  0  -  =   BS 
      q  w  e  r  t  y  u  i  o  p  [  ]   \
       a  s  d  f  g  h  j  k  l  ;  '   RET
        z  x  c  v  b  k  m  ,  .  / 
                  SPACE

Then you will find that the standard Qweyboard layout looks like

     _  _  P  K  I  _  O  K  P  _  _  _   BS
      _  F  T  J  O  U  I  J  T  F  _  _   \
       Z  S  C  R  E  A  E  R  C  S  Z   RET
        _  _  L  N  Y  N  L  _  _  _ 
                 NOSPACE

where underscore (_) denotes a key that is not yet used by Qweyboard. Keys not
used by Qweyboard retain their normal meaning on the computer – so do all key
combinations. This means that someting like "ctrl-c" is still in it's regular
place. Only regular typing is different on the Qweyboard, not keyboard
shortcuts.

This illustrates the normal layer, where each key produces the letter it says on
the tin. Additional letters are produced by combining different keys. To see
illustrations of other letters and symbols, see the section below on typing on
the Qweyboard.


Typing on the Qweyboard
-----------------------

If we shift the letter of the Qweyboard to look more like the original
Veyboard, you'll notice it is largely symmetric in its layout.

      _  _  P  K  I  _  O  K  P  _  _  _   BS
      _  F  T  J  O  U  I  J  T  F  _  _    \
      Z  S  C  R  E  A  E  R  C  S  Z     RET
         _  _  L  N  Y  N  L  _  _  _ 
                  NOSPACE

To make this section easier, I'll invent a notation for talking about keys on this keyboard. We can split the keyboard into a left half, a middle and a right half. The division looks like so:

                    | M |
                    | I |
                    | D |
        L E F T     | D |    R I G H T
        H A L F     | L |    H A L F
                    | E |
                    |   |
      _  _  P  K  I | _ | O  K  P  _  _  _   BS
      _  F  T  J  O | U | I  J  T  F  _  _    \
      Z  S  C  R  E | A | E  R  C  S  Z     RET
         _  _  L  N | Y | N  L  _  _  _ 
                  NOSPACE

When talking about the key on the left side that on its own represents the
letter T, I will simply write LT. Similarly, RC refers to the key on the right
half which produces the letter C. The middle key A is MA, and so on.

When typing on the Qweyboard, you will be typing what is practically "one
syllable at a time". So, to write the word "tan", you press the keys LT MA
RN. These keys can be pressed in any order; the output will be the order in
which they appear on the Qweyboard. You can even press all three keys
simultaneously, if your keyboard supports it!

A more complicated word like "snails" follows a similar principle. Divide the word up according to left/middle/right halves, and you get

    Word:   S N |    A   | I L S
                |        |
    Half:  Left | Middle | Right
                |        |
    Keys: LS LN |   MA   | RI RL RS

Again, these keys can be pressed in any order, or even simultaneously.

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
      _  _  D  .  _  _  _  .  D  _  _  _   \
       _  _  G  _  _  _  _  _  G  _  _   RET
        _  _  H  W  _  W  H  _  _  _ 
                 NOSPACE

Here, the hash mark indicates that we're holding down the J key, and if you type
any of the other keys with letters, you get that letter instead. So, for
example, the LJ LC combination gives you the letter G. If you want to type a
word like "deer", you'll have to break it down into

    Word:     D  E | E R
                   |
    Half:     Left | Right
                   |
    Keys: LJ LT LE | RE RR
    
where, as you know, the LJ LT combination produces the letter D, and the other
keys simply produce what's on their label.

There's a similar thing going on with the R keys to produce some additional
letters. In their case, the layout will look like

     _  _  _  _  _  _  _  _  _  _  _  _   BS
      _  _  _  _  _  _  _  _  _  _  _  _   \
       _  _  _  .  _  _  _  .  _  _  _   RET
        _  _  V  M  _  M  V  _  _  _ 
                 NOSPACE

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

In terms of getting started and hand placement, I loosely recommend memorizing
that the "central column" of keys are situated on the QWERTY keys Y H B. These
keys split the keyboard in half. Then I let my index, middle and ring fingers
rest on the "QWERTY top row", i.e. the keys E R T, U I O for each hand. The
little finger rests where it naturally lands, on the keys for S and ;.

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


Disclaimer
----------

Most of this, both text and software and theory, is very improvised. The
Velotype/Veyboard is a proprietary, protected, hidden and very expensive
technology. I have never been near the hardware. I have only seen pictures
and read short descriptions on how it's used.

Still, I want to bring something like it to the public, in an open source shape.


Todo
----

These are roughly in order of importance, I guess.

* `[x]` Command-line arguments for timeout delta and language

* `[x]` Let space key denote "no space"

* `[x]` Introduce tasking so that the keyboard can output a syllable after it
  has waited long enough without having to type a new syllable first

* `[x]` General cleaning up of code

* `[x]` Ensure makefile considers which platform it builds on

* `[x]` Ensure proper exceptions are raised in the right places

* `[ ]` Figure out how to make the timeout a part of the qweyboard rather than the surrounding scaffolding main

* `[ ]` "escape shortcut" to release any grabs and stop producing output.
  press again to re-enable the qweyboard (does this require refactoring? probably)

* `[ ]` make it possible to type X (K+Z) and Q (C+F). might require rethinking
  the whole layer idea?

* `[ ]` Backspace should

    1. Clear currently pressed
    2. Erase last space (?)
    3. Erase last syllable

  Requires the qweyboard package to move away from outputting a string...
  (or possibly outputting something like "Either BackSpace String")

* `[ ]` include symbols and numbers in layout

* `[ ]` Recognise "new sentence" (double tap space?) and allow
  auto-capitalisation

* `[ ]` Config files for things like customized layout, escape shortcut etc...

* `[ ]` Some sort of expansion dictionaries? "LF LS LL MA RJ RC => FSLAG =>
  förslag"

* `[ ]` Get a better picture of key presses and releases (to be able to detect
  things such as double tapping?)
