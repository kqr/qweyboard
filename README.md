Qweyboard
=========

![Qweyboard standard layout image](https://raw.githubusercontent.com/kqr/qweyboard/master/images/standard-layout.png)

Qweyboard aims to be an open source Velotype/Veyboard like experience on a
regular standard computer keyboard.

The Velotype/Veyboard is an orthographic, stenographic keyboard which lets you
type really fast by exploiting language features. Orthographic means you still
type letter-by-letter (as opposed to phonetically, based on sound). Stenographic
means you generally make fewer keypresses than with regular keyboards.


Try it!
-------

Currently Qweyboard only supports X11-based systems, as that is what I have and
use. You should be able to download and run it by typing

    $ git clone https://github.com/kqr/qweyboard
    $ cd qweyboard
    $ make
    $ ./bin/qweyboard

While the program is running, you can type using the Qweyboard in any window and
any application.

The Qweyboard program does *not* affect any keyboard shortcuts (like ctrl-s,
alt-x, super-w and so on).

If you need to type a password or otherwise temporarily break out of the
Qweyboard layout, you can do that by pressing ctrl-shift-x, which will disable
the Qweyboard but keep the program running. Press ctrl-shift-x again to
re-enable it.


Dependencies
------------

If Qweyboard doesn't want to run or build (undefined symbol, linker errors or
whatnot), install the prerequisite dependencies:

 * GNAT (Ada compiler; Ada is compiled with GCC but the gnatmake frontend helps)
 * Xlib (X11 client programming library)
 * XInput2 (support for more advanced input in X11)
 * XTest (extension that enables clients to create input events as if from a real device)

On Debian, these are installed with

    $ sudo apt-get install gnat libx11-6 libx11-dev libxtst-6 libxtst-dev libxi-6 libxi-dev


Short tutorial
--------------

This is a short tutorial on how to use the Qweyboard. The Qweyboard is easy to
learn, because it is a lot like a regular QWERTY keyboard in that each key
produces a letter in the final word. THere are three main differences to your
normal QWERTY keyboard:

1. The most common letters are under your fingers already, which means less
movement is required. By just learning the ten keys under your fingers, you can
already type a lot of stuff.

2. The keys are almost completely symmetrically laid out. This makes the
Qweyboard surprisingly easy to learn. Your muscle memory loves symmetry.

3. It doesn't matter which order you press keys on the Qweyboard. As long as you
press the same keys, the same word will appear on the screen regardless of which
order you pressed the keys in.


### Layout

To start learning, refer to the following image, which illustrates a regular
QWERTY keyboard with the Qweyboard layout on it. (This is the image that's also
at the top of this document.)

![Qweyboard standard layout image](https://raw.githubusercontent.com/kqr/qweyboard/master/standard-layout.png)

The colourful circles indicate where your fingers go when they are in their
neutral position, which you return to this once you've hit some keys. The purple
circles are your thumbs. You'll want to use those when you type on the
Qweyboard! Below is a similar illustration, except the keys each finger is
responsible for are lit up in the colour of that finger.

![Qweyboard finger responsibilities](https://raw.githubusercontent.com/kqr/qweyboard/master/images/finger-keys.png)

When you type on the Qweyboard, you type entire *syllables* at once. A word like
"tin" is produced by pressing the left T-key, the right I-key and the right
N-key at the same time. Try it now! Look at the picture below and press your
left ring finger, your right index finger and your right thumb
simultaneously.

![Typing "tin"](https://raw.githubusercontent.com/kqr/qweyboard/master/images/tin.png)

You don't have to move your fingers – you type a whole word by just striking
down on a few fingers.

> Oh, and you're on a laptop keyboard so your keyboard doesn't register that many
key presses at once? Not a big problem. The Qweyboard is designed such that if
you press several keys in rapid enough succession, that counts as pressing them
simultaneously. Any time I speak of pressing many keys at the same time, you can
mentally translate that as "pressing keys in quick enough succession".

> It doesn't matter in which order you press the keys. The final word is based
on *which* keys you press, not in which order they are pressed.

> How quick is "rapid enough"? That's configurable. At the moment, the default
is 500 milliseconds, but this may change as I get more experience myself and
can make better judgements about these things.

So what if you want to type the reverse, "nit"? Use the *left* L-key instead, and
then the right I-key and the right T-key. If you mirror the keys you press, the
output word is also mirrored!

![Typing "nit" instead](https://raw.githubusercontent.com/kqr/qweyboard/master/images/nit.png)

You see how the output word is produced by the Qweyboard according to the order
of the keys on the keyboard, not according to which order you pressed them in.


### Secondary Letters

Some keys have two or more letters on them. The big central letters are the ones
normally produced by the key. The other letters are produced by holding down
several keys simultaneously. The most common of these are the "J combinations".

As you can see, the J keys in the image have a little circle in the upper right
corner. This circle indicates that if you combine the J key with any other key
on the same side that has a small letter in the upper right, that letter will be
produced instead. The most common example of this is probably how hitting both
J and T on the same side will produce the letter D.

In other words, to type a word like "does", you'll press T J O E S, where the T
J combination represents the letter D. See the image below!

TODO: add image here

The same thing goes for R, except then we're talking about the lower right
corner. A common example is probably R and N, which produces an M.

There are three special cases. These are indicated on the image by small letters
in the lower *left* corner. These have to be remembered, and they are:

* O + I = A
* F + C = Q
* K + Z = X

The O+I combination may surprise you, but it's there because it makes a lot of
words much more convenient. A is a common vowel.


### Multi-syllable Words

While most words in regular text consist of single syllables (for example, 77%
of words in this paragraph), there are plenty of multi-syllable words too. As
you have seen in your experiments, the Qweyboard automatically inserts a space
after you have typed a syllable, since this is the most common case.

If you want to type a word that consists of multiple syllables, like "splendid",
you'll begin by typing the first syllable normally, and then to join the second
syllable to the first, you hold down the "nospace" key simultaneously to typing
the second syllable. So the final word consists of two chords, the latter
combined with the nospace key.


### Period and Comma

If you press the "nospace" key on its own, you get a single space
character. Other keys that behave this way are the right J and O keys, which,
when used completely on their own in a chord, produces a comma and a period,
respectively. So when you want to finish a sentence, you press the right O key
alone.


### Language-specific Substitutions

Sometimes the order of keys on the Qweyboard isn't optimal for the language
that is being typed. For example, in English, [stuff about subs]


### To be continued...

This is all I'm going to type up at the moment. Feel free to ask questions
and play around.



Language Variants
-----------------

langauges directory. standard is only an example. standard is not english. english needs to be added

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

* `[ ]` explain: Languages based on substitutions rather than dictionaries

* `[ ]` explain: Standard layout does not contain all optimisations for english. Standard is
  meant to be a compromise of as many languages as possible

* `[ ]` explain: Qweyboard does not have a convenient H key, so some accomodations need to be made

* `[x]` Q on C, V on F, E on RF

* `[x]` Order inits: L and N last. Order tails: L and N first!

* NEW DISCOVERIES AFTER READING THE PATENT!!

* `[x]` FJ=V

* `[x]` LC LR RR RC = backspace

* `[ ]` Init substitutions

    * [only in languages where its needed?] ZC=Ccedilla?

    * [english only, not standard]
        * ZV = ZW
        * TV = TW
        * DV = DW

* `[ ]` tails substitutions

    * SZ=STS
    * YZ=YS
    * NZ=NS
    * DZ=DS
    * MZ=MS
    * TZ=TS
    * SJ=SS
    * CZ=CH
    * CS=SH
    * DZ=TH
    * BF=BE
    * CF=CE
    * DF=DF (etc consonants)
    * KP=RP
    * KC=CK
    * GZ=GT
    * PZ=PT
    * FZ=FT
    * NC=GN

    * [english only, not standard]
        * GDF=DGE
        * GHZ=GHT
        * IESZ=IESS (?)
        * BZ=BLE (doabz=doable)
        * KT=CT
        * AJ=AY
        * OJ=OY
        * UJ=UY
        * EJ=EY
        * IJ=Y
        * ANJ=ANY

* `[ ]` [swedish] LO = Ä, but LO+RO=OO: ÄO=OO substitution

* `[ ]` LZ RZ = return

* `[ ]` shift = sssssymbol keys? actually trickier than I had thought...
     insight!! don't use the actual shift key on the keyboard dummy...

* `[ ]` Actually read layout files...

* `[ ]` do something about memory leaks

* `[ ]` allow capitalisation somehow. perhaps find a spot for the MCAP key?

* `[ ]` Unicodify the whole project. Painfully dependent on iso-8859-1 right now

* `[ ]` come up with a way to define shortcuts on the fly?

* `[ ]` Escape shortcut should be configurable with something like `-e <escape shortcut>`?

* `[ ]` Get a better picture of key presses and releases (to be able to detect
  things such as double tapping? or long presses?)

