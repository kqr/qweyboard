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

The software takes a few command-line switches that configure its behaviour.

    Usage: ./bin/qweyboard [OPTION]
    
    [OPTION] is any combination of the following options:
    
        -l <language file>   : Modifies the standard layout with the
                               key mappings indicated in the specified
                               language file.
    
        -t <milliseconds>    : Set the timeout for what counts as one
                               stroke. If you want 0, NKRO is strongly
                               recommended. Default value is 500, which
                               is probably way too high.
    
        -d <dictionary file> : Specifies a file of abbreviations to use.
    
        -v,-vv,-vvv          : Sets the log level of the software. If you
                               want to know what goes on inside, this is
                               where to poke...



Dependencies
------------

If Qweyboard doesn't want to run or build (undefined symbol, linker errors or
whatnot), install the prerequisite dependencies:

 * GNAT (Ada compiler; Ada is compiled with GCC but the GNAT frontend helps)
 * GPRBuild (should come with GNAT; alert me if it does not)
 * Xlib (X11 client programming library)
 * XInput2 (support for more advanced input in X11)
 * XTest (extension that enables clients to create input events as if from a real device)

On Debian, these are installed with

    $ sudo apt-get install gnat libx11-6 libx11-dev libxtst-6 libxtst-dev libxi-6 libxi-dev


Short tutorial
--------------

This is a short tutorial on how to use the Qweyboard. The Qweyboard is easy to
learn, because it is a lot like a regular QWERTY keyboard in that each key
produces a letter in the final word. There are three main differences to your
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

![Qweyboard standard layout image](https://raw.githubusercontent.com/kqr/qweyboard/master/images/standard-layout.png)

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

In other words, to type a word like "does", you'll press T J O E S, where the
T+J combination represents the letter D. See the image below!

![Typing "does" through the T+J combination](https://raw.githubusercontent.com/kqr/qweyboard/master/images/does.png)

The same thing goes for R, except then we're talking about the lower right
corner. A common example is probably R and N, which produces an M.

There are three special cases. These are indicated on the image by small letters
in the lower *left* corner. These have to be remembered, and they are:

* O + I = A
* F + C = Q
* K + Z = X

The O+I combination may surprise you, but it's there because it makes a lot of
words much more convenient. A is a common vowel. With this, you can type
a word like "stand" without moving your fingers at all.

![Typing "stand" including the O+I combination](https://raw.githubusercontent.com/kqr/qweyboard/master/images/stand.png)


### Multi-syllable Words

As you have discovered in your experiments, the Qweyboard automatically inserts
a space after each syllable. This is because in regular writing, most words by
far (for example, 77% of the words in this paragraph) are single-syllable words.
The Qweyboard optimises for this most common case, relieving you of pressing
the most pressed key on other keyboards.

However, there *are* words which consist of multiple syllables. To type these
on the Qweyboard, you start by typing the first syllable normally, and let the
Qweyboard insert the space. Then as you type the second syllable, also hold
down the "nospace" key. The "nospace" key will attach the new syllable to the
last one.

So a two-syllable word like "splendid" is typed with two "strokes" or "chords":
first "splen", and then, in conjunction with the space key, "did". See the
images below for each.

![Typing "splen", the first syllable of "splendid"](https://raw.githubusercontent.com/kqr/qweyboard/master/images/splen.png)
![Typing "did" in conjunction with the nospace key to complete "splendid"](https://raw.githubusercontent.com/kqr/qweyboard/master/images/did-nospace.png)

A word of more than two syllables are just continued in the same fashion. Each
additional syllable is typed in conjunction with the nospace key.

> Of course, if your keyboard doesn't support typing with the space key pressed,
you can simply press the space key once at some point during the syllable. It
doesn't matter if it's before, after, or somewhere in the middle.


### Space, Period and Comma

If you press the "nospace" key on its own, you get a single space
character.

Two other keys behave this way. They are the right J and O keys, which, when
used completely alone in a "one-key chord", produces a comma and a period,
respectively. So when you want to finish a sentence, you press the right O key
on its own.


### Language-specific Substitutions

Given that the Qweyboard only has 32 letter keys, we're bound to encounter
situations when the order of keys doesn't cut it. There are several different
kinds of these cases in English:

1. Words normally start with the digraph "sp" in that order: "special",
"spring", "sponsor" and so on. However, when the
origin of the word is Greek, the reverse digraph can appear: psychology, psalm,
psoriasis. "Psalm" is a one-syllable word, but going just by the key order on
the Qweyboard, it takes three syllables to write: "p-sal-m".

   To make these cases easier to handle, the Qweyboard can substitute the letter
combination "ZP" – when it occurs in the beginning of a word – with the letter
combination "PS". So to type "psa" more easily on the Qweyboard, you actually
press the keys for "zpa".

   (Another of these substitutions takes care of the end of the word so you can
actually type "psalm" in one stroke.)

2. Another common case is when the key order of letters *never* occur in common
writing. In English, this is the case with the letter combination "kc" at the
end of a word. A word like "rock" is one syllable, but requires two syllables
according to Qweyboard key order: "roc-k".

   However, the Qweyboard will substitute the letter combination "KC" at the end of
a word for the more commonly occurring "CK", so if you type "rokc" in one
stroke, you will get "rock".

3. Sometimes a certain common combination of letters is impossible, given the
plain Qweyboard. This is the case with the "th" combination, for example. If
you press the J+L combination to get an H, then the J key is pressed and thus
the T key will produce the letter D, giving you the output "dh". It's
impossible to type both T and H at the same time, since they both require
different states of the J key.

   The Qweyboard solves this by substituting the letter combination "DH" at the
start of a word for the much more common "TH".

4. Another reason the Qweyboard may substitute a letter combination is simply
for convenience. The letter combination "rp" is somewhat common at the end of a
word, like in "chirp". However, actually pressing the R and the P key
simultaneously requires you to strain your fingers quite a bit. We realise that
the letter combination "kp" is much more rare, so we repurpose that: "KP" at
the end of a word is replaced by "RP" – a much more convenient chord.

5. The last reason to perform substitutions can be grammar. It's common for
words to end in something like "de", as in "tide". By spelling, this is two
syllables: "ti-de" or "tid-e". By sound, it's just one. The Qweyboard helps
you add the "final e" by replacing the letter combination "DF" with "DE".

   The F key is so commonly used this way as a "final e" that this purpose is
marked on the key in the images you have seen above.

   This can be made even more advanced when you target a specific language. The
"ble" ending (as in "fallible", "culpable", "malleable") is common enough in
English that an English language variant of the Qweyboard can let you type the
"ble" ending by substituting the "BZ" combination at the end of a word.

These are the most common cases that warrant substitutions. If you can come up
with your own, you are of course free to use them by creating your own "language
variant". In fact, substitutions are such an important part of language variants
that you should read the entire section on them below.


### Special Characters and Symbols

The goal is to have a shift key that, when used in a chord, turns all letters
into symbols instead, according to the language variant. This is not yet
implemented.


### Capitalisation

The goal is to have auto-capitalisation at the start of new sentences, with the
MCAP key to override it, very similar to the "nospace" key. This is not yet
implemented.


### About the H Key

The real Velotype/Veyboard has the letter H on one of the palm keys. This makes
some key combinations with H much more convenient, and as such it is used in a
bunch of places where you wouldn't even expect the letter H. Since a regular
keyboard has no palm key at all, I have simply thrown out the idea of a H key
for the time being. This means some concessions need to be made in terms of
key combinations and such. I welcome all and any advice.


Language Variants
-----------------

Since typing can be made so much faster and easier once you know about the
language that is being written, the Qweyboard will - if you ask it – read in a
"language variant" that customises its behaviour slightly.

A "language variant" consists of two things:

* A key mapping, linking keys to the symbols (often letters) they produce, and
* A substitute mapping, linking sequences of letters to other sequences of
  letters they should be replaced with.

These two things both go in a file called a "Qweyboard language variant
definition" file, which has a special format. If you want to create one of these
definitions, you should look at the existing files in the `languages` directory
for inspiration. I can't promise it's exactly correct, but I've tried to make
the format something like what is described here:
https://github.com/kqr/qweyboard/blob/master/src/languages_parser.ads#L11-L21

There is a "standard Qweyboard", which is what you get if you don't specify any
language variant at all. This standard is hard-coded into the program and cannot
be changed. (Which implies, correctly, that the `languages/standard_example.qrd`
file is just an example – it is not actually read by the program unless you
specifically ask it to.)

Any language variant you choose will be applied *on top of* the standard. This
means language variants contain changes and additions to the standard, they are
not built from a blank slate. (And currently it's not possible to do so, either.
This can be discussed, of course.) If you need to "remove" a substitution
because it's bad for your language, you can simply redefine it to equal
itself. (I.e. if your language file says "ZP=ZP" then that will override the
standard "ZP=PS" definition.)

It's important to note here that *standard is not English*. Standard is my
attempt at a *compromise* that will work reasonably well for as many people
as possible. If you speak a somewhat common language that is somewhat similar
to English, you should be able to have a decent experience with the "standard"
Qweyboard. You can get a *better* experience by creating a variant for your
language, of course, but the standard should be good enough to get started.

There is at the moment *no* English variant that ships with the Qweyboard.
The reason for this is simple: I use the Swedish variant also for English,
so I am not the right person to maintain an English variant. I have however
collected a couple of completely arbitrary tips for anyone who wants to
create an English version, which can be found in the `languages` directory.

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

* `[ ]` LZ RZ = return

* `[ ]` shift = sssssymbol keys? actually trickier than I had thought...
     insight!! don't use the actual shift key on the keyboard dummy...

* `[ ]` do something about memory leaks

* `[ ]` allow capitalisation somehow. perhaps find a spot for the MCAP key? auto-caps after sentence end

* `[ ]` Escape shortcut should be configurable with something like `-e <escape shortcut>`?

* `[ ]` Get a better picture of key presses and releases (to be able to detect
  things such as double tapping? or long presses?)

