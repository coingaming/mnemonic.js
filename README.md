# mnemonic.js

`mnemonic.js` creates arbitrary strength radom pass-phrases or hexadecimal uids which you *can* actually remember.

## Huh? My passwords are fine.

Probably not. You are passwords are either not random, or they are not long enough. Ok, maybe you are one of those people who **really** have a strong memory, but most of are not.

Strong, secure passwords need to have an entropy of at least ~80 bits. Here are some common cases to what that corresponds in real life:

 * If we consider trully random passwords of the entire ASCII printable set, we get an entropy of about 6.5 bits/character. That means, you need about 12 randomly chosen characters among the entire set to be ok. Not easy.

 * When you use non-random passwords your entropy/character reduces dramatically. For english, it's about 2 bits/character. You need a lot of characters to get there! And no, adding 1 at the end or using *1337* speak does not help much.

## I am sold. How does this work?

`mnemonic.js` uses a relatively small (1626 to be exact) set of english words that are chosen among the [list](http://en.wiktionary.org/wiki/Wiktionary:Frequency_lists/Contemporary_poetry) of frequently used words in contemporary english poetry and are (hopefully) memorable.

To generate a passphrase, a sequence of random 32-bit unsigned integers is generated. The bigger the range of the sequence the stronger it is. This sequence is then transformed to a list of words from the dictionary, 3 words per integer. The function that transforms the integer `x~i~` to the indexes `w~i~` of the words is the following (1:1 and reversible) function:

    w~i,1~ = x mod n
    w~i,2~ = (x / n + w~i,1~) mod n
    w~i,3~ = (x / n^2 + w~i,2~) mod n

where `n=1626` is the number of words. Repeating for each integer, gives you the sequence of words:

    words = [ dict[w~i,1~], dict[w~i,2~], dict[w~i,3~] for each i ]

In code, to generate a 96-bit password, i.e. 9 words generated from 3 random 32-bit unsigned integers,

```javascript

    m = new Mnemonic(96);
    m.toWords();
    ["grey", "climb", "demon", "snap", "shove", "fruit", "grasp", "hum", "self"]

```

If you wanna see the random sequence, or the the 96-bit number in hexadecimal,

```javascript

    m.random
    [174975897, 171815469, 1859322123]

    m.toHex();
    "0a6deb990a3db22d6ed3010b"

```

## AMD loading

under18n will register as an anonymous module if you use [requireJS](http://requirejs.org/).
