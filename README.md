# sound

Explore sound generation using SDL.

Starting with simple example from [Frederico Bittencourt](https://blog.fredrb.com/2023/08/08/audio-programming-note-sdl).

Mod to allow playing small chord of 3 notes. Under control of keys `1`, `2` and `3`.
```
$ jenga run c/synth
```

Next example: Basic `.wav` player. Bit like `aplay`.
```
$ jenga run c/myplay -- sounds/happy.wav
$ aplay sounds/happy.wav
```

Haskell code: sample rate halving:
```
$ cp sounds/beat.wav g1
$ jenga run run g1 g2
$ ls -l g1 g2
$ aplay g1 g2
```

More processing modes:
```
$ cat sounds/beat.wav | (begin|mono|8bit|hr|rep 2|aplay)
```
