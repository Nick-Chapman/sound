# sound

Explore sound generation using SDL.

Starting with simple example from [Frederico Bittencourt](https://blog.fredrb.com/2023/08/08/audio-programming-note-sdl).

Mod to allow playing small chord of 3 notes. Under control of keys `1`, `2` and `3`.
```
$ jenga run c/synth
```

New example: Basic `.wav` player. Bit like `aplay`
```
$ jenga run c/myplay -- happy.wav
$ aplay happy.wav
```
