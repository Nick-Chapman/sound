#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>

const int sample_rate = 44100;

const float volume = 1.;

static unsigned tick = 0; // counter for audio samples

// Mode A/B -- 3 places to keep in sync
//typedef float SAMPLE; //A
typedef Sint16 SAMPLE; //B

static void audio_callback(void *userdata, Uint8* stream8, int len) {
  SAMPLE* stream = (SAMPLE*)stream8;
  const unsigned n = len / sizeof(SAMPLE);
  static unsigned cb;
  printf("%3d: callback: len=%d, n=%d (%6d) : \n", ++cb, len, n, tick);
  fflush(stdout);
  for (int i = 0; i < n; i++) {
    tick++;
    const float phase = tick * (2 * M_PI) / sample_rate;
    const float amp = SDL_sinf(440.0 * phase) * volume;
    //stream[i] = amp; //A
    stream[i] = amp * 32768; //B
  }
}

int main() {
  assert (0 == SDL_Init(SDL_INIT_EVERYTHING));
  const int number_samples = 4096;
  SDL_AudioSpec spec = {
    //.format = AUDIO_F32, //A
    .format = AUDIO_S16LSB, //B
    .channels = 1,
    .freq = sample_rate,
    .samples = number_samples,
    .callback = audio_callback
  };
  assert (0 == SDL_OpenAudio(&spec, NULL));
  SDL_PauseAudio(0); //unpause
  bool quit = false;
  while (!quit) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        quit = true;
        break;
      }
    }
    SDL_Delay(10);
  }
  printf("\nquitting!\n");
  return 0;
}
