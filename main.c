#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>

const int sample_rate = 44100;

// minor chord
const float note1 = 523.3; //C
const float note2 = 622.3; //Eb  (659.3 E)
const float note3 = 784.0; //G

const float volume = 0.3f;

static unsigned tick = 0;

static void audio_callback(void *userdata, Uint8* stream, int len) {
  float* fstream = (float*)stream;
  const unsigned n = len / sizeof(float);
  static unsigned cb;
  printf("%d: callback: len=%d, n=%d (%d)\n", ++cb, len, n, tick);
  for (int i = 0; i < n; i++) {
    tick++;
    const float f = tick * (2 * M_PI) / sample_rate;
    const float amp = SDL_sinf(note1 * f) + SDL_sinf(note2 * f) + SDL_sinf(note3 * f);
    fstream[i] = amp * volume;
  }
  static bool first_time = true;
  if (first_time) {
    first_time = false;
    FILE *file = fopen("plot_output", "w");
    assert(file);
    for (int i = 0; i < n; i++) {
      fprintf(file, "%.5f\n", fstream[i]);
    }
    fclose(file);
  }
}

int main() {
  assert (0 == SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS));
  const int number_samples = 4096;
  SDL_AudioSpec spec = {
      .format = AUDIO_F32,
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
      }
    }
  }
  printf("\nquitting!\n");
  return 0;
}
