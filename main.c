#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>

const int sample_rate = 44100;
const float my_note = 523;
const float rate = sample_rate / my_note;
const float step_size = (2 * M_PI) / rate;
const float volume = 0.8f;

static unsigned tick = 0;

static void audio_callback(void *userdata, Uint8* stream, int len) {
  float* fstream = (float*)stream;
  const unsigned n = len / sizeof(float);
  static unsigned cb;
  printf("%d: callback: len=%d, n=%d (%d)\n", ++cb, len, n, tick);
  for (int i = 0; i < n; i++) {
    tick++;
    float current_step = tick * step_size;
    float amp = SDL_sinf(current_step) * volume;
    fstream[i] = amp;
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
