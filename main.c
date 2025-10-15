
#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>

typedef struct {
  float current_step;
  float step_size;
  float volume;
} oscillator;

static oscillator oscillate(float rate, float volume) {
  oscillator o = {
      .current_step = 0,
      .volume = volume,
      .step_size = (2 * M_PI) / rate,
  };
  return o;
}

static float next(oscillator *os) {
  float ret = SDL_sinf(os->current_step);
  os->current_step += os->step_size;
  return ret * os->volume;
}

static oscillator *the_oscillator;

static void oscillator_callback(void *userdata, Uint8 *stream, int len) {
  const unsigned n = len / sizeof(float);
  //static unsigned cb;
  //printf("%d: callback: len=%d, n=%d\n", ++cb, len, n);
  float *fstream = (float *)stream;
  for (int i = 0; i < n; i++) {
    float v = next(the_oscillator);
    fstream[i] = v;
  }
  static bool first_time = true;
  if (first_time) {
    first_time = false;
    FILE *plot_output = fopen("plot_output", "w");
    if (plot_output == NULL) {
      printf("Failed to open file: %d", errno);
      exit(1);
    }
    for (int i = 0; i < n; i++) {
      fprintf(plot_output, "%.5f\n", fstream[i]);
    }
    fclose(plot_output);
  }
}

const int sample_rate = 44100;
const int number_samples = 4096;
const float my_note = 523;

int main() {

  if (SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS) < 0) {
    printf("Failed to initialize SDL: %s\n", SDL_GetError());
    return 1;
  }

  const float rate = (float) sample_rate / my_note;
  oscillator a4 = oscillate(rate, 0.8f);
  the_oscillator = &a4;

  SDL_AudioSpec spec = {
      .format = AUDIO_F32,
      .channels = 1,
      .freq = sample_rate,
      .samples = number_samples,
      .callback = oscillator_callback,
  };

  if (SDL_OpenAudio(&spec, NULL) < 0) {
    printf("Failed to open Audio Device: %s\n", SDL_GetError());
    return 1;
  }

  SDL_PauseAudio(0);

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
  return 0;
}
