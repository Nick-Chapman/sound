#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>

typedef struct {
  unsigned tick;
  float last_current_step;
  float last_current_stepA;
  float current_step;
  float current_stepA;
  float step_size;
  float volume;
} oscillator;

static oscillator oscillate(float rate, float volume) {
  oscillator o = {
      .tick = 0,
      .current_step = 0,
      .volume = volume,
      .step_size = (2 * M_PI) / rate,
  };
  return o;
}

static float next(oscillator *os) {
  os->tick++;
  os->current_step += os->step_size;
  os->current_stepA = os->tick * os->step_size;
  return SDL_sinf(os->current_step) * os->volume; //orig: has audible glitches
  //return SDL_sinf(os->current_stepA) * os->volume; //better!
}

static void audio_callback(oscillator *os, float* fstream, int len) {
  const unsigned n = len / sizeof(float);
  static unsigned cb;
  printf("%d: callback: len=%d, n=%d -- %f (%f) %f (%f) \n", ++cb, len, n,
         os->current_step,
         os->current_step - os->last_current_step,
         os->current_stepA,
         os->current_stepA - os->last_current_stepA
         );
  os->last_current_step = os->current_step;
  os->last_current_stepA = os->current_stepA;
  for (int i = 0; i < n; i++) {
    float v = next(os);
    fstream[i] = v;
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

const int sample_rate = 44100;
const float my_note = 523;

int main() {
  assert (0 == SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS));
  const float rate = (float) sample_rate / my_note;
  oscillator osc = oscillate(rate, 0.8f);
  const int number_samples = 4096;
  SDL_AudioSpec spec = {
      .format = AUDIO_F32,
      .channels = 1,
      .freq = sample_rate,
      .samples = number_samples,
      .callback = (void*)audio_callback,
      .userdata = &osc,
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
