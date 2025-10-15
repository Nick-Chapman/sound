#include "SDL2/SDL_audio.h"
#include "SDL2/SDL_timer.h"
#include <SDL2/SDL.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>

const int SAMPLE_RATE = 44100;
const int BUFFER_SIZE = 4096;

const float A4_OSC = (float)SAMPLE_RATE / 440.00f;

FILE *plot_output;

typedef struct {
  float current_step;
  float step_size;
  float volume;
} oscillator;

oscillator oscillate(float rate, float volume) {
  oscillator o = {
      .current_step = 0,
      .volume = volume,
      .step_size = (2 * M_PI) / rate,
  };
  return o;
}

float next(oscillator *os) {
  float ret = SDL_sinf(os->current_step);
  os->current_step += os->step_size;
  return ret * os->volume;
}

oscillator *A4_oscillator;

void oscillator_callback(void *userdata, Uint8 *stream, int len) {
  float *fstream = (float *)stream;
  for (int i = 0; i < BUFFER_SIZE; i++) {
    float v = next(A4_oscillator);
    fstream[i] = v;
#ifdef PLOT
    fprintf(plot_output, "%.5f\n", fstream[i]);
#endif
  }
}

void close() {
#ifdef PLOT
  fclose(plot_output);
#endif
}

int main() {

  if (SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS) < 0) {
    printf("Failed to initialize SDL: %s\n", SDL_GetError());
    return 1;
  }

#ifdef PLOT
  plot_output = fopen("plot_output", "w");
  if (plot_output == NULL) {
    printf("Failed to open file: %d", errno);
    return 1;
  }
#endif

  oscillator a4 = oscillate(A4_OSC, 0.8f);
  A4_oscillator = &a4;

  SDL_AudioSpec spec = {
      .format = AUDIO_F32,
      .channels = 1,
      .freq = SAMPLE_RATE,
      .samples = 4096,
      .callback = oscillator_callback,
  };

  if (SDL_OpenAudio(&spec, NULL) < 0) {
    printf("Failed to open Audio Device: %s\n", SDL_GetError());
    return 1;
  }

  SDL_PauseAudio(0);

  while (true) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        close();
        return 0;
      }
    }
  }

  close();
  return 0;
}
