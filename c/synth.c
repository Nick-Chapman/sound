#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>

const int sample_rate = 44100;

// minor chord
const float note1 = 523.3; //C
const float note2 = 622.3; //Eb  (659.3 E)
const float note3 = 784.0; //G
//const float note3 = 1.5 * note1;

static bool press1 = false;
static bool press2 = false;
static bool press3 = false;

static unsigned tick = 0; // counter for audio samples
static unsigned eloop = 0; // counter for main event-handling loop (just for info)

static void audio_callback(void *userdata, Uint8* stream, int len) {
  float* fstream = (float*)stream;
  const unsigned n = len / sizeof(float);
  static unsigned cb;
  printf("%3d: callback: len=%d, n=%d (%6d) %4d : %c%c%c\r", ++cb, len, n, tick, eloop,
         press1 ? '1' : '.',
         press2 ? '2' : '.',
         press3 ? '3' : '.'
         );
  fflush(stdout);
  for (int i = 0; i < n; i++) {
    tick++;

    const float vol1 = press1 ? 0.3 : 0;
    const float vol2 = press2 ? 0.3 : 0;
    const float vol3 = press3 ? 0.3 : 0;

    const float f = tick * (2 * M_PI) / sample_rate;
    fstream[i] =
      SDL_sinf(note1 * f) * vol1 +
      SDL_sinf(note2 * f) * vol2 +
      SDL_sinf(note3 * f) * vol3;
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
  assert (0 == SDL_Init(SDL_INIT_EVERYTHING));
  const int number_samples = 1024; //4410; //5000; //4096;
  SDL_AudioSpec spec = {
      .format = AUDIO_F32,
      .channels = 1,
      .freq = sample_rate,
      .samples = number_samples,
      .callback = audio_callback
  };
  assert (0 == SDL_OpenAudio(&spec, NULL));
  SDL_PauseAudio(0); //unpause

  SDL_Window* window = SDL_CreateWindow("sound", 0,0, 200, 0, 0);
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, 0);
  SDL_SetRenderDrawColor(renderer, 30, 30, 30, 255);
  SDL_RenderClear(renderer);
  SDL_RenderPresent(renderer);

  bool quit = false;
  while (!quit) {
    eloop++;
    //printf(".");
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        quit = true;
        break;
      case SDL_KEYDOWN: {
        int sym = e.key.keysym.sym;
        //printf("down: sym=%d ('%c')\n",sym,sym);
        switch (sym) {
        case SDLK_ESCAPE:
        case 'q':
          printf("escape or q\n");
          quit = true;
          break;
        case '1': press1 = true; break;
        case '2': press2 = true; break;
        case '3': press3 = true; break;
        }
        break;
      }
      case SDL_KEYUP: {
        int sym = e.key.keysym.sym;
        //printf("up: sym=%d ('%c')\n",sym,sym);
        switch (sym) {
        case '1': press1 = false; break;
        case '2': press2 = false; break;
        case '3': press3 = false; break;
        }
        break;
      }
      }
    }
    SDL_Delay(10);
  }
  printf("\nquitting!\n");
  return 0;
}
