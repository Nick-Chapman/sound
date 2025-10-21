#include <SDL2/SDL.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>
#include <sys/stat.h>

typedef Sint16 SAMPLE;

static unsigned tick = 0; // counter for audio samples

static unsigned count_samples;
static SAMPLE* sample_data;

static void audio_callback(void *userdata, Uint8* stream8, int len) {
  SAMPLE* stream = (SAMPLE*)stream8;
  const unsigned n = len / sizeof(SAMPLE);
  //static unsigned cb;
  //printf("%3d: callback: len=%d, n=%d (%6d) : \n", ++cb, len, n, tick);
  fflush(stdout);
  for (int i = 0; i < n; i++) {
    tick++;
    if (tick < count_samples) {
      stream[i] = sample_data[tick + 22]; //wav header is 44 bytes
    } else  {
      stream[i] = 0;
    }
  }
}

unsigned bytesize_of_file(FILE* file) {
  int fd = fileno(file);
  struct stat stat_buf;
  fstat(fd, &stat_buf);
  return stat_buf.st_size;
}

int main(int argc, char* argv[]) {

  //char* filename = "happy.wav";
  char* filename = argv[1];
  printf("reading file: %s\n", filename);
  FILE *file = fopen(filename, "r");
  assert(file);

  count_samples = bytesize_of_file(file) / sizeof(SAMPLE);
  sample_data = malloc(count_samples * sizeof(SAMPLE));
  assert (count_samples == fread(sample_data,sizeof(SAMPLE),count_samples,file));

  const Uint16 number_channels = *(Uint16*)((char*)sample_data+22);
  printf("number_channels = %d\n",number_channels);

  const Uint32 sample_rate = *(Uint32*)((char*)sample_data+24);
  printf("sample_rate = %d\n",sample_rate);

  const float duration_s = (float)count_samples / sample_rate / number_channels;
  printf("duration = %0.1fs\n",duration_s);

  assert (0 == SDL_Init(SDL_INIT_EVERYTHING));

  const unsigned batch_size = 4096;

  SDL_AudioSpec spec = {
    .format = AUDIO_S16LSB,
    .channels = number_channels,
    .freq = sample_rate,
    .samples = batch_size,
    .callback = audio_callback
  };
  assert (0 == SDL_OpenAudio(&spec, NULL));
  SDL_PauseAudio(0); //unpause
  bool quit = false;
  while (!quit && tick < count_samples+batch_size) {
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
  printf("quitting!\n");
  return 0;
}
