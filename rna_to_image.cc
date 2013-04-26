#include "image_builder.h"
#include "rope.h"
#include "util.h"
#include <fstream>
#include <iostream>

using namespace std;

int main() {
  ImageBuilder image_builder;
  for (;;) {
    char line[256];
    if (!cin.getline(line, sizeof(line)))
      break;
    image_builder.Step(Rope(line));
  }

  SavePpm("image.ppm", image_builder.GetBitmap(), Bitmap::W, Bitmap::H);

  return 0;
}
