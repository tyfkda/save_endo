#include "image_builder.h"
#include "rope.h"
#include "util.h"
#include <fstream>
#include <iostream>

using namespace std;

int main() {
  ImageBuilder image_builder;
  while (!cin.eof()) {
    char line[256];
    cin.getline(line, sizeof(line));
    image_builder.Step(Rope(line));
  }

  SavePpm("image.ppm", image_builder.GetBitmap(), Bitmap::W, Bitmap::H);

  return 0;
}
