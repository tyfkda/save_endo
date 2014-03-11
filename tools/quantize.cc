#include "util.h"
#include <iostream>
#include <png.h>
using namespace std;

int CompareImage(unsigned char* image1, unsigned char* image2,
                 int width, int height) {
  int risk = 0;
  for (int i = 0; i < width * height; ++i) {
    unsigned char* p = &image1[i * 3];
    unsigned char* q = &image2[i * 3];
    int r = p[0] - q[0];
    int g = p[1] - q[1];
    int b = p[2] - q[2];
    if (r == 0 && g == 0 && b == 0) {
      p[0] = p[1] = p[2] = 255;
    } else {
      ++risk;
      int diff = min(abs(r) + abs(g) + abs(b), 255);
      p[0] = diff;
      p[1] = p[2] = 0;
    }
  }
  return risk;
}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cerr << "2 filename required." << endl;
    return 1;
  }

  int width, height;
  unsigned char* image = LoadPng(argv[1], &width, &height);
  if (image == NULL) {
    cerr << "Load " << argv[1] << " failed." << endl;
    return 1;
  }

  // Modify color.
  for (int i = 0; i < height; ++i) {
    for (int j = 0; j < width; ++j) {
      unsigned char* p = &image[(i * width + j) * 3];
      p[0] = (p[0] & 1) ? 255 : 0;
      p[1] = (p[1] & 1) ? 255 : 0;
      p[2] = (p[2] & 1) ? 255 : 0;
    }
  }

  SavePng(argv[2], width, height, image);

  return 0;
}
