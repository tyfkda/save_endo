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

  struct {
    unsigned char* image;
    int width, height;
  } img[2];

  const char* fns[] = { argv[1], argv[2] };
  for (int i = 0; i < 2; ++i) {
    img[i].image = LoadPng(fns[i], &img[i].width, &img[i].height);
    if (img[i].image == NULL) {
      return 1;
    }
  }
  if (img[0].width != img[1].width || img[0].height != img[1].height) {
    cerr << "Image sizes are different." << endl;
  }

  int risk = CompareImage(img[0].image, img[1].image,
                          img[0].width, img[0].height);
  SavePng("compared.png", img[0].width, img[0].height, img[0].image);
  cout << risk << endl;

  return 0;
}
