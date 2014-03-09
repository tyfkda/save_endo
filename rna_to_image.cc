#include "image_builder.h"
#include "rope.h"
#include "util.h"
#include <fstream>
#include <iostream>
#include <unistd.h>  // for getopt

using namespace std;

Bitmap CalcLowerLayer(const ImageBuilder& image_builder) {
  size_t nlayer = image_builder.GetBitmapCount();
  if (nlayer <= 2) {
    return image_builder.GetBitmap(0);
  }
  Bitmap tmp;
  for (size_t layer = nlayer - 2; ; --layer) {
    Bitmap target(image_builder.GetBitmap(layer - 1));
    const Bitmap* src = &image_builder.GetBitmap(layer);
    if (layer < nlayer - 2) {
      src = &tmp;
    }
    target.Compose(*src);
    if (layer == 1) {
      return target;
    }
    tmp = target;
  }
}

const Bitmap Merge(const Bitmap& lower_layer, const Bitmap& bitmap) {
  Bitmap target;
  for (int i = 0; i < Bitmap::H; ++i) {
    for (int j = 0; j < Bitmap::W; ++j) {
      const Pixel& p0 = lower_layer.GetPixel(j, i);
      const Pixel& p1 = bitmap.GetPixel(j, i);
      if (!(p1.rgb.r == 0 && p1.rgb.g == 0 && p1.rgb.b == 0 && p1.a == 0)) {
        target.SetPixel(j, i, p1);
      } else {
        target.SetPixel(j, i, p0);
      }
    }
  }
  return target;
}

int main(int argc, char* argv[]) {
  const char* filename = "image.png";
  bool repeat = false;
  int step = 1;

  int ch;
  while ((ch = getopt(argc, argv, "o:rs:")) != -1) {
    switch (ch) {
    case 'o':
      filename = optarg;
      break;
    case 'r':
      repeat = true;
      break;
    case 's':
      step = atoi(optarg);
      if (step < 1)
        step = 1;
      break;
    default:
      break;
    }
  }

  ImageBuilder image_builder;
  int no = 0;
  size_t bitmap_count = 0;
  Bitmap lower_layer;
  for (int lineno = 1;; ++lineno) {
    char line[256];
    if (!cin.getline(line, sizeof(line)))
      break;
    image_builder.Step(Rope(line));

    if (repeat) {
      const char* targets[] = {
        "PFFICCP",
        "PIIPIIP",
        "PCCPFFP",
        "PFFPCCP",
        "PFFICCF",
      };
      bool updated = false;
      for (size_t i = 0; i < sizeof(targets) / sizeof(*targets); ++i) {
        if (strcmp(line, targets[i]) == 0) {
          updated = true;
          break;
        }
      }
      if (updated) {
        if (no % step == 0) {
          if (bitmap_count != image_builder.GetBitmapCount()) {
            bitmap_count = image_builder.GetBitmapCount();
            if (bitmap_count > 1) {
              lower_layer = CalcLowerLayer(image_builder);
            }
          }

          cerr << "#" << no << "/" << lineno << ":" << line << endl;

          char fn[256];
          sprintf(fn, filename, no / step);
          if (bitmap_count <= 1) {
            SavePng(fn, Bitmap::W, Bitmap::H, image_builder.GetLastBitmap());
          } else {
            SavePng(fn, Bitmap::W, Bitmap::H, Merge(lower_layer, image_builder.GetLastBitmap()));
          }
        } else {
          if (bitmap_count != image_builder.GetBitmapCount()) {
            bitmap_count = 0;  // Force update next.
          }
        }
        ++no;
      }
    }
  }

  if (!repeat) {
    SavePng(filename, Bitmap::W, Bitmap::H, image_builder.GetLastBitmap());
  } else {
    char fn[256];
    sprintf(fn, filename, no / step + 1);
    SavePng(fn, Bitmap::W, Bitmap::H, image_builder.GetLastBitmap());
  }

  return 0;
}
