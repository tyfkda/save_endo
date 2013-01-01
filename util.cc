#include "util.h"

#include "rope.h"
#include "image_builder.h"
#include <assert.h>
#include <iostream>

using namespace std;

Color Color::black   = { {   0,   0,   0 }, 1 };
Color Color::red     = { { 255,   0,   0 }, 1 };
Color Color::green   = { {   0, 255,   0 }, 1 };
Color Color::yellow  = { { 255, 255,   0 }, 1 };
Color Color::blue    = { {   0,   0, 255 }, 1 };
Color Color::magenta = { { 255,   0, 255 }, 1 };
Color Color::cyan    = { {   0, 255, 255 }, 1 };
Color Color::white   = { { 255, 255, 255 }, 1 };
Color Color::transparent = { { 0, 0, 0 }, 0 };
Color Color::opaque      = { { 0, 0, 0 }, 255 };

ostream& operator<<(ostream& o, const PItem& p) {
  switch (p.type) {
  case PBASE:  o << p.u.base.c; break;
  case PSKIP:  o << "!" << p.u.skip.n; break;
  case PSEARCH:  o << "?" << string(p.u.search.start, p.u.search.len); break;
  case PBEGIN:  o << "("; break;
  case PEND:  o << ")"; break;
  default:  assert(false);
  }
  return o;
}


Environment::Environment() {}

void Environment::Add(const Rope& dna) {
  dna_.push_back(dna);
}

const Rope& Environment::Get(size_t i) const {
  if (i >= dna_.size()) {
    static const Rope empty("");
    return empty;
  }
  return dna_[i];
}

void SavePpm(const char* filename, const Bitmap& bitmap, int w, int h) {
  FILE* fp = fopen(filename, "w");
  fprintf(fp, "P3\n%d %d\n%d\n", w, h, 255);
  for (int i = 0; i < h; ++i) {
    for (int j = 0; j < w; ++j) {
      const Pixel& p = bitmap.GetPixel(j, i);
      fprintf(fp, "%d %d %d ", p.rgb.r, p.rgb.g, p.rgb.b);
    }
  }
  fclose(fp);
}
