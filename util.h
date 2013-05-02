#ifndef __UTIL_H__
#define __UTIL_H__

#include <ostream>
#include <vector>

class Bitmap;
class Rope;

enum PType {
  PBASE,
  PSKIP,
  PSEARCH,
  PBEGIN,
  PEND,
};

struct PItem {
  PType type;
  union {
    struct {
      char c;
    } base;
    struct {
      int n;
    } skip;
    struct {
      const char* start;
      int len;
    } search;
  } u;

  static PItem Base(char c) {
    PItem p;
    p.type = PBASE;
    p.u.base.c = c;
    return p;
  }
  static PItem Skip(int n) {
    PItem p;
    p.type = PSKIP;
    p.u.skip.n = n;
    return p;
  }
  static PItem Search(const char* start, int len) {
    PItem p;
    p.type = PSEARCH;
    p.u.search.start = start;
    p.u.search.len = len;
    return p;
  }
  static PItem Begin() {
    PItem p;
    p.type = PBEGIN;
    return p;
  }
  static PItem End() {
    PItem p;
    p.type = PEND;
    return p;
  }
};

std::ostream& operator<<(std::ostream& o, const PItem& p);

enum TType {
  TBASE,
  TREFER,
  TENCODE,
};

struct TItem {
  TType type;
  union {
    struct {
      char c;
    } base;
    struct {
      int n;
      int l;
    } refer;
    struct {
      int n;
    } encode;
  } u;

  static TItem Base(char c) {
    TItem t;
    t.type = TBASE;
    t.u.base.c = c;
    return t;
  }

  static TItem Refer(int n, int l) {
    TItem t;
    t.type = TREFER;
    t.u.refer.n = n;
    t.u.refer.l = l;
    return t;
  }

  static TItem Encode(int n) {
    TItem t;
    t.type = TENCODE;
    t.u.refer.n = n;
    return t;
  }
};

class Environment {
public:
  Environment();

  void Add(const Rope& dna);

  const Rope& Get(size_t i) const;

private:
  std::vector<Rope> dna_;
};

struct Pos {
  int x, y;

  Pos(int xx = 0, int yy = 0) : x(xx), y(yy) {}
};

struct RGB {
  unsigned char r, g, b;

  bool operator==(const RGB& c) const {
    return r == c.r && g == c.g && b == c.b;
  }
  bool operator!=(const RGB& c) const {
    return r != c.r || g != c.g || b != c.b;
  }
};

struct Color {
  RGB rgb;
  unsigned char a;

  bool operator==(const Color& c) const {
    return rgb == c.rgb && a == c.a;
  }
  bool operator!=(const Color& c) const {
    return rgb != c.rgb || a != c.a;
  }

  static Color black;
  static Color red;
  static Color green;
  static Color yellow;
  static Color blue;
  static Color magenta;
  static Color cyan;
  static Color white;
  static Color transparent;
  static Color opaque;
};

typedef Color Pixel;

// Save image in PNG format.
bool SavePng(const char* filename, int width, int height,
             const void* raw_image);
bool SavePng(const char* filename, int width, int height, const Bitmap& bitmap);

// Load PNG image.
unsigned char* LoadPng(const char* filename, int* pwidth, int* pheight);

#endif
