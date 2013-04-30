#ifndef __IMAGE_BUILDER_H__
#define __IMAGE_BUILDER_H__

#include "util.h"
#include <vector>

class Rope;

enum Dir {
  North,
  East,
  South,
  West,
};

struct Bitmap {
  static const int W = 600, H = 600;

  Pixel bitmap[W * H];

  Bitmap();
  Bitmap(const Bitmap& src)  { *this = src; }
  const Pixel& GetPixel(int x, int y) const {
    return bitmap[y * W + x];
  }
  void SetPixel(int x, int y, const Pixel& c);

  void Compose(const Bitmap& layer);

  const Bitmap& operator=(const Bitmap& src);

  static Bitmap transparentBitmap;
};

class ImageBuilder {
public:
  ImageBuilder();

  size_t GetBitmapCount() const { return bitmaps_.size(); }
  const Bitmap& GetBitmap(int i) const { return bitmaps_[i]; }
  const Bitmap& GetLastBitmap() const { return bitmaps_[bitmaps_.size() - 1]; }

  void Step(const Rope&  rna);

  void AddBitmap(const Bitmap& bitmap);
  void AddColor(const Color& c);
  Pixel CurrentPixel() const;

  static void Move(Pos* pos, Dir dir);
  static Dir TurnCounterClockwise(Dir dir);
  static Dir TurnClockwise(Dir dir);
  void Line(const Pos& p0, const Pos& p1);
  void TryFill();
  void Fill(const Pos& pos, const Pixel& initial, const Pixel& newColor);
  void Compose();
  void Clip();

  Pixel GetPixel(int x, int y);
  void SetPixel(int x, int y, const Pixel& c);

 private:
  // Coord
  Pos position_;
  Pos mark_;
  std::vector<Bitmap> bitmaps_;
  std::vector<Pixel> bucket_;
  Dir dir_;
};

#endif
