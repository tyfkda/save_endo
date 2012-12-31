#include "image_builder.h"

#include "rope.h"
#include <assert.h>
#include <iostream>

using namespace std;

Color black   = { {   0,   0,   0 }, 255 };
Color red     = { { 255,   0,   0 }, 255 };
Color green   = { {   0, 255,   0 }, 255 };
Color yellow  = { { 255, 255,   0 }, 255 };
Color blue    = { {   0,   0, 255 }, 255 };
Color magenta = { { 255,   0, 255 }, 255 };
Color cyan    = { {   0, 255, 255 }, 255 };
Color white   = { { 255, 255, 255 }, 255 };
Color transparent = { { 0, 0, 0 }, 0 };
Color opaque      = { { 0, 0, 0 }, 255 };

Bitmap transparentBitmap;


Bitmap::Bitmap() {
  Pixel transparent = { { 0, 0, 0 }, 0 };
  for (int i = 0; i < W * H; ++i) {
    bitmap[i] = transparent;
  }
}

void Bitmap::SetPixel(int x, int y, const Pixel& c) {
  if (0 <= x && x < W && 0 <= y && y < H) {
    bitmap[y * W + x] = c;
  }
}


ImageBuilder::ImageBuilder() : dir_(East) {
  position_.x = position_.y = 0;
  mark_.x = mark_.y = 0;
  bitmaps_.push_back(transparentBitmap);
}

void ImageBuilder::Build(const vector<Rope>& rna) {
  cout << "rna: #" << rna.size() << endl;

  for (vector<Rope>::const_iterator it = rna.begin();
       it != rna.end(); ++it) {
    const Rope& r = *it;
    if (r == "PIPIIIC") {
      AddColor(black);
    } else if (r == "PIPIIIP") {
      AddColor(red);
    } else if (r == "PIPIICC") {
      AddColor(green);
    } else if (r == "PIPIICF") {
      AddColor(yellow);
    } else if (r == "PIPIICP") {
      AddColor(blue);
    } else if (r == "PIPIIFC") {
      AddColor(magenta);
    } else if (r == "PIPIIFF") {
      AddColor(cyan);
    } else if (r == "PIPIIPC") {
      AddColor(white);
    } else if (r == "PIPIIPF") {
      AddColor(transparent);
    } else if (r == "PIPIIPP") {
      AddColor(opaque);
    } else if (r == "PIIPICP") {
      bucket_.clear();
    } else if (r == "PIIIIIP") {
      Move(&position_, dir_);
    } else if (r == "PCCCCCP") {
      dir_ = TurnCounterClockwise(dir_);
    } else if (r == "PFFFFFP") {
      dir_ = TurnClockwise(dir_);
    } else if (r == "PCCIFFP") {
      mark_ = position_;
    } else if (r == "PFFICCP") {
      Line(position_, mark_);
    } else if (r == "PIIPIIP") {
      TryFill();
    } else if (r == "PCCPFFP") {
      AddBitmap(transparentBitmap);
    } else if (r == "PFFPCCP") {
      Compose();
    } else if (r == "PFFICCF") {
      Clip();
    } else {
      cout << "Unknown: " << r << endl;
    }
  }
}

void ImageBuilder::AddBitmap(const Bitmap& bitmap) {
  if (bitmaps_.size() < 10) {
    bitmaps_.push_back(bitmap);
  }
}

void ImageBuilder::AddColor(const Color& c) {
  bucket_.push_back(c);
}

Pixel ImageBuilder::CurrentPixel() const {
  if (bucket_.empty()) {
    return opaque;
  }
  
  int r = 0, g = 0, b = 0, a = 0;
  for (vector<Pixel>::const_iterator it = bucket_.begin();
       it != bucket_.end(); ++it) {
    r += it->rgb.r;
    g += it->rgb.g;
    b += it->rgb.b;
    a += it->a;
  }
  int n = bucket_.size();
  a /= n;
  Pixel p = { { r * a / 255, g * a / 255, b * a / 255 }, a };
  return p;
}

void ImageBuilder::Move(Pos* pos, Dir dir) {
  switch (dir) {
  case North:  pos->y = (pos->y - 1 + Bitmap::H) % Bitmap::H; break;
  case East:   pos->x = (pos->x + 1) % Bitmap::W; break;
  case South:  pos->y = (pos->y + 1) % Bitmap::H; break;
  case West:   pos->x = (pos->x - 1 + Bitmap::W) % Bitmap::W; break;
  }
}

Dir ImageBuilder::TurnCounterClockwise(Dir dir) {
  switch (dir) {
  case North:  return West;
  case East:   return North;
  case South:  return East;
  case West:   return South;
  }
  assert(!"Must not happen");
  return dir;
}

Dir ImageBuilder::TurnClockwise(Dir dir) {
  switch (dir) {
  case North:  return East;
  case East:   return South;
  case South:  return West;
  case West:   return North;
  }
  assert(!"Must not happen");
  return dir;
}

void ImageBuilder::Line(const Pos& p0, const Pos& p1) {
  Pixel pix = CurrentPixel();
  
  int deltax = p1.x - p0.x;
  int deltay = p1.y - p0.y;
  int d = max(abs(deltax), abs(deltay));
  int c = (deltax * deltay < 0) ? 1 : 0;
  int x = p0.x * d + ((d - c) / 2);
  int y = p0.y * d + ((d - c) / 2);
  for (int i = 0; i < d; ++i) {
    SetPixel(x / d, y / d, pix);
    x += deltax;
    y += deltay;
  }
  SetPixel(p1.x, p1.y, pix);
}

void ImageBuilder::TryFill() {
  Pixel newColor = CurrentPixel();
  Pixel oldColor = GetPixel(position_.x, position_.y);
  if (newColor != oldColor) {
    Fill(position_, oldColor, newColor);
  }
}

void ImageBuilder::Fill(const Pos& pos, const Pixel& initial, const Pixel& newColor) {
  vector<Pos> buf;
  buf.push_back(pos);
  
  while (!buf.empty()) {
    Pos p = buf[buf.size() - 1];
    buf.pop_back();
    if (GetPixel(p.x, p.y) == initial) {
      SetPixel(p.x, p.y, newColor);
      if (p.x > 0)             buf.push_back(Pos(p.x - 1, p.y));
      if (p.x < Bitmap::W - 1) buf.push_back(Pos(p.x + 1, p.y));
      if (p.y > 0)             buf.push_back(Pos(p.x, p.y - 1));
      if (p.y < Bitmap::H - 1) buf.push_back(Pos(p.x, p.y + 1));
    }
  }
}

void ImageBuilder::Compose() {
  if (bitmaps_.size() >= 2) {
    Bitmap* b0 = &bitmaps_[bitmaps_.size() - 1];
    Bitmap* b1 = &bitmaps_[bitmaps_.size() - 1];
    for (int i = 0; i < Bitmap::W * Bitmap::H; ++i) {
      Pixel* p0 = &b0->bitmap[i];
      Pixel* p1 = &b1->bitmap[i];
      p1->rgb.r = p0->rgb.r + (p1->rgb.r * (255 - p0->a) / 255);
      p1->rgb.g = p0->rgb.g + (p1->rgb.g * (255 - p0->a) / 255);
      p1->rgb.b = p0->rgb.b + (p1->rgb.b * (255 - p0->a) / 255);
      p1->a = p0->a + (p1->a * (255 - p0->a) / 255);
    }
    bitmaps_.pop_back();
  }
}

void ImageBuilder::Clip() {
  if (bitmaps_.size() >= 2) {
    Bitmap* b0 = &bitmaps_[bitmaps_.size() - 1];
    Bitmap* b1 = &bitmaps_[bitmaps_.size() - 1];
    for (int i = 0; i < Bitmap::W * Bitmap::H; ++i) {
      Pixel* p0 = &b0->bitmap[i];
      Pixel* p1 = &b1->bitmap[i];
      p1->rgb.r = p1->rgb.r * p0->a / 255;
      p1->rgb.g = p1->rgb.g * p0->a / 255;
      p1->rgb.b = p1->rgb.b * p0->a / 255;
      p1->a = p1->a * p0->a / 255;
    }
    bitmaps_.pop_back();
  }
}

Pixel ImageBuilder::GetPixel(int x, int y) {
  if (0 <= x && x < Bitmap::W && 0 <= y && y < Bitmap::H) {
    Bitmap* bitmap = &bitmaps_[bitmaps_.size() - 1];
    return bitmap->bitmap[y * Bitmap::W + x];
  }
  assert(!"Out of range");
  return transparent;
}

void ImageBuilder::SetPixel(int x, int y, const Pixel& c) {
  assert(!bitmaps_.empty());
  Bitmap* bitmap = &bitmaps_[bitmaps_.size() - 1];
  bitmap->SetPixel(x, y, c);
}
