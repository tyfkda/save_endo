#include "util.h"

#include "rope.h"
#include "image_builder.h"
#include <assert.h>
#include <png.h>
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

// Save full-color png to file.
bool SavePng(const char* filename, int width, int height,
             const void* raw_image) {
  png_structp pp = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL,
                                           NULL);
  png_infop ip = png_create_info_struct(pp);
  // Initialize.
  FILE* fp = fopen(filename, "wb");
  png_init_io(pp, fp);
  png_set_IHDR(pp, ip, width, height,
               8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
               PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  // Line buffer.
  png_bytep raw1D = static_cast<png_bytep>(const_cast<void*>(raw_image));
  png_bytepp raw2D = (png_bytepp)malloc(height * sizeof(png_bytep));
  for (int i = 0; i < height; i++)
    raw2D[i] = &raw1D[i * png_get_rowbytes(pp, ip)];

  // Write.
  png_write_info(pp, ip);
  png_write_image(pp, raw2D);
  png_write_end(pp, ip);
  // Free.
  png_destroy_write_struct(&pp, &ip);
  free(raw2D);
  fclose(fp);
  return true;
}

bool SavePng(const char* filename, int width, int height, const Bitmap& bitmap) {
  unsigned char* image = new unsigned char[width * height * 3];
  for (int i = 0; i < height; ++i) {
    for (int j = 0; j < width; ++j) {
      const Pixel& p = bitmap.GetPixel(j, i);
      if (p.rgb.r == 0 && p.rgb.g == 0 && p.rgb.b == 0 && p.a == 0) {
        image[(i * width + j) * 3 + 0] = 0;
        image[(i * width + j) * 3 + 1] = 0;
        image[(i * width + j) * 3 + 2] = 0;
      } else {
        image[(i * width + j) * 3 + 0] = p.rgb.r;
        image[(i * width + j) * 3 + 1] = p.rgb.g;
        image[(i * width + j) * 3 + 2] = p.rgb.b;
      }
    }
  }
  bool result = SavePng(filename, width, height, image);
  delete[] image;
  return result;
}

unsigned char* LoadPng(const char* filename, int* pwidth, int* pheight) {
  FILE* fp = fopen(filename, "rb");
  if (fp == NULL) {
    cerr << "Cannot open image file:" << filename << endl;
    return NULL;
  }

  const size_t HEADER_SIZE = 8;
  png_byte header[HEADER_SIZE];
  if (fread(header, 1, HEADER_SIZE, fp) != HEADER_SIZE) {
    cerr << "Failed to read header." << endl;
    return NULL;
  }
  if (png_sig_cmp(header, 0, HEADER_SIZE) != 0) {
    cerr << "File is not PNG:" << filename << endl;
    return NULL;
  }

  png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL,
                                               NULL, NULL);
  png_infop info_ptr = png_create_info_struct(png_ptr);

  png_bytep image = NULL;
  if (!setjmp(png_jmpbuf(png_ptr))) {
    png_init_io(png_ptr, fp);
    png_set_sig_bytes(png_ptr, HEADER_SIZE);

    png_uint_32 width, height;
    int depth, color_type, interlace_type;
    png_read_info(png_ptr, info_ptr);
    png_get_IHDR(png_ptr, info_ptr, &width, &height, &depth, &color_type,
                 &interlace_type, NULL, NULL);

    cout << width << "x" << height << ":" << depth << endl;

    int row_size;
    int image_size;

    row_size = (png_get_rowbytes(png_ptr, info_ptr) + 3) & ~3;
    image_size = row_size * height;
    cout << "row_size:" << row_size << ", image_size:" << image_size << endl;
    image = new png_byte[image_size];

    for (png_uint_32 i = 0; i < height; ++i) {
      png_read_row(png_ptr, &image[i * row_size], NULL);
    }
    png_read_end(png_ptr, NULL);

    *pwidth = width;
    *pheight = height;
  }

  fclose(fp);
  return image;
}
