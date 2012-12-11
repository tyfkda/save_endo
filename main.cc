#include "dna_to_rna.h"
#include "image_builder.h"
#include "util.h"
#include <fstream>
#include <iostream>

using namespace std;

void Main(const char* prefix, const char* filename) {
  ifstream ifs(filename);
  if (!ifs) {
    cerr << "Cannot open \"" << filename << "\"" << endl;
    return;
  }

  string dnabuf;
  ifs >> dnabuf;

  Rope dna;
  if (prefix != NULL) {
    dna = prefix;
  }
  dna += dnabuf;

  DnaToRna dna2rna(dna);
  dna2rna.Execute();

  ImageBuilder imageBuilder;
  imageBuilder.Build(dna2rna.GetRna());

  SavePpm("image.ppm", imageBuilder.GetBitmap(), Bitmap::W, Bitmap::H);
}

int main(int argc, char* argv[]) {
  const char* prefix = NULL;
  if (argc >= 2) {
    prefix = argv[1];
  }
  Main(prefix, "endo.dna");

  return 0;
}
