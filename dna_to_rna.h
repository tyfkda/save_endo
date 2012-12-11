#ifndef __DNA_TO_RNA_H__
#define __DNA_TO_RNA_H__

#include "rope.h"
#include <ostream>
#include <string>
#include <vector>

class Environment;
class PItem;
class TItem;

class DnaToRna {
public:
  DnaToRna(const Rope& dna);

  void Execute();
  bool Step();

  const Rope& GetDna() const { return dna_; }
  const std::vector<Rope>& GetRna() const { return rna_; }

  bool Pattern(std::vector<PItem>* p);
  bool Template(std::vector<TItem>* t);
  void MatchReplace(std::vector<PItem>& pat, std::vector<TItem>& t);
  void Replace(const std::vector<TItem>& tpl, const Environment& e);
  int Nat();
  const std::string& Consts();

  static Rope Protect(int l, const Rope& d);
  static Rope Quote(const Rope& d);
  static std::string Asnat(int n);
  static size_t Search(const char* pattern, int patternLen,
                       const Rope& dna, size_t i);

private:
  Rope dna_;
  std::vector<Rope> rna_;
  std::vector<std::string> constBuffer_;
};

#endif
