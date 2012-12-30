#ifndef __ROPE_H__
#define __ROPE_H__

#include <ostream>
#include <string>

class Rope {
public:
  Rope() : s(""), p(0) {}
  Rope(const char* str) : s(str), p(0) {}
  Rope(const std::string& str) : s(str), p(0) {}

  void Drop(int n);
  char Shift();
  Rope Sub(size_t start, size_t end) const;
  int Strncmp(const char* str, int len) const;

  bool empty() const { return size() == 0; }
  size_t size() const { return s.size() - p; }
  size_t find(const char* str, int len, size_t start) const;
  const char operator[](int x) const { return s[x + p]; }
  void operator=(const Rope& r2);
  void operator+=(char c);
  void operator+=(const Rope& r2);
  const Rope operator+(const Rope& r2);
  bool operator==(const std::string& s2) const;

  std::string ToString() const;

  void Output(std::ostream& o) const;

private:
  std::string s;
  size_t p;
};

std::ostream& operator<<(std::ostream& o, const Rope& rope);
inline bool operator==(const char* s1, const Rope& r2) { return r2 == s1; }

#endif
