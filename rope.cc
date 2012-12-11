#include "rope.h"

using namespace std;

void Rope::Drop(int n) {
  p += min(static_cast<size_t>(n), s.size());
}

char Rope::Shift() {
  if (empty())
    return '\0';
  return s[p++];
}

Rope Rope::Sub(size_t start, size_t end) const {
  if (end <= start)
    return Rope("");
  size_t len = min(end - start, size());
  return Rope(s.substr(start + p, len));
}

int Rope::Strncmp(const char* str, int len) const {
  for (int i = 0; i < len; ++i) {
    int d = s[i + p] - str[i];
    if (d != 0)
      return d;
  }
  return 0;
}

void Rope::operator=(const Rope& r2) {
  s = r2.s.substr(r2.p);
  p = 0;
}

void Rope::operator+=(char c) {
  s += c;
}

void Rope::operator+=(const Rope& r2) {
  if (empty()) {
    *this = r2;
    return;
  }
  s += r2.s.substr(r2.p);
}

const Rope Rope::operator+(const Rope& r2) {
  if (empty())
    return r2;
  if (r2.empty())
    return *this;
  if (p == 0) {
    if (r2.p == 0)
      return Rope(s + r2.s);
    else
      return Rope(s + r2.ToString());
  } else {
    if (r2.p == 0)
      return Rope(ToString() + r2.s);
    else
      return Rope(ToString() + r2.ToString());
  }
}

bool Rope::operator==(const string& s2) const {
  if (p == 0)
    return s == s2;
  return s.substr(p) == s2;
}

string Rope::ToString() const {
  if (p == 0)
    return s;
  return s.substr(p);
}

void Rope::Output(ostream& o) const {
  if (p == 0) {
    o << s;
    return;
  }
  o << s.substr(p);
}

size_t Rope::find(const char* str, int len, size_t start) const {
  for (string::size_type i = start; i < size(); ++i) {
    if (strncmp(&s[i + p], str, len) == 0)
      return i;
  }
  return -1;
}

ostream& operator<<(ostream& o, const Rope& rope) { rope.Output(o); return o; }
