#include "dna_to_rna.h"

#include "rope.h"
#include "util.h"
#include <assert.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>

#define ABORT(msg) { \
    cerr << msg << " : " << __FILE__ << "(" << __LINE__ << ")" << endl; \
    exit(1); \
}

//#define LOG(msg) cerr << msg << endl
#define LOG(msg) /* nothing */

using namespace std;

void DebugLog(const char* msg) {
  cerr << msg << endl;
}

template<typename T> const T& last(const vector<T>& vec) {
  assert(!vec.empty());
  return vec[vec.size() - 1];
}


DnaToRna::DnaToRna(const Rope& dna) :
  dna_(dna) {}

void DnaToRna::Execute() {
  int n = 0;
  for (;;) {
    printf("\r#%d size:%ld  ", n, dna_.size());
    fflush(stdout);

    if (!Step())
      break;
    
    if (++n >= 1000) break;
  }
  LOG("DNA left: " << dna_.size());
}

bool DnaToRna::Step() {
  vector<PItem> p;
  if (!Pattern(&p))
    return false;
  vector<TItem> t;
  if (!Template(&t))
    return false;
  MatchReplace(p, t);
  return true;
}

bool DnaToRna::Pattern(vector<PItem>* p) {
  LOG(endl << "Start Pattern: " << dna_.size());
  
  int lvl = 0;
  for (;;) {
    switch (dna_.Shift()) {
    default:
      ABORT("Unexpected, " << dna_[-1]);
      break;
    case '\0':  // End of sequence
      return false;
    case 'C':  // 'C'
      p->push_back(PItem::Base('I'));
      break;
    case 'F':  // 'F'
      p->push_back(PItem::Base('C'));
      break;
    case 'P':  // 'P'
      p->push_back(PItem::Base('F'));
      break;
    case 'I':
      switch (dna_.Shift()) {
      default:
        ABORT("Unexpected, I" << dna_[-1]);
        break;
      case 'C':  // 'IC'
        p->push_back(PItem::Base('P'));
        //LOG("Push P");
        break;
      case 'P':  // 'IP'
        {
          int n = Nat();
          //LOG("Skip " << n);
          p->push_back(PItem::Skip(n));
        }
        break;
      case 'F':  // 'IF'
        {
          dna_.Drop(1);
          const string& s(Consts());
          p->push_back(PItem::Search(&s[0], s.size()));
          //LOG("Search: " << s);
        }
        break;
      case 'I':
        switch (dna_.Shift()) {
        default:
          ABORT("Unexpected, II" << dna_[-1]);
          break;
        case 'P':  // 'IIP'
          ++lvl;
          p->push_back(PItem::Begin());
          //LOG("Begin(" << lvl << ") {");
          break;
        case 'C':  // 'IIC'
        case 'F':  // 'IIF'
          if (lvl == 0)
            return true;
          --lvl;
          p->push_back(PItem::End());
          //LOG("End (" << lvl << ") }");
          break;
        case 'I':  // 'III'
          rna_.push_back(dna_.Sub(0, 7));
          //LOG("Push RNA: " << dna_.Sub(0, 7));
          dna_.Drop(7);
          break;
        }
        break;
      }
      break;
    }
  }
  return false;
}

bool DnaToRna::Template(vector<TItem>* t) {
  LOG(endl << "Start Template: " << dna_.size());
  
  for (;;) {
    switch (dna_.Shift()) {
    default:
      cerr << "Unexpected: " << dna_[-1] << "(" << int(dna_[-1]) << ")"
           << endl;
      ABORT("Unexpected");
      break;
    case 'C':  // 'C'
      //LOG("Push 'I'");
      t->push_back(TItem::Base('I'));
      break;
    case 'F':  // 'F'
      //LOG("Push 'C'");
      t->push_back(TItem::Base('C'));
      break;
    case 'P':  // 'P'
      //LOG("Push 'F'");
      t->push_back(TItem::Base('F'));
      break;
    case 'I':
      switch (dna_.Shift()) {
      default:
        cerr << "Unexpected: " << dna_[-1] << "(" << int(dna_[-1]) << ")"
             << endl;
        ABORT("Unexpected");
        break;
      case 'C':  // 'IC'
        //LOG("Push 'P'");
        t->push_back(TItem::Base('P'));
        break;
      case 'F':  // 'IF'
      case 'P':  // 'IP'
        {
          int l = Nat();
          int n = Nat();
          //LOG("Push Refer(" << n << ", " << l << ")");
          t->push_back(TItem::Refer(n, l));
        }
        break;
      case 'I':
        switch (dna_.Shift()) {
        case 'C':  // 'IIC'
        case 'F':  // 'IIF'
          return true;
        case 'P':  // 'IIP'
          {
            int n = Nat();
            //LOG("Push Encode(" << n << ")");
            t->push_back(TItem::Encode(n));
          }
          break;
        case 'I':  // 'III'
          rna_.push_back(dna_.Sub(0, 7));
          //LOG("Push RNA: " << string(dna_, 7));
          dna_.Drop(7);
          break;
        }
        break;
        
      case '\0':
        return false;
      }
      break;
      
    case '\0':
      return false;
    }
  }
  return false;
}

void DnaToRna::MatchReplace(vector<PItem>& pat, vector<TItem>& t) {
  LOG(endl << "Start Mathch Replace: " << dna_.size());
  LOG("Pattern: #" << pat.size());
  
  size_t i = 0;
  Environment e;
  vector<int> c;
  for (vector<PItem>::const_iterator itp = pat.begin(); itp != pat.end();
       ++itp) {
    const PItem& p = *itp;
    switch (p.type) {
    case PBASE:
      LOG("Base: " << p.u.base.c);
      if (dna_[i] == p.u.base.c) {
        ++i;
      } else {
        LOG("Not match: " << p.u.base.c << " - " << dna_[i]);
        return;
      }
      break;
    case PSKIP:
      LOG("Skip: " << p.u.skip.n);
      i += p.u.skip.n;
      if (i > dna_.size()) {
        LOG("Skip over: " << p.u.skip.n);
        return;
      }
      break;
    case PSEARCH:
      LOG("Search: " << string(p.u.search.start, p.u.search.len));
      {
        size_t n = Search(p.u.search.start, p.u.search.len, dna_, i);
        if (n >= i) {
          i = n;
        } else {
          LOG("Cannot find:" << string(p.u.search.start, p.u.search.len));
          return;
        }
      }
      break;
    case PBEGIN:
      LOG("Begin (" << i);
      c.push_back(i);
      break;
    case PEND:
      assert(!c.empty());
      LOG("End )" << last(c) << " - " << i);
      e.Add(dna_.Sub(last(c), i));
      c.pop_back();
      break;
    }
  }
  
  dna_.Drop(i);
  Replace(t, e);
}

void DnaToRna::Replace(const vector<TItem>& tpl, const Environment& e) {
  Rope r;
  for (vector<TItem>::const_iterator it = tpl.begin(); it != tpl.end(); ++it) {
    const TItem& t = *it;
    switch (t.type) {
    case TBASE:  r += t.u.base.c; break;
    case TREFER:
      {
        const Rope& dna = e.Get(t.u.refer.n);
        r += Protect(t.u.refer.l, dna);
      }
      break;
    case TENCODE:  r += Asnat(e.Get(t.u.encode.n).size()); break;
    }
  }
  LOG("Replace: add " << r.size());
  dna_ = r + dna_;
}

int DnaToRna::Nat() {
#if 0
  switch (dna_.Shift()) {
  case 'P':
    return 0;
  case 'I': case 'F':
    return 2 * Nat();
  case 'C':
    return 2 * Nat() + 1;
  default:
    ABORT("Not implemented");
    return 0;  // finish()
  }
#else
  int n;
  for (n = 0; ; ++n) {
    char c = dna_[n];
    if (c == 'P')
      break;
    if (c != 'I' && c != 'F' && c != 'C') {
      // finish()
      ABORT("Not implemented");
      return 0;  // finish()
    }
  }

  assert(n <= 32);

  int x = 0;
  for (int i = 0; i < n; ++i) {
    x <<= 1;
    switch (dna_[n - i - 1]) {
    case 'C':
      ++x;
      break;
    }
  }
  dna_.Drop(n + 1);
  return x;
#endif
}

const string& DnaToRna::Consts() {
  string s;
  for (;;) {
    switch (dna_.Shift()) {
    case 'C':  s += 'I';  break;
    case 'F':  s += 'C';  break;
    case 'P':  s += 'F';  break;
    case 'I':
      switch (dna_.Shift()) {
      case 'C':  s += 'P';  break;
      default:
        dna_ = dna_.Sub(-2, 0) + dna_;
        goto L_exit;
      }
      break;
    case '\0':
      goto L_exit;
      break;
    default:
      cerr << dna_[-1] << endl;
      ABORT("Unexpected");
      goto L_exit;
    }
  }
 L_exit:
  constBuffer_.push_back(s);
  return last(constBuffer_);
}

Rope DnaToRna::Protect(int l, const Rope& d) {
  if (l == 0)
    return d;
  return Protect(l - 1, Quote(d));
}

Rope DnaToRna::Quote(const Rope& d) {
  string s;
  for (size_t i = 0; i < d.size(); ++i) {
    switch (d[i]) {
    case 'I':  s += 'C'; break;
    case 'C':  s += 'F'; break;
    case 'F':  s += 'P'; break;
    case 'P':  s += "IC"; break;
    default:  return s;
    }
  }
  return Rope(s);
}

string DnaToRna::Asnat(int n) {
  string s;
  for (;;) {
    if (n == 0)
      return s + 'P';
    if ((n & 1) == 0)
      s += 'I';
    else
      s += 'C';
    n >>= 1;
  }
}

size_t DnaToRna::Search(const char* pattern, int patternLen,
                        const Rope& dna, size_t i) {
  string::size_type index = dna.find(pattern, patternLen, i);
  if (index != string::npos)
    return index + patternLen;
  return -1;
}
