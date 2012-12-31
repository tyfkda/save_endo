#include "dna_to_rna.h"
#include "gtest/gtest.h"
#include "util.h"

using namespace std;

string ToString(int n) {
  char buf[16];
  sprintf(buf, "%d", n);
  return buf;
}

string ToString(const vector<PItem>& p) {
  Rope r;
  for (vector<PItem>::const_iterator it = p.begin(); it != p.end(); ++it) {
    const PItem& item = *it;
    switch (item.type) {
    case PBASE:  r += item.u.base.c;  break;
    case PSKIP:  r += "!" + ToString(item.u.skip.n);  break;
    case PSEARCH:  r += "?" + string(item.u.search.start, item.u.search.len);  break;
    case PBEGIN:  r += "(";  break;
    case PEND:  r += ")";  break;
    }
  }
  return r.ToString();
}

TEST(DnaToRnaTest, Pattern) {
  {
    vector<PItem> p;
    DnaToRna dna2rna("CIIC");
    EXPECT_TRUE(dna2rna.Pattern(&p));
    EXPECT_EQ(1, p.size());
    EXPECT_EQ("I", ToString(p));
    EXPECT_TRUE(dna2rna.GetDna().empty());
  }

  {
    vector<PItem> p;
    DnaToRna dna2rna("IIPIPICPIICICIIF");
    EXPECT_TRUE(dna2rna.Pattern(&p));
    EXPECT_EQ(4, p.size());
    EXPECT_EQ("(!2)P", ToString(p));
    EXPECT_TRUE(dna2rna.GetDna().empty());
  }

  {
    vector<PItem> p;
    DnaToRna dna2rna("IIIICFPICFIIC");
    EXPECT_TRUE(dna2rna.Pattern(&p));
    EXPECT_EQ(0, p.size());
    EXPECT_TRUE(dna2rna.GetDna().empty());
    EXPECT_EQ(1, dna2rna.GetRna().size());
    EXPECT_EQ("ICFPICF", dna2rna.GetRna()[0]);
  }
}

TEST(DnaToRnaTest, Nat) {
  DnaToRna dna2rna("CICFCP--");
  EXPECT_EQ(21, dna2rna.Nat());
  EXPECT_EQ("--", dna2rna.GetDna());
}

TEST(DnaToRnaTest, Asnat) {
  EXPECT_EQ("CICICP", DnaToRna::Asnat(21));
}

TEST(DnaToRnaTest, Consts) {
  {
    DnaToRna dna2rna("CFPICIF");
    EXPECT_EQ("ICFP", dna2rna.Consts());
    EXPECT_EQ("IF", dna2rna.GetDna());
  }

  {
    DnaToRna dna2rna("");
    EXPECT_EQ("", dna2rna.Consts());
  }
}

TEST(DnaToRnaTest, Quote) {
  EXPECT_EQ("CFPICCFPIC", DnaToRna::Quote("ICFPICFP"));
}

TEST(DnaToRnaTest, Protect) {
  EXPECT_EQ("ICFPICFP", DnaToRna::Protect(0, "ICFPICFP"));
  EXPECT_EQ("CFPICCFPIC", DnaToRna::Protect(1, "ICFPICFP"));
  EXPECT_EQ("FPICCFFPICCF", DnaToRna::Protect(2, "ICFPICFP"));
}

TEST(DnaToRnaTest, Search) {
  const char pattern[] = "ICFP";
  const Rope dna("ICFPICFP");
  EXPECT_EQ(4, DnaToRna::Search(pattern, strlen(pattern), dna, 0));
  EXPECT_EQ(8, DnaToRna::Search(pattern, strlen(pattern), dna, 1));
  EXPECT_EQ(8, DnaToRna::Search(pattern, strlen(pattern), dna, 4));
  EXPECT_EQ(-1, DnaToRna::Search(pattern, strlen(pattern), dna, 5));

  EXPECT_EQ(-1, DnaToRna::Search(pattern, strlen(pattern), Rope("PFPFPF"), 0));
}

TEST(DnaToRnaTest, Replace) {
  {  // Base.
    DnaToRna dna2rna("-JUNKDNA");
    vector<TItem> tpl;
    tpl.push_back(TItem::Base('I'));
    tpl.push_back(TItem::Base('C'));
    tpl.push_back(TItem::Base('F'));
    tpl.push_back(TItem::Base('P'));
    Environment e;
    dna2rna.Replace(tpl, e);
    EXPECT_EQ("ICFP-JUNKDNA", dna2rna.GetDna());
  }

  {  // Refer.
    DnaToRna dna2rna("-JUNKDNA");
    vector<TItem> tpl;
    tpl.push_back(TItem::Refer(0, 2));
    Environment e;
    e.Add("ICFP");
    dna2rna.Replace(tpl, e);
    EXPECT_EQ("FPICCF-JUNKDNA", dna2rna.GetDna());
  }

  {  // Encode.
    DnaToRna dna2rna("-JUNKDNA");
    vector<TItem> tpl;
    tpl.push_back(TItem::Encode(0));
    Environment e;
    e.Add("ICFPICFPICFPICFPICFPI");
    dna2rna.Replace(tpl, e);
    EXPECT_EQ("CICICP-JUNKDNA", dna2rna.GetDna());
  }
}

TEST(DnaToRnaTest, Step) {
  {
    vector<PItem> p;
    DnaToRna dna2rna("");
    EXPECT_FALSE(dna2rna.Step());
    EXPECT_TRUE(dna2rna.GetDna().empty());
  }

  {
    vector<PItem> p;
    DnaToRna dna2rna("IIPIPICPIICICIIFICCIFPPIICCFPC");
    EXPECT_TRUE(dna2rna.Step());
    EXPECT_EQ("PICFC", dna2rna.GetDna());
  }

  {
    vector<PItem> p;
    DnaToRna dna2rna("IIPIPICPIICICIIFICCIFCCCPPIICCFPC");
    EXPECT_TRUE(dna2rna.Step());
    EXPECT_EQ("PIICCFCFFPC", dna2rna.GetDna());
  }

  {
    vector<PItem> p;
    DnaToRna dna2rna("IIPIPIICPIICIICCIICFCFC");
    EXPECT_TRUE(dna2rna.Step());
    EXPECT_EQ("I", dna2rna.GetDna());
  }
}

TEST(DnaToRnaTest, Execute) {
  {
    vector<PItem> p;
    DnaToRna dna2rna("");
    dna2rna.Execute();
    EXPECT_TRUE(dna2rna.GetDna().empty());
    EXPECT_TRUE(dna2rna.GetRna().empty());
  }
}

//
