#include "rope.h"
#include "gtest/gtest.h"

using namespace std;

TEST(RopeTest, Basic) {
  Rope e;
  EXPECT_EQ(0, e.size());
  EXPECT_TRUE(e.empty());
  EXPECT_EQ("", e);

  Rope foo("foo");
  EXPECT_EQ(3, foo.size());
  EXPECT_FALSE(foo.empty());
  EXPECT_EQ("foo", foo);
}

TEST(RopeTest, Drop) {
  Rope r("abracadabra");
  r.Drop(4);
  EXPECT_EQ(7, r.size());
  EXPECT_FALSE(r.empty());
  EXPECT_EQ("cadabra", r);

  r.Drop(100);
  EXPECT_EQ(0, r.size());
  EXPECT_TRUE(r.empty());

  EXPECT_EQ('\0', r.Shift());
}

TEST(RopeTest, Shift) {
  Rope r("ab");
  EXPECT_EQ('a', r.Shift());
  EXPECT_EQ(1, r.size());

  EXPECT_EQ('b', r.Shift());
  EXPECT_EQ(0, r.size());

  EXPECT_EQ('\0', r.Shift());
  EXPECT_EQ(0, r.size());
}

TEST(RopeTest, Sub) {
  Rope r("abracadabra");
  Rope sub = r.Sub(4, 7);
  EXPECT_EQ("abracadabra", r);
  EXPECT_EQ("cad", sub);

  EXPECT_EQ("", r.Sub(4, 4));
  EXPECT_EQ("", r.Sub(4, 3));
}

TEST(RopeTest, Strncmp) {
  Rope r("abracadabra");
  EXPECT_EQ(0, r.Strncmp("abra", 4));
  EXPECT_TRUE(r.Strncmp("aara", 4) > 0);
}

TEST(RopeTest, Reference) {
  Rope r("abracadabra");
  EXPECT_EQ('a', r[0]);
  EXPECT_EQ('\0', r[11]);
}

TEST(RopeTest, Assign) {
  {
    Rope source("abracadabra");
    Rope target;
    target = source;
    EXPECT_EQ("abracadabra", target);
  }

  {
    Rope source("abracadabra");
    Rope target;
    source.Drop(4);
    target = source;
    EXPECT_EQ("cadabra", target);
  }

  {
    Rope source("abracadabra");
    Rope target("junk");
    target = source;
    EXPECT_EQ("abracadabra", target);
  }
}

TEST(RopeTest, StrAppend) {
  Rope r("abr");
  r += 'a';
  EXPECT_EQ("abra", r);
  r += "cadabra";
  EXPECT_EQ("abracadabra", r);
}

TEST(RopeTest, StrCat) {
  Rope r("abra");
  EXPECT_EQ("abracadabra", r + "cadabra");
}

TEST(RopeTest, ToString) {
  Rope r("abracadabra");
  EXPECT_EQ("abracadabra", r.ToString());
}
