#include "util.h"
#include "gtest/gtest.h"
#include "rope.h"

TEST(UtilTest, EnvironmentGet) {
  Environment e;
  e.Add(Rope("dna1"));
  e.Add(Rope("dna2"));
  e.Add(Rope("dna3"));
  EXPECT_EQ("dna1", e.Get(0));
  EXPECT_EQ("dna2", e.Get(1));
  EXPECT_EQ("dna3", e.Get(2));
  EXPECT_TRUE(e.Get(3).empty());
}

//
