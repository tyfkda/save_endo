#include "image_builder.h"
#include "gtest/gtest.h"

using namespace std;

TEST(ImageBuilderTest, AddColor) {
  const Color b = Color::black;
  const Color r = Color::red;
  const Color m = Color::magenta;
  const Color w = Color::white;
  const Color y = Color::yellow;
  const Color c = Color::cyan;
  const Color t = Color::transparent;
  const Color o = Color::opaque;

  {
    ImageBuilder ib;
    ib.AddColor(t);
    ib.AddColor(o);
    ib.AddColor(o);
    Pixel pxl = ib.CurrentPixel();
    EXPECT_EQ(0, pxl.rgb.r);
    EXPECT_EQ(0, pxl.rgb.g);
    EXPECT_EQ(0, pxl.rgb.b);
    EXPECT_EQ(170, pxl.a);
  }

  {
    ImageBuilder ib;
    ib.AddColor(b);
    ib.AddColor(y);
    ib.AddColor(c);
    Pixel pxl = ib.CurrentPixel();
    EXPECT_EQ(85, pxl.rgb.r);
    EXPECT_EQ(170, pxl.rgb.g);
    EXPECT_EQ(85, pxl.rgb.b);
    EXPECT_EQ(255, pxl.a);
  }

  {
    ImageBuilder ib;
    ib.AddColor(y);
    ib.AddColor(t);
    ib.AddColor(o);
    Pixel pxl = ib.CurrentPixel();
    EXPECT_EQ(127, pxl.rgb.r);
    EXPECT_EQ(127, pxl.rgb.g);
    EXPECT_EQ(0, pxl.rgb.b);
    EXPECT_EQ(127, pxl.a);
  }

  {
    ImageBuilder ib;
    for (int i = 0; i < 18; ++i)  ib.AddColor(b);
    for (int i = 0; i < 7; ++i)  ib.AddColor(r);
    for (int i = 0; i < 39; ++i)  ib.AddColor(m);
    for (int i = 0; i < 10; ++i)  ib.AddColor(w);
    for (int i = 0; i < 3; ++i)  ib.AddColor(o);
    for (int i = 0; i < 1; ++i)  ib.AddColor(t);
    Pixel pxl = ib.CurrentPixel();
    EXPECT_EQ(143, pxl.rgb.r);
    EXPECT_EQ(25, pxl.rgb.g);
    EXPECT_EQ(125, pxl.rgb.b);
    EXPECT_EQ(191, pxl.a);
  }
}
