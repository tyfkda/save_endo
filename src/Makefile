
BINDIR=../bin
OBJDIR=obj

RNA2IMAGE_OBJS=\
	$(OBJDIR)/rna_to_image.o \
	$(OBJDIR)/image_builder.o \
	$(OBJDIR)/rope.o \
	$(OBJDIR)/util.o \

CALC_RISK_OBJS=\
	$(OBJDIR)/calc_risk.o \
	$(OBJDIR)/rope.o \
	$(OBJDIR)/util.o \

EXES=\
	$(BINDIR)/rna_to_image \
	$(BINDIR)/dna_to_rna \
	$(BINDIR)/calc_risk \

all:	$(EXES)

$(BINDIR)/dna_to_rna:	DnaToRnaMain.hs DnaToRna.hs
	$(GHC) -o $@ DnaToRnaMain.hs

$(BINDIR)/rna_to_image:	$(RNA2IMAGE_OBJS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LIBPATH) $(LIBS) $^ -o $@

$(BINDIR)/calc_risk:	$(CALC_RISK_OBJS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LIBPATH) $(LIBS) $^ -o $@

clean:
	rm -rf $(DNA2RNA_OBJS) a.out
	rm -rf $(EXES)
	rm -rf *.hi *.o *.a $(OBJDIR)/*.*
	rm -rf $(EXES)

INC =
LIBPATH =
LIBS = -lpng

# Flags passed to the preprocessor.
CPPFLAGS = -I$(GTEST_DIR)/include

# Flags passed to the C++ compiler.
#CXXFLAGS = -g -Wall -Wextra -O2 $(INC)
CXXFLAGS = -g -Wall -Wextra -DGTEST_USE_OWN_TR1_TUPLE=1 -std=c++0x  # -Werror

CXX = g++

GHC = ghc -O2 -odir $(OBJDIR) -hidir $(OBJDIR)
RUNHASKELL = runhaskell

$(OBJDIR)/%.o:	%.cc
	$(CXX) -c -o $@ $(CXXFLAGS) $<


$(OBJDIR)/image_builder.o:	image_builder.cc image_builder.h rope.h
$(OBJDIR)/rope.o:	rope.cc rope.h
$(OBJDIR)/util.o:	util.cc util.h image_builder.h rope.h





TESTS=rope_test util_test

# Points to the root of Google Test, relative to where this file is.
# Remember to tweak this if you move this file.
GTEST_DIR = /usr/local/gtest

# All Google Test headers.  Usually you shouldn't change this
# definition.
GTEST_HEADERS = $(GTEST_DIR)/include/gtest/*.h \
                $(GTEST_DIR)/include/gtest/internal/*.h

# All Google Test headers.  Usually you shouldn't change this
# definition.
GTEST_HEADERS = $(GTEST_DIR)/include/gtest/*.h \
                $(GTEST_DIR)/include/gtest/internal/*.h

# Builds gtest.a and gtest_main.a.

# Usually you shouldn't tweak such internal variables, indicated by a
# trailing _.
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)

# For simplicity and to avoid depending on Google Test's
# implementation details, the dependencies specified below are
# conservative and not optimized.  This is fine as Google Test
# compiles fast and for ordinary users its source rarely changes.
gtest-all.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest-all.cc

gtest_main.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
            $(GTEST_DIR)/src/gtest_main.cc

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^

# Builds a sample test.  A test should link with either gtest.a or
# gtest_main.a, depending on whether it defines its own main()
# function.

tests:	$(TESTS)

rope_test:	$(OBJDIR)/rope_test.o $(OBJDIR)/rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

rope_test.o:	rope_test.cc rope.h $(GTEST_HEADERS)

util_test:	$(OBJDIR)/util_test.o $(OBJDIR)/util.o $(OBJDIR)/rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@ $(LIBPATH) $(LIBS)

util_test.o:	util_test.cc util.h rope.h $(GTEST_HEADERS)

image_builder_test:	$(OBJDIR)/image_builder_test.o $(OBJDIR)/image_builder.o $(OBJDIR)/util.o $(OBJDIR)/rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

image_builder_test.o:	image_builder_test.cc image_builder.h util.h $(GTEST_HEADERS)

DnaToRnaTest:
	$(RUNHASKELL) DnaToRnaTest.hs

#
