
DNA2RNA_OBJS=\
	dna_to_rna.o \
	rope.o \
	util.o \

RNA2IMAGE_OBJS=\
	rna_to_image.o \
	image_builder.o \
	rope.o \
	util.o \

all:	dna_to_rna rna_to_image

dna_to_rna:	$(DNA2RNA_OBJS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $^ -o $@

rna_to_image:	$(RNA2IMAGE_OBJS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $^ -o $@

clean:
	rm -rf $(DNA2RNA_OBJS) a.out

# Flags passed to the preprocessor.
CPPFLAGS += -I$(GTEST_DIR)/include

# Flags passed to the C++ compiler.
CXXFLAGS += -g -Wall -Wextra -O2

CXX = g++

RUNHASKELL = runhaskell

%.o:	%.cc
	$(CXX) -c -o $@ $(CXXFLAGS) $<


dna_to_rna.o:	dna_to_rna.cc dna_to_rna.h rope.h util.h
image_builder.o:	image_builder.cc image_builder.h rope.h
main.o:	main.cc dna_to_rna.h image_builder.h rope.h
rope.o:	rope.cc rope.h
util.o:	util.cc util.h image_builder.h rope.h





TESTS=rope_test dna_to_rna_test

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

dna_to_rna_test:	dna_to_rna_test.o dna_to_rna.o util.o rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

dna_to_rna_test.o:	dna_to_rna_test.cc dna_to_rna.h util.h $(GTEST_HEADERS)

rope_test:	rope_test.o rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

rope_test.o:	rope_test.cc rope.h $(GTEST_HEADERS)

util_test:	util_test.o util.o rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

util_test.o:	util_test.cc util.h rope.h $(GTEST_HEADERS)

image_builder_test:	image_builder_test.o image_builder.o util.o rope.o gtest_main.a
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -lpthread $^ -o $@

image_builder_test.o:	image_builder_test.cc image_builder.h util.h $(GTEST_HEADERS)

DnaToRnaTest:
	$(RUNHASKELL) DnaToRnaTest.hs

#
