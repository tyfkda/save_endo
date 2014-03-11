
BINDIR=../bin
OBJDIR=obj

QUANTIZE_OBJS=\
	$(OBJDIR)/quantize.o \

EXES=\
	$(BINDIR)/quantize \
	$(BINDIR)/dna_to_rna \
	$(BINDIR)/calc_risk \

all:	$(EXES)

$(BINDIR)/quantize:	$(QUANTIZE_OBJS)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LIBPATH) $(LIBS) $^ -o $@ ../src/obj/util.o

clean:
	rm -rf $(DNA2RNA_OBJS) a.out
	rm -rf $(EXES)
	rm -rf *.hi *.o *.a $(OBJDIR)/*.*
	rm -rf $(EXES)

INC = -I ../src
LIBPATH =
LIBS = -lpng

# Flags passed to the preprocessor.
CPPFLAGS = -I$(GTEST_DIR)/include

# Flags passed to the C++ compiler.
CXXFLAGS = -g -Wall -Wextra -Werror -O2 $(INC)

CXX = g++

GHC = ghc -O2 -odir $(OBJDIR) -hidir $(OBJDIR)
RUNHASKELL = runhaskell

$(OBJDIR)/%.o:	%.cc
	$(CXX) -c -o $@ $(CXXFLAGS) $<


$(OBJDIR)/image_builder.o:	image_builder.cc image_builder.h rope.h
$(OBJDIR)/rope.o:	rope.cc rope.h
$(OBJDIR)/util.o:	util.cc util.h image_builder.h rope.h

#
