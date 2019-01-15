# Define DEBUG or FAST mode:
#   Build the "debug" version with: `make` or `make mode=DEBUG`
#   Build the "trap" version with: `make mode=TRAP`
#   Build the "fast" version with: `make mode=FAST`
mode ?= DEBUG

#   Build the "static" version with: `make static=TRUE`
static ?= FALSE

#Compiler and linker
COMPILER = gfortran
LINKER = gfortran

BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none

DEBUG_OPTS = -g -fbacktrace -O0 -fcheck=all
TRAP_OPTS = -ffpe-trap=invalid,zero,overflow,underflow,precision,denormal
FAST_OPTS = -O3 -march=native -m64 -Ofast

LINK_OPTS = -fbacktrace

ifeq ($(mode),DEBUG)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS)
else ifeq ($(mode), TRAP)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS) $(TRAP_OPTS)
else ifeq ($(mode), FAST)
  FCOPTS = $(BASIC_OPTS) $(FAST_OPTS)
else
  #mode=DEBUG or mode=FAST or mode=TRAP
endif

ifeq ($(static), TRUE)
  FLOPTS = $(LINK_OPTS) -static
else ifeq ($(static), FALSE)
  FLOPTS = $(LINK_OPTS)
else
  #static=TRUE or static=FALSE
endif

.PHONY: .FORCE

#Target directories.
OBJDIR = obj
SRCDIR = src
BINDIR = bin

OBJS = \
	$(OBJDIR)/main.o \
	$(OBJDIR)/module1.o \

.DEFAULT_GOAL := $(BINDIR)/main.elf

clean: .FORCE
	rm -f $(BINDIR)/*.elf
	rm -f $(OBJDIR)/*.o
	rm -f $(OBJDIR)/*.mod
	rm -f $(OBJDIR)/version.txt

#objects
$(OBJDIR)/main.o: $(SRCDIR)/main.f90 $(OBJDIR)/module1.o
	$(COMPILER) $(FCOPTS) -J$(OBJDIR) -c $< -o $@

$(OBJDIR)/module1.o: $(SRCDIR)/module1.f90
	$(COMPILER) $(FCOPTS) -J$(OBJDIR) -c $< -o $@

#elfs
$(BINDIR)/main.elf: $(OBJS)
	make $(OBJDIR)/version.txt
	$(LINKER) $(FLOPTS) $^ -o $@

#public targets
run: $(BINDIR)/main.elf .FORCE
	$(BINDIR)/main.elf

memcheck: $(BINDIR)/main.elf .FORCE
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes $^

$(OBJDIR)/version.txt: .FORCE
	git log -1 --pretty=format:"commit %H%n" > $(OBJDIR)/version.txt #hash
	git log -1 --pretty=format:"Date: %ad" >> $(OBJDIR)/version.txt #date
	git status -sb >> $(OBJDIR)/version.txt
