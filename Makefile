# Define DEBUG or FAST mode:
#   Build the "debug" version with: `make` or `make mode=DEBUG`
#   Build the "trap" version with: `make mode=TRAP`
#   Build the "fast" version with: `make mode=FAST`

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

ifeq ($(mode), DEBUG)
  FCOPTS = $(BASIC_F_OPTS) $(DEBUG_F_OPTS)
else ifeq ($(mode), TRAP)
  FCOPTS = $(BASIC_F_OPTS) $(TRAP_F_OPTS)
else ifeq ($(mode), FAST)
  FCOPTS = $(BASIC_F_OPTS) $(FAST_F_OPTS)
else
  #mode=DEBUG or mode=FAST or mode=TRAP
endif

ifeq ($(static), TRUE)
  FLOPTS = $(LINK_OPTS) -static
else ifeq ($(static), FALSE)
  FLOPTS = $(LINK_OPTS) -static
else
  #static=TRUE or static=FALSE
endif

.PHONY: .FORCE

#Target directories.
OBJDIR = obj
SRCDIR = src
RUNDIR = run

OBJS = \
	$(OBJDIR)/main.o \
	$(OBJDIR)/module1.o \

.DEFAULT_GOAL := $(RUNDIR)/main.elf

clean: .FORCE
	rm -f $(RUNDIR)/*.elf
	rm -f $(OBJDIR)/*.o
	rm -f $(OBJDIR)/*.mod

#objects
$(OBJDIR)/main.o: $(SRCDIR)/main.f90 $(OBJDIR)/module1.o
	$(COMPILER) $(FCOPTS) -J$(OBJDIR) -c $< -o $@

$(OBJDIR)/module1.o: $(SRCDIR)/module1.f90
	$(COMPILER) $(FCOPTS) -J$(OBJDIR) -c $< -o $@

#elfs
$(RUNDIR)/main.elf: $(OBJS)
	make $(OBJDIR)/version.txt
	$(LINKER) $(FLOPTS) $^ -o $@

#public targets
run: $(RUNDIR)/main.elf .FORCE
	$(RUNDIR)/main.elf

memcheck: $(RUNDIR)/main.elf .FORCE
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes $^

$(OBJDIR)/version.txt: .FORCE
	git log -1 --pretty=format:"commit %H%n" > $(OBJDIR)/version.txt #hash
	git log -1 --pretty=format:"Date: %ad" >> $(OBJDIR)/version.txt #date
	git status -sb >> $(OBJDIR)/version.txt
