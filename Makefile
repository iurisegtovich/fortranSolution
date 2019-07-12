# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Instruções

# O programa make:

# Para executar as receitas e construir o programa basta digitar
## make

# Opção recompilar do make:

# Para forçar recompilar todos os arquivos do projeto,
# independentemente da verificação de hora de modificação
## make -B

# Funções preparadas nesse arquivo:

# Para apagar todos os arquivos objeto temporários e programa final
## make clean

# Para (re-)compilar e rodar o programa
## make run

# Para (re-)compilar e rodar o programa no modo de verificação de memória
## make memcheck

# Para (re-)compilar e rodar o programa no modo de debug step-by-step
## make debug

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# mode: pode mudar aqui para fast ou trap se quiser usar os modos de código acelerado (e sem rastreamento de erros) ou modo trap (com detecção de infinity, nan, etc,...)

## make run mode=debug
## make run mode=trap
## make run mode=fast

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Arquivos e receitas do projeto:

## Receita para o programa final:
bin/main.elf: .FORCE
	make clean
	$(COMPILER) $(FCOPTS) -Jobj -c src/module1.f90 -o obj/module1.o
	$(COMPILER) $(FCOPTS) -Jobj -c src/main.f90 -o obj/main.o
	$(LINKER) $(LINK_OPTS) obj/*.o -o $@

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Makefile keywords

#CONFIGURATIONS
#default target keyword
.DEFAULT_GOAL := build

# phony target .FORCE to force executing keyword recipes ignoring like-named files
.PHONY: .FORCE

#KEYWORDS
build: .FORCE
	make bin/main.elf mode=$(mode)

run: .FORCE
	make bin/main.elf mode=$(mode)
	bin/main.elf

debug: .FORCE
	make bin/main.elf mode=debug
	gdb bin/main.elf

memcheck: .FORCE
	make bin/main.elf mode=debug
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes --show-leak-kinds=all bin/main.elf

clean: .FORCE
	rm -f bin/*.elf
	rm -f obj/*.o
	rm -f obj/*.mod
	rm -f bin/version.txt

version: .FORCE
	git log -1 --pretty=format:"commit %H%n" > bin/version.txt #hash
	git log -1 --pretty=format:"Date: %ad%n" >> bin/version.txt #date
	git status -sb >> bin/version.txt #status

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais do Makefile:

#Compiler and linker
COMPILER = gfortran
LINKER = gfortran

#flags for each mode
BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none -g
##> -pedantic -std=f2008ts
# Issue warnings for uses of extensions to Fortran 95.  -pedantic
#           also applies to C-language constructs where they occur in GNU
#           Fortran source files, such as use of \e in a character constant
#           within a directive like "#include".
#
#           Valid Fortran 95 programs should compile properly with or without
#           this option.  However, without this option, certain GNU extensions
#           and traditional Fortran features are supported as well.  With this
#           option, many of them are rejected.
#
#           Some users try to use -pedantic to check programs for conformance.
#           They soon find that it does not do quite what they want---it finds
#           some nonstandard practices, but not all.  However, improvements to
#           GNU Fortran in this area are welcome.
#
#           This should be used in conjunction with -std=f95, -std=f2003 or
#           -std=f2008.
#
#-std=std
#           Specify the standard to which the program is expected to conform,
#           which may be one of f95, f2003, f2008, gnu, or legacy.  The default
#           value for std is gnu, which specifies a superset of the Fortran 95
#           standard that includes all of the extensions supported by GNU
#           Fortran, although warnings will be given for obsolete extensions
#           not recommended for use in new code.  The legacy value is
#           equivalent but without the warnings for obsolete extensions, and
#           may be useful for old non-standard programs.  The f95, f2003 and
#           f2008 values specify strict conformance to the Fortran 95, Fortran
#           2003 and Fortran 2008 standards, respectively; errors are given for
#           all extensions beyond the relevant language standard, and warnings
#           are given for the Fortran 77 features that are permitted but
#           obsolescent in later standards. -std=f2008ts allows the Fortran
#           2008 standard including the additions of the Technical
#           Specification (TS) 29113 on Further Interoperability of Fortran
#           with C and TS 18508 on Additional Parallel Features in Fortran.


debug_OPTS = -O0 -fbacktrace -fcheck=bounds -fcheck=array-temps -fcheck=do -fcheck=mem -DDEBUG
###as flags -fcheck=pointer e -fcheck=recursive (inclusas no -fcheck=all) estavam gerando problemas no gdb

#trap
trap_OPTS = -ffpe-trap=invalid,zero,overflow,underflow,denormal
##> ‘invalid’ (invalid floating point operation, such as SQRT(-1.0)),
##> ‘zero’ (division by zero),
##> ‘overflow’ (overflow in a floating point operation),
##> ‘underflow’ (underflow in a floating point operation),
##> ‘inexact’ (loss of precision during operation), and
##> ‘denormal’ (operation performed on a denormal value). 
## The first three exceptions (‘invalid’, ‘zero’, and ‘overflow’) often indicate serious errors, and unless the program has provisions for dealing with these exceptions, enabling traps for these three exceptions is probably a good idea. 
### (https://gcc.gnu.org/onlinedocs/gfortran/Debugging-Options.html)

#fast
fast_OPTS = -march=native -Ofast -fno-backtrace
### (https://wiki.gentoo.org/wiki/GCC_optimization/pt-br)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Lógica de seleção de modos de construção
mode ?= debug
#selection
ifeq ($(mode),debug)
  FCOPTS = $(BASIC_OPTS) $(debug_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),trap)
  FCOPTS = $(BASIC_OPTS) $(debug_OPTS) $(trap_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),fast)
  FCOPTS = $(BASIC_OPTS) $(fast_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),release)
  FCOPTS = $(BASIC_OPTS)
  LINK_OPTS = -static
else
  $(error mode value - "mode=debug" or "mode=fast" or "mode=trap")
endif

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

