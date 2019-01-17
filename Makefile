# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Instruções

# Para executar as receitas e construir o programa basta digitar
## make

# Para forçar recompilar todos os arquivos do projeto,
# independentemente da verificação de hora de modificação
## make -B

# Para apagar todos os arquivos objeto temporários e programa final
## make clean

# Para (re-)compilar e rodar o programa
## make run

# Para (re-)compilar e rodar o programa no modo de verificação de memória
## make memcheck

# Para (re-)compilar e rodar o programa no modo de debug step-by-step
## make debug

# Para (re-)compilar e rodar o programa no modo de debug step-by-step
## make debug

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# mode: pode mudar aqui para fast ou trap se quiser usar os modos de código acelerado (e sem rastreamento de erros) ou modo trap (com detecção de infinity, nan, etc,...)

mode ?= debug

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Arquivos e receitas do projeto:

## Receita para o programa final:
$(mode)/bin/main.elf: $(mode)/obj/main.o $(mode)/obj/module1.o
	make version
	$(LINKER) $(LINK_OPTS) $^ -o $@

## Receita para cada objeto:
$(mode)/obj/main.o: src/main.f90 $(mode)/obj/module1.o Makefile
	$(COMPILER) $(FCOPTS) -J$(mode)/obj -c $< -o $@

$(mode)/obj/module1.o: src/module1.f90 Makefile
	$(COMPILER) $(FCOPTS) -J$(mode)/obj -c $< -o $@

#$(mode)/obj/module2.o: src/module2.f90 $(mode)/obj/module1.o Makefile
#	$(COMPILER) $(FCOPTS) -J$(mode)/obj -c $< -o $@

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Makefile keywords

build: .FORCE
	make $(mode)/bin/main.elf mode=$(mode)

run: .FORCE
	make $(mode)/bin/main.elf mode=$(mode)
	$(mode)/bin/main.elf

debug: .FORCE
	make debug/bin/main.elf mode=debug
	# - - - - - - - - - - - - - - - - - - - - - - - #
	# gdb CheatSheet:                               #
	#                                               #
	# > start                                       #
	# > s                 #(step)                   #
	# > break main.f90:15 #(set breakpoint)         #
	# > c                 #(continue)               #
	# > n                 #(next)                   #
	# > p x               #(print x)                #
	# > finish #(step out)                          #
	# > q #(quit)                                   #
	#                                               #
	# > - - - - - - - - - - - - - - - - - - - - - - #
	gdb debug/bin/main.elf

memcheck: .FORCE
	make debug/bin/main.elf mode=debug
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes debug/bin/main.elf

clean: .FORCE
	rm -f $(mode)/bin/*.elf
	rm -f $(mode)/obj/*.o
	rm -f $(mode)/obj/*.mod
	rm -f $(mode)/bin/version.txt

version: .FORCE
	git log -1 --pretty=format:"commit %H%n" > $(mode)/bin/version.txt #hash
	git log -1 --pretty=format:"Date: %ad%n" >> $(mode)/bin/version.txt #date
	git status -sb >> $(mode)/bin/version.txt #status

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais do Makefile:

#Compiler and linker
COMPILER = gfortran
LINKER = gfortran

#flags for each mode
BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none -g -pedantic -std=f2008ts

debug_OPTS = -O0 -fbacktrace -fcheck=bounds -fcheck=array-temps -fcheck=do -fcheck=mem
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
  FCOPTS = $(BASIC_OPTS) $(fast_OPTS)
  LINK_OPTS = -static
else
  $(error mode value - "mode=debug" or "mode=fast" or "mode=trap")
endif

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais para as regras de construção

#default target keyword
.DEFAULT_GOAL := build

# phony target .FORCE to force executing keyword recipes ignoring like-named files
.PHONY: .FORCE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
