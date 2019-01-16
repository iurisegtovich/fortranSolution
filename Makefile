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
# Arquivos e receitas do projeto:

## Receita para o programa final:
bin/main.elf: obj/main.o obj/module1.o
	make version
	$(LINKER) $(FLOPTS) $^ -o $@

## Receita para cada objeto:
obj/main.o: src/main.f90 obj/module1.o Makefile
	$(COMPILER) $(FCOPTS) -Jobj -c $< -o $@

obj/module1.o: src/module1.f90 Makefile
	$(COMPILER) $(FCOPTS) -Jobj -c $< -o $@

#obj/module2.o: src/module2.f90 obj/module1.o Makefile
#	$(COMPILER) $(FCOPTS) -Jobj -c $< -o $@

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Makefile keywords

build: .FORCE
	make bin/main.elf mode=$(mode)
	#done

run: .FORCE
	make bin/main.elf mode=$(mode)
	./bin/main.elf

debug: .FORCE
	make bin/main.elf mode=DEBUG
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
	gdb bin/main.elf

memcheck: .FORCE
	make bin/main.elf mode=DEBUG
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes bin/main.elf

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

DEBUG_OPTS = -O0 -fcheck=all

TRAP_OPTS = -ffpe-trap=invalid,zero,overflow,underflow,precision,denormal

FAST_OPTS = -O3 -march=native -m64 -Ofast

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Lógica de seleção de modos de construção

#default mode
mode ?= DEBUG
#selection
ifeq ($(mode),DEBUG)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS)
  LINK_OPTS = -fbacktrace
else ifeq ($(mode),TRAP)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS) $(TRAP_OPTS)
  LINK_OPTS = -fbacktrace
else ifeq ($(mode),FAST)
  FCOPTS = $(BASIC_OPTS) $(FAST_OPTS)
  LINK_OPTS =
else ifeq ($(mode),RELEASE)
  FCOPTS = $(BASIC_OPTS) $(FAST_OPTS)
  LINK_OPTS = -static
else
  $(error mode value - "mode=DEBUG" or "mode=FAST" or "mode=TRAP")
endif

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais para as regras de construção

#default target keyword
.DEFAULT_GOAL := build

# phony target .FORCE to force executing keyword recipes ignoring like-named files
.PHONY: .FORCE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
