1. install git/gfortran/gcc toolchains

# windows
  1. Download
  
  https://gitforwindows.org/
  > Download Git for Windows SDK
  
  2. Configure
  
  Open git-sdk
  Configure mingw:
  $ pacman -sS gfortran
  $ pacman -S toolchain
  Change working dir
  cd /c/users/...
  

# linux
  [sudo] apt install gfortran gcc build-essentials make git


2. CLONE THE FORTRAN SOLUTION TEMPLATE AND TEST

git clone https://github.com/iurisegtovich/fortranSolution

tests:

make run -B mode=DEBUG
make run -B mode=TRAP
make run -B mode=FAST
     memcheck
                       static=TRUE

optionally for keep using git:
create a host repo (github/gitlab/...)
change origin
$ git remote add origin https://gitXXX.com/YOURUSERNAME/YOURPROJECT.git
$ git push -u origin master

# obs
> branch master has a template that is simplified with no dependency checks
> branch withDependency has a template with dependency checks (requires different dir for each mode)

