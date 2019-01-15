PROGRAM main

  use iso_fortran_env, ONLY: REAL64, output_unit

  use module1

  integer :: i
  real(REAL64) :: X

CONTAINS

  subroutine subS1(j,y)
    integer, intent(in) :: j
    real(REAL64), intent(out) :: y
    y = 2.d0**j
  end subroutine
  
END PROGRAM main
