PROGRAM main

  use iso_fortran_env, ONLY: REAL64, output_unit

  use module1

  integer :: i
  real(REAL64) :: X
  
  i=2
  x=3.d0
  
  write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', i, x
  
  call subS1(j=i,y=x)
  
  write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', i, x
  
  call subM1S1(k=i,z=x)
  
  write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', i, x

CONTAINS

  subroutine subS1(j,y)
    integer, intent(in) :: j
    real(REAL64), intent(out) :: y
    write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', j, y
    y = 2.d0**j
    write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', j, y
  end subroutine
  
END PROGRAM main
