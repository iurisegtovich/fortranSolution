MODULE module1
  use iso_fortran_env, ONLY: REAL64, output_unit

  integer :: mi
  real(REAL64) :: mx

CONTAINS

  subroutine subM1S1(k,z)
    integer, intent(in) :: k
    real(REAL64), intent(out) :: z
    write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', k, z
    z = 2.d0**(-k)
    write(*,'(A,A,I0.0,A,I0.0,ES12.4)') __FILE__,':',__LINE__, ' -> ', k, z
  end subroutine
  
END MODULE
