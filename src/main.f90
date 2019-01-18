PROGRAM main

  use iso_fortran_env, ONLY: REAL64, output_unit

  use module1

    real( REAL64 ) :: xs !mole fraction of solute
    real( REAL64 ) :: T_K
    integer :: i

    xs=.0
    do i = 1, 100
      xs=xs+1./1000.
      print*, Tfus(xs)
      xs=xs+1./1000.
    enddo
  
END PROGRAM main

