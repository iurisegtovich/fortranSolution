MODULE module1
  use iso_fortran_env, ONLY: REAL64, output_unit

CONTAINS

  function Tfus( xs ) result( T_K )

  !arguments
    real( REAL64 ),  intent( in ) :: xs !mole fraction of solute
    real( REAL64 ) :: T_K
    real( REAL64 ) :: step
  !local
    integer :: i
    integer,  parameter :: IMAX=10
  !allocation
    
    T_K=273.15d0 !initial guess
  
    
  !implementation
    do i=1,  imax
  
      step = Res( xs, T_K ) / dResdT( xs, T_K )
      
      T_K = T_K - step

     if( dabs( step ) < 1d-9 ) then
        exit
     endif
     
     if ( i==imax ) then
       T_K=1.d5
       exit
     endif
     
    enddo
    
    contains
  
!
!----------------------------------------------------------------------------------------------------------------------------------------------------
!
  
    function Res( xs, T_K )
    !arguments
      real( REAL64 ),  intent( in ) :: xs
      real( REAL64 ),  intent( in ) :: T_K
      real( REAL64 ) :: Res !numerical method residue
    !local
      real( REAL64 ),  parameter :: dHfusw=6009.5d0 ![J.mol^ - 1] água - gelo Ref{Haghighi,  2009,  Thesis}
      real( REAL64 ),  parameter :: dCp=38.9016d0 ![J.mol^ - 1.K^ - 1] Diferença de Cps  água - gelo a aproximadamente 0°C. Ref: Engineering Toolbox
      real( REAL64 ) :: gammaw
      real( REAL64 ) :: xw
      real( REAL64 ),  parameter :: R = 8.31447d0 ![SI]
      real( REAL64 ),  parameter :: Tstmw=273.15d0 ![K] !standard melting point
    
    !allocation

    !implementation
      xw=1.d0-xs
      gammaw=func_GAMMAw( xs, T_K )

      Res = -dlog( xw * gammaw ) + ( ( - 1. / ( T_K * R ) ) * ( dHfusw - ( Tstmw * dCp ) ) ) + ( dHfusw / ( R * Tstmw ) ) + ( ( dCp / R ) * dlog( T_K / Tstmw ) ) - ( dCp / R )
   
    end function Res
  
!
!----------------------------------------------------------------------------------------------------------------------------------------------------
!
  
    function dResdT( xs, T_K )
    !arguments
      real( REAL64 ),  intent( in ) :: xs !molar fraction of solute
      real( REAL64 ),  intent( in ) :: T_K
      real( REAL64 ) :: dResdT !numerial derivative of Res
    !local
      real( REAL64 ),  parameter :: dT = 1.d-5
    !allocation
      
    !implementation
      dResdT = ( Res( xs, T_K + dT ) - Res( xs, T_K -dT ) ) / ( 2.d0 * dT )
    end function dResdT
    
    function func_GAMMAw( xs, T_K ) result(gammaw)
    !SAMPLE MARGULES LIKE GAMMA
    !arguments
      real( REAL64 ),  intent( in ) :: xs !molar fraction of solute
      real( REAL64 ),  intent( in ) :: T_K
      real( REAL64 ) :: gammaw !numerial derivative of Res
      real( REAL64 ), parameter :: A=1.

      gammaw=exp(A*xs**2)

    end function func_GAMMAw
    
  end function Tfus

END MODULE
