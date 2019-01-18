module modelo1_mod

use modelos_mod
!use Variaveis_Globais

implicit none

	type, extends(modelos) :: modelo_1
		contains
		procedure :: resolve => resolve_1
	end type modelo_1

	contains

	function i_modelo_1()
		type(modelo_1) :: i_modelo_1
	end function i_modelo_1

	!----------------------------------------------------------------------
	subroutine resolve_1(z,varent, varsai, param)
		class(modelo_1) :: z
		!declaration
		real(8) :: varent(:), varsai(:)
		real(8) :: param(npar)

		!Variaveis do Usuario
		
		!modelo
		integer :: i, j
		character(100) :: string
		real(8) :: Psat_w_ice, T, PSATpar(3), DT, RT, iRT
		
		T = varent(1)
		PSATpar(:) = param(:)
		DT = T-273.16d0
		RT = (1.d0 - T/273.16d0)
		iRT = (273.16d0/T - 1.d0)

		Psat_w_ice = 611.657*dexp(PSATpar(1)*(RT)*(dabs(RT))**(PSATpar(2)-1.d0)+PSATpar(3)*(iRT))
		
		varsai(1) = Psat_w_ice
		
	end subroutine resolve_1

	!-----------------------------------------------------------
end module modelo1_mod
