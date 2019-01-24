program main

  use, intrinsic :: iso_c_binding

  use module1

    !EXAMPLE FROM http://www.idris.fr/eng/ada/biblis/ada-fftw-eng.html
    integer(kind=C_INT), parameter  :: n = 16
    complex(kind=C_DOUBLE_COMPLEX), dimension(n)  :: in, out
    type(C_PTR) :: p, p2
    integer i,j
    real fact
   
    do i=1,N
       j=i-1
       in(i)=cmplx(j*j,1)
    enddo
   
    write(*,*)"create plans"
   
    p  = fftw_plan_dft_1d(N,in,out,FFTW_FORWARD,FFTW_ESTIMATE)
    p2 = fftw_plan_dft_1d(N,in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
   
    write(*,*)"do it"
   
    call fftw_execute_dft(p, in, out)
    do i=1,N
       write(*,"(f12.4,1x,f12.4)")out(i)
    enddo
   
    write(*,*)
    write(*,*)"undo it"
   
    call fftw_execute_dft(p2, out,in)
    fact=1.0/N
    do i=1,N
       write(*,"(f10.2,1x,f10.2)")in(i)*fact
    enddo
   
    write(*,*)"clean up"
    call fftw_destroy_plan(p)
    call fftw_destroy_plan(p2)
   
  end program main
