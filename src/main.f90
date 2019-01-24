program main

        integer, parameter :: N = 1000

        real(8) :: in(N)

        complex(8) :: out(N/2 + 1)

        integer(8) :: plan
        
        integer(8) :: FFTW_ESTIMATE !?

        print*, out

        call dfftw_plan_dft_r2c_1d(plan,N,in,out,FFTW_ESTIMATE)
        
        call dfftw_execute_dft_r2c(plan, in, out)
        
        call dfftw_destroy_plan(plan)
	
	    print*, out
	
end program main
