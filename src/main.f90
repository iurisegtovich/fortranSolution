program main
use module1
implicit none

REAL(8) :: mNa, mCl, mSO4, mCa
REAL(8):: gamma_Ca, gamma_SO4

mNa = 2
mCa = 1
mSO4 = 1
mCl = 2

call mixing(1.d0,gamma_Ca, gamma_SO4)
print*, gamma_Ca, gamma_SO4 !sqn


call pitzer(mNa,mCl,mCa,mSO4,gamma_Ca,gamma_SO4)
print*, gamma_Ca
print *, gamma_SO4
! write(*,'(E26.16E3)') gamma_SO4
! write(*,'(G26.16E3)') gamma_SO4
! write(*,'(D26.16)') gamma_SO4
! write(*,'(F26.16)') gamma_SO4

! write(*,'(F0.0)') gamma_SO4*100000000
! write(*,'(F0.10)') gamma_SO4*100000000
! write(*,'(E1.1)') gamma_SO4
! write(*,'(E1.10)') gamma_SO4

end program
