program main
use module1
implicit none

REAL(8) :: mNa, mCl, mSO4, mCa
REAL(8):: gamma_Ca, gamma_SO4

mNa = 2
mCa = 1
mSO4 = 1
mCl = 2

call pitzer(mNa,mCl,mCa,mSO4,gamma_Ca,gamma_SO4)
print*, gamma_Ca
print *, gamma_SO4

end program
