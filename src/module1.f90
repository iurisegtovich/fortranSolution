MODULE module1
  use iso_fortran_env, ONLY: REAL64, output_unit

CONTAINS

SUBROUTINE pitzer(mNa,mCl,mCa,mSO4,gamma_Ca,gamma_SO4)

!Uses the Pitzer equations to calculate the activity coefficients of Ca and SO4
!and the activity of H2O in Ca-Na-Cl-SO4 solutions. Illustrates the use of the Pitzer equations.
!Equation numbers from Harvie & Weare (1980) are indicated by HW().

IMPLICIT none

REAL(REAL64),INTENT(in) :: mNa, mCl, mSO4, mCa
REAL(REAL64),INTENT(out):: gamma_Ca, gamma_SO4
REAL(REAL64) :: v, w, y, f_gamma, Z, F, IS, ISp
REAL(REAL64) :: phi_NaCa, phip_NaCa, phiphi_NaCa, Etheta, Ethetap
REAL(REAL64) :: phi_ClSO4, phip_ClSO4, phiphi_ClSO4
REAL(REAL64) :: term1, term1a, term1b, term2, term3
REAL(REAL64) :: term4, term5, term6, term7, term8, term9
REAL(REAL64) :: terma, termb, termc, termd
REAL(REAL64) :: CNaCl, CNaSO4, CCaCl, CCaSO4
REAL(REAL64) :: ln_gamma_Ca, ln_gamma_SO4, osmotic, sum_m
!parameter data are from Harvier & Weare (1980).
REAL(REAL64),PARAMETER :: Aphi = 0.392, b = 1.2
REAL(REAL64),PARAMETER :: alpha = 2.0, alpha1 = 1.4, alpha2 = 12.0
REAL(REAL64),PARAMETER :: theta_NaCa = 0.07, theta_ClSO4 = 0.02
REAL(REAL64),PARAMETER :: psi_NaCaCl = -0.014, psi_NaCaSO4 = -0.023
REAL(REAL64),PARAMETER :: psi_ClSO4Na = 0.0014, psi_ClSO4Ca = 0.0
REAL(REAL64),PARAMETER :: CphiNaCl = 0.00127, CphiNaSO4 = 0.00497, CphiCaCl = -0.00034, CphiCaSO4 = 0.0
REAL(REAL64),DIMENSION(3),PARAMETER :: BNaCl(3)= [0.07650, 0.2664, 0.0]
REAL(REAL64),DIMENSION(3),PARAMETER :: BNaSO4(3) = [0.01958, 1.1130, 0.0]
REAL(REAL64),DIMENSION(3),PARAMETER :: BCaCl(3) = [0.31590, 1.6140, 0.0]
REAL(REAL64),DIMENSION(3),PARAMETER :: BCaSO4(3) = [0.20000, 2.6500, -57.70]


!SOME PRELIMINARIES:
!===================
!sum of all m terms
sum_m = mNa+mCa+mCl+mSO4

!Ionic strength functions
IS = (mNa+mCl+4*mCa+4*mSO4)/2
ISp = sqrt(IS)

!some commonly used parameters
v = alpha *ISp
w = alpha1*ISp
y = alpha2*ISp
Z = mNa+mCl+2*mCa+2*mSO4
!convert Cphi to C

 CNaCl = CphiNaCl /2
 CNaSO4 = CphiNaSO4/(2*sqrt(2.0))
 CCaCl = CphiCaCl /(2*sqrt(2.0))
 CCaSO4 = CphiCaSO4/4

!calculate unsymmetrical mixing terms Etheta, Ethetap

CALL mixing(IS,Etheta,Ethetap)
!calculate phi terms: interactions between ions of like sign
phiphi_NaCa = theta_NaCa + Etheta + IS*Ethetap
phi_NaCa = theta_NaCa + Etheta
phip_NaCa = Ethetap
phiphi_ClSO4 = theta_ClSO4 + Etheta + IS*Ethetap
phi_ClSO4 = theta_ClSO4 + Etheta
phip_ClSO4 = Ethetap

!D-H TERM:
!=========
term1 = mNa*mCl *(BNaCl(2) *gp(v)/IS) + mNa*mSO4*(BNaSO4(2)*gp(v)/IS) + mCa*mCl *(BCaCl(2) *gp(v)/IS) + mCa*mSO4*(BCaSO4(2)*gp(w)/IS + BCaSO4(3)*gp(y)/IS)

term1a = mNa*mCa *phip_NaCa
term1b = mCl*mSO4*phip_ClSO4
f_gamma = -Aphi*((ISp/(1+b*ISp)) + (2/b)*(log(1+b*ISp)))
F = f_gamma + term1 + term1a + term1b

!ACTIVITY COEFFICIENT OF Ca:
!===========================
term2 = mCl *(2*(BCaCl(1) + BCaCl(2) *g(v)) + Z*CCaCl)+ mSO4*(2*(BCaSO4(1)+ BCaSO4(2)*g(w) + BCaSO4(3)*g(y)) + Z*CCaSO4)

term3 = mNa*(2*phi_NaCa + mCl*psi_NaCaCl + mSO4*psi_NaCaSO4)
term4 = mCl*mSO4*psi_ClSO4Ca
term5 = 2*(mNa*mCl*CNaCl + mNa*mSO4*CNaSO4 + mCa*mCl*CCaCl + mCa*mSO4*CCaSO4)

ln_gamma_Ca = 4*F + term2 + term3 + term4 + term5
gamma_Ca = exp(ln_gamma_Ca)

!ACTIVITY COEFFICIENT OF SO4:
!============================
term6 = mNa*(2*(BNaSO4(1) + BNaSO4(2)*g(v)) + Z*CNaSO4)+ mCa*(2*(BCaSO4(1) + BCaSO4(2)*g(w) + BCaSO4(3)*g(y)) + Z*CCaSO4)
term7 = mCl*(2*phi_ClSO4 + mNa*psi_ClSO4Na + mCa*psi_ClSO4Ca)
term8 = mNa*mCa*psi_NaCaSO4
term9 = term5
ln_gamma_SO4 = 4*F + term6 + term7 + term8 + term9
gamma_SO4 = exp(ln_gamma_SO4)


CONTAINS
function g(x)

IMPLICIT none
REAL(REAL64) :: g,x
g = 2*(1-(1+x)*exp(-x))/x**2
END function g
function gp(x)
IMPLICIT none
REAL(REAL64) :: gp,x
gp = -2*(1-(1+x+x**2/2)*exp(-x))/x**2
END function gp

END SUBROUTINE pitzer

SUBROUTINE mixing(I,Etheta,Ethetap)
!Evaluates unsymmetrical mixing terms Etheta, Ethetap, using the Chebyshev approximation
!mentioned in Harvie & Weare (1980), Appendix.

IMPLICIT none

REAL(REAL64),INTENT(in) :: I
REAL(REAL64),INTENT(out) :: Etheta,Ethetap
INTEGER :: m,k
REAL(REAL64) :: x,xMN,xMM,xNN,z,dzdx,JMN,JMM,JNN,JpMN,JpMM,JpNN
REAL(REAL64),PARAMETER :: Aphi = 0.392
REAL(REAL64),DIMENSION(0:20,2) :: ak
REAL(REAL64),DIMENSION(0:22) :: bk,dk

!array ak values are from Pitzer (1991) Table B-1, and copied from file phrqpitz.for
!in the USGS phrqptz distribution

ak(0:20,1) = (/1.925154014814667d0, -0.060076477753119d0, -0.029779077456514d0,	&
-0.007299499690937d0, 0.000388260636404d0, 0.000636874599598d0,			&
0.000036583601823d0, -0.000045036975204d0, -0.000004537895710d0,			&
0.000002937706971d0, 0.000000396566462d0, -0.000000202099617d0,			&
-0.000000025267769d0, 0.000000013522610d0, 0.000000001229405d0,			&
-0.000000000821969d0, -0.000000000050847d0, 0.000000000046333d0,			&
0.000000000001943d0, -0.000000000002563d0, -0.000000000010991d0/)

ak(0:20,2) = (/0.628023320520852d0, -0.028796057604906d0,0.006519840398744d0,			&
-0.000242107641309d0, -0.000004583768938d0, 0.000000216991779d0,				&
-0.000000006944757d0, 0.462762985338493d0, 0.150044637187895d0,				&
-0.036552745910311d0, -0.001668087945272d0, 0.001130378079086d0, -0.000887171310131d0,	&
0.000087294451594d0, 0.000034682122751d0, -0.000003548684306d0, -0.000000250453880d0,	&
0.000000080779570d0, 0.000000004558555d0, -0.000000002849257d0, 0.000000000237816d0/)

bk(21:22)=0. 
dk(21:22)=0.
!Originally used uninitialized variables, caught by valgrind, should these be set to zero, according to theory: pitzer, ACTIVITY COEFFICIENTS IN ELCTROLYTE SOLUTIONS, 1991 (PAG 125)

!zCa = +2; zNa = +1 and zSO4 = -2; zCl = 1
!so if M is Ca or SO4 and N is Na or Cl,

xMN = 6*2*Aphi*sqrt(I)
! i.e., 6*zCa*zNa*0.0392*sqrt(I); 6*2*zCl*zSO4*0.0392*sqrt(I)

xMM = 6*4*Aphi*sqrt(I)
! i.e., 6*zCa*zCa*0.0392*sqrt(I); 6*4*zSO4*zSO4*0.0392*sqrt(I)

xNN = 6*1*Aphi*sqrt(I)
! i.e., 6*zNa*zNa*0.0392*sqrt(I); 6*1*zCl*zCl*0.0392*sqrt(I)
!this DO loop evaluates the J functions as described in Pitzer (1991) appendix B.

DO k=1,3
IF (k==1) x=xMN
IF (k==2) x=xMM
IF (k==3) x=xNN

IF (x <= 1) THEN
z = 4.0d0 * x**(0.2d0) - 2.0d0
dzdx = (0.8d0) * x**(-0.8d0)

DO m =20,0,-1
bk(m) = z*bk(m+1) - bk(m+2) + ak(m,1)
dk(m) = bk(m+1) + z*dk(m+1) - dk(m+2)
END DO

ELSE
z = (40.0d0/9.0d0) * x**(-0.1d0) - 22.0d0/9.0d0
dzdx = -(40.0d0/90.0d0) * x**(-1.1d0)

DO m=20,0,-1
bk(m) = z*bk(m+1) - bk(m+2) + ak(m,2)
dk(m) = bk(m+1) + z*dk(m+1) - dk(m+2)
END DO

END IF

IF (k==1) THEN
JMN = 0.25d0*x -1.0d0 + 0.5d0*(bk(0) - bk(2))
JpMN = 0.25d0 + 0.5d0*(dzdx)*(dk(0) - dk(2))

ELSE IF (k==2) THEN
JMM = 0.25d0*x -1.0d0 + 0.5d0*(bk(0) - bk(2))
JpMM = 0.25d0 + 0.5d0*(dzdx)*(dk(0) - dk(2))

ELSE
JNN = 0.25d0*x -1d0 + 0.5d0*(bk(0) - bk(2))
JpNN = 0.25d0 + 0.5d0*(dzdx)*(dk(0) - dk(2))
END IF
END DO
!finally, calculation of the Etheta terms.
Etheta = (2.0d0/(4.0d0*I)) * (JMN - 0.5d0*JMM - 0.5d0*JNN)
! HW(A2)
Ethetap = -(Etheta/I) + (2.0d0/(8.0d0*I**2))*(xMN*JpMN - 0.5d0*xMM*JpMM - 0.5d0*xNN*JpNN)
! HW(A3)

END SUBROUTINE mixing
  
END MODULE
