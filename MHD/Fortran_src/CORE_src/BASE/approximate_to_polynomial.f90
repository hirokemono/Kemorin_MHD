!>@file   approximate_to_polynomial.f90
!!
!! @author J-P Moreau, H. Matsui
!! @date   Programmed in  Sep., 2022
!!
!
!> @brief Subroutine to make polynomial approximation
!!      subroutine s_Approx_poly(n_data, m_poly, X, Y, A, SE)
!!        integer(kind = kint), intent(in) :: n_data, m_poly
!!        real(kind = kreal), intent(in) :: X(n_data), Y(n_data)
!!        real(kind = kreal), intent(inout) ::  A(m_poly+1)
!!        real(kind = kreal), intent(inout) ::  SE(m_poly+1)
!!*********************************************************
!!*    Approximation of a discrete real function F(x) by  *
!!*    least squares                                      *
!!* ----------------------------------------------------- *
!!* Ref.: "M騁hodes de calcul num駻ique, Tome 2 By Claude  *
!!*        Nowakowski, PSI Edition, 1984" [BIBLI 04].     *
!!* ----------------------------------------------------- *
!!* n_data: Number of points    : 11                      *
!!*                                                       *
!!* m_poly: Degree of polynomial: 3                       *
!!*                                                       *
!!* X, Y: Example of Function to approximate:             *
!!*  X(1), Y(1) = 0 0                                     *
!!*  X(2), Y(2) = 0.1 0.2                                 *
!!*  X(3), Y(3) = 0.2 0.4                                 *
!!*  X(4), Y(4) = 0.3 0.6                                 *
!!*  X(5), Y(5) = 0.4 0.8                                 *
!!*  X(6), Y(6) = 0.5 1                                   *
!!*  X(7), Y(7) = 0.6 0.8                                 *
!!*  X(8), Y(8) = 0.7 0.6                                 *
!!*  X(9), Y(9) = 0.8 0.4                                 *
!!*  X(10), Y(10) = 0.9 0.2                               *
!!*  X(11), Y(11) = 1 0                                   *
!!*                                                       *
!!* A: Example of Polynomial approximation of degree 3    *
!!*  from 11 points                                       *
!!* Coefficients of polynomial:                           *
!!*  A(0) =    -0.069930070                               *
!!*  A(1) =     3.496503497                               *
!!*  A(2) =    -3.496503497                               *
!!*  A(3) =     0.000000000                               *
!!*                                                       *
!!* B: Standard error of the coefficients                 *
!!*                                                       *
!!*                                                       *
!!* Example of Approximated function:                     *
!!*        X           Y                                  *
!!*    0.000000   -0.069930                               *
!!*    0.100000    0.244755                               *
!!*    0.200000    0.489510                               *
!!*    0.300000    0.664336                               *
!!*    0.400000    0.769231                               *
!!*    0.500000    0.804196                               *
!!*    0.600000    0.769231                               *
!!*    0.700000    0.664336                               *
!!*    0.800000    0.489510                               *
!!*    0.900000    0.244755                               *
!!*    1.000000   -0.069930                               *
!!*                                                       *
!!*                    F90 Version By J-P Moreau, Paris.  *
!!*                           (www.jpmoreau.fr)           *
!!*********************************************************
!!
!!@verbatim
!!@endverbatim
      module approximate_to_polynomial
!
      use m_precision
      use m_constants
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine approx_poly(n_data, m_poly, X, Y, A, SE)
!
      integer(kind = kint), intent(in) :: n_data, m_poly
      real(kind = kreal), intent(in) :: X(n_data), Y(n_data)
      real(kind = kreal), intent(inout) :: A(m_poly+1)
      real(kind = kreal), intent(inout) :: SE(m_poly+1)

      integer(kind = kint) i, ij, j, k, n, m1, m2
      real(kind = kreal), allocatable ::  C(:,:)
      real(kind = kreal), allocatable ::  B(:),Xc(:),Yx(:)
      real(kind = kreal), allocatable ::  p(:), epsilon(:)
      real(kind = kreal) :: s, yc
      real(kind = kreal) :: x_ave, ss, ss_xx

      allocate(p(n_data), epsilon(n_data))
      allocate(B(m_poly+2))
      allocate(Xc(m_poly+2), Yx(m_poly+2))
      allocate(C(m_poly+2,m_poly+2))

      n = n_data - 1
      m1=m_poly+1
      m2=m_poly+2
      do k=1, m2
        Xc(k)=0.d0
        do i=1, n_data
          Xc(k) = Xc(k) + X(i)**k
        end do
      end do
!
      yc=0.d0
      do i=1, n_data
        yc = yc + Y(i)
      end do
!
      do k=1, m_poly
        Yx(k)=0.d0
        do i=1, n_data
          Yx(k) =  Yx(k) + Y(i)*X(i)**k
        end do 
      end do
      do i=1, m1
        do j=1, m1
          ij=i+j-2
          if (i==1.and.j==1)  then
            C(1,1) = n_data
          else 
            C(i,j)=Xc(ij)
          end if 
        end do
      end do
!
      B(1)=yc
      do i=2,m1
        B(i)=Yx(i-1)
      end do 
!
      do k=1, m_poly
        do i=k+1, m1
          B(i) = B(i) - C(i,k)/C(k,k)*B(k)
          do j=k+1, m1
            C(i,j) = C(i,j) - C(i,k)/C(k,k)*C(k,j)
          end do
        end do
      end do
!
      A(m1)=B(m1)/C(m1,m1)
      do i=m_poly, 1, -1
        s=0.d0
        do k=i+1, m1  
          s = s + C(i,k)*A(k)
        end do 
        A(i) = (B(i)-s)/C(i,i)
      end do
!
      do i = 1, n_data
        p(i) = 0.d0
        do k = 1, m1
          p(i) = p(i)*X(i) + A(m1+1-k)
        end do 
      end do
!
      x_ave = 0.0d0
      do i=1, n_data
        x_ave = x_ave + X(i)
      end do
      x_ave = x_ave / dble(n_data)
!
      ss = 0.0d0
      ss_xx = 0.0d0
      do i=1, n_data
        epsilon(i) = Y(i) - p(i)
        ss = ss + epsilon(i)**2
        ss_xx = ss_xx + (X(i) - x_ave)**2
      end do
      ss = sqrt(ss / dble(n_data-2))
!
      SE(1) = ss * sqrt(1.0d0 / dble(n_data) + x_ave**2 / ss_xx)
      SE(2) = ss * sqrt(1.0d0 / ss_xx)
!
!      print *,' '
!      write(*,'(a,I2,a,I2,a)') ' Polynomial approximation of degree ', &
!     &            m_poly, ' (', n+1, ' points)'
!      print *,' Coefficients of polynomial:'
!      do i=1, m1
!        write(*,'(a,I1,a,2F15.9)') '    A(', i-1, ') = ', A(i), SE(i)
!      end do
!      print *,' ' 
!      print *,' Approximated function:'
!      print *,'       X           Y  '
!      do i=1, n_data
!        write(*,'(3F12.6)')  X(i), p(i), Y(i)
!      end do
!
      deallocate(B, C)
      deallocate(Xc, Yx, p, epsilon)
      end subroutine approx_poly
!
!   --------------------------------------------------------------------
!
      end module approximate_to_polynomial
