!>@file   lubksb_357band_mul2.f90
!!@brief  module lubksb_357band_mul2
!!
!!@author H. Matsui
!!@date Programmed on 2007
!
!>@brief  Solve more than one 3, 5, or 7 band matriox at once
!!         after LU decomposition
!!
!!@verbatim
!!---------------------------------------------------------------------
!!      subroutine lubksb_3band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!      subroutine lubksb_5band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!      subroutine lubksb_7band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!      subroutine lubksb_9band_mul2(Msmp, Msmp_stack, mcomp, n,        &
!!     &          band_lu, i_pivot, x)
!!
!! solve the set of n linear equations Ax=b. Here is band_lu input 
!! matrix after LU decompsition, determined by the routine ludcmp_band.
!! i_pivot is input as the
!! permutation vector returned by ludcmp. b(1:n) is input
!! as the right-hand side vectror b, and returns with the
!! solution vector x.
!!
!! band_lu, n, np and i_pivot are not modified by this routine
!! and be left in place for successive calls with different
!! right hand sides b. This routine takes into account
!! the possibility that b will begin with many zero elements,
!! so it is efficient for use in matrix inversion.
!!
!!
!!   Format of band matrix
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Arbitorary band matrix
!!      band_a(i-j+iband+1,j) = a(i,j)
!!      band_a(k,j) = a(k+j-iband-1,j)
!!   3-band matrix
!!      band_a(i-j+2,j) = a(i,j)
!!      band_a(k,j) = a(k+j-2,j)
!!   5-band matrix
!!      band_lu(i-j+3,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-3,j)
!!   7-band matrix
!!      band_lu(i-j+4,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-4,j)
!!   9-band matrix
!!      band_lu(i-j+5,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-5,j)
!!---------------------------------------------------------------------
!!@endverbatim
!
      module lubksb_357band_mul2
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_3band_mul2(Msmp, Msmp_stack, mcomp, n,          &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(mcomp,n)
      real(kind = kreal), intent(in) :: band_lu(mcomp,5,n)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(m,i)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                  - band_lu(mst:med,4,1) * x(mst:med,1)
        do i = 3, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(mst:med,5,i-2)*x(mst:med,i-2)         &
     &                  - band_lu(mst:med,4,i-1)*x(mst:med,i-1)
        end do
!
!        x(mst:med,n)   =  x(mst:med,n)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                  - band_lu(mst:med,2,n) * x(mst:med,n))
!
        do i = n-2,1,-1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                  - band_lu(mst:med,2,i+1) * x(mst:med,i+1)       &
     &                  - band_lu(mst:med,1,i+2) * x(mst:med,i+2) )
        end do
!
        do i = 1, n
          x(mst:med,i) =   x(mst:med,i) / band_lu(mst:med,3,i)
        end do
      end do
!$omp end parallel do
!
      end subroutine lubksb_3band_mul2
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band_mul2(Msmp, Msmp_stack, mcomp, n,          &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(mcomp,n)
      real(kind = kreal), intent(in) :: band_lu(mcomp,9,n)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(m,i)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
        x(mst:med,2) = x(mst:med,2) - band_lu(mst:med,6,1)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3) - band_lu(mst:med,7,1)*x(mst:med,1) &
     &                              - band_lu(mst:med,6,2)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4) - band_lu(mst:med,8,1)*x(mst:med,1) &
     &                              - band_lu(mst:med,7,2)*x(mst:med,2) &
     &                              - band_lu(mst:med,6,3)*x(mst:med,3)
        do i = 5, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(mst:med,9,i-4)*x(mst:med,i-4)         &
     &                  - band_lu(mst:med,8,i-3)*x(mst:med,i-3)         &
     &                  - band_lu(mst:med,7,i-2)*x(mst:med,i-2)         &
     &                  - band_lu(mst:med,6,i-1)*x(mst:med,i-1)
        end do
!
!        x(mst:med,n) =    x(mst:med,n)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(4,n,  mst:med) * x(mst:med,n  ))
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(mst:med,4,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(3,n,  mst:med) * x(mst:med,n  ))
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(mst:med,4,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,3,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,2,n  ) * x(mst:med,n  ))
        do i = n-4, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(mst:med,4,i+1) * x(mst:med,i+1)      &
     &                   - band_lu(mst:med,3,i+2) * x(mst:med,i+2)      &
     &                   - band_lu(mst:med,2,i+3) * x(mst:med,i+3)      &
     &                   - band_lu(mst:med,1,i+4) * x(mst:med,i+4))
        end do
!
        do i = 1, n
          x(mst:med,i) =   x(mst:med,i) / band_lu(mst:med,5,i)
        end do
      end do
!$omp end parallel do
!
      end subroutine lubksb_5band_mul2
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band_mul2(Msmp, Msmp_stack, mcomp, n,          &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(mcomp,n)
      real(kind = kreal), intent(in) :: band_lu(mcomp,13,n)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(m,i)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                - band_lu(mst:med,8, 1)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3)                                     &
     &                - band_lu(mst:med,9, 1)*x(mst:med,1)              &
     &                - band_lu(mst:med,8, 2)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4)                                     &
     &                - band_lu(mst:med,10,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,9, 2)*x(mst:med,2)              &
     &                - band_lu(mst:med,8, 3)*x(mst:med,3)
        x(mst:med,5) = x(mst:med,5)                                     &
     &                - band_lu(mst:med,11,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,10,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,9, 3)*x(mst:med,3)              &
     &                - band_lu(mst:med,8, 4)*x(mst:med,4)
        x(mst:med,6) = x(mst:med,6)                                     &
     &                - band_lu(mst:med,12,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,11,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,10,3)*x(mst:med,3)              &
     &                - band_lu(mst:med,9, 4)*x(mst:med,4)              &
     &                - band_lu(mst:med,8, 5)*x(mst:med,5)
        do i = 7, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(mst:med,13,i-6)*x(mst:med,i-6)        &
     &                  - band_lu(mst:med,12,i-5)*x(mst:med,i-5)        &
     &                  - band_lu(mst:med,11,i-4)*x(mst:med,i-4)        &
     &                  - band_lu(mst:med,10,i-3)*x(mst:med,i-3)        &
     &                  - band_lu(mst:med,9, i-2)*x(mst:med,i-2)        &
     &                  - band_lu(mst:med,8, i-1)*x(mst:med,i-1)
        end do
!
!        x(mst:med,n) =   x(mst:med,n)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(mst:med,6,n  ) * x(mst:med,n  ))
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(mst:med,6,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,5,n  ) * x(mst:med,n  ))
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(mst:med,6,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,5,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,4,n  ) * x(mst:med,n  ))
        x(mst:med,n-4) = (x(mst:med,n-4)                                &
     &                   - band_lu(mst:med,6,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,5,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,4,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,3,n  ) * x(mst:med,n  ))
        x(mst:med,n-5) = (x(mst:med,n-5)                                &
     &                   - band_lu(mst:med,6,n-4) * x(mst:med,n-4)      &
     &                   - band_lu(mst:med,5,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,4,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,3,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,2,n  ) * x(mst:med,n  ))
        do i = n-6, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(mst:med,6,i+1) * x(mst:med,i+1)      &
     &                   - band_lu(mst:med,5,i+2) * x(mst:med,i+2)      &
     &                   - band_lu(mst:med,4,i+3) * x(mst:med,i+3)      &
     &                   - band_lu(mst:med,3,i+4) * x(mst:med,i+4)      &
     &                   - band_lu(mst:med,2,i+5) * x(mst:med,i+5)      &
     &                   - band_lu(mst:med,1,i+6) * x(mst:med,i+6))
        end do
!
        do i = 1, n
          x(mst:med,i) =   x(mst:med,i) / band_lu(mst:med,7,i)
        end do
      end do
!$omp end parallel do
!
      end subroutine lubksb_7band_mul2
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_9band_mul2(Msmp, Msmp_stack, mcomp, n,          &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(mcomp,n)
      real(kind = kreal), intent(in) :: band_lu(mcomp,17,n)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(m,i)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                - band_lu(mst:med,10,1)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3)                                     &
     &                - band_lu(mst:med,11,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,10,2)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4)                                     &
     &                - band_lu(mst:med,12,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,11,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,10,3)*x(mst:med,3)
        x(mst:med,5) = x(mst:med,5)                                     &
     &                - band_lu(mst:med,13,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,12,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,11,3)*x(mst:med,3)              &
     &                - band_lu(mst:med,10,4)*x(mst:med,4)
        x(mst:med,6) = x(mst:med,6)                                     &
     &                - band_lu(mst:med,14,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,13,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,12,3)*x(mst:med,3)              &
     &                - band_lu(mst:med,11,4)*x(mst:med,4)              &
     &                - band_lu(mst:med,10,5)*x(mst:med,5)
        x(mst:med,7) = x(mst:med,7)                                     &
     &                - band_lu(mst:med,15,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,14,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,13,3)*x(mst:med,3)              &
     &                - band_lu(mst:med,12,4)*x(mst:med,4)              &
     &                - band_lu(mst:med,11,5)*x(mst:med,5)              &
     &                - band_lu(mst:med,10,6)*x(mst:med,6)
        x(mst:med,8) = x(mst:med,8)                                     &
     &                - band_lu(mst:med,16,1)*x(mst:med,1)              &
     &                - band_lu(mst:med,15,2)*x(mst:med,2)              &
     &                - band_lu(mst:med,14,3)*x(mst:med,3)              &
     &                - band_lu(mst:med,13,4)*x(mst:med,4)              &
     &                - band_lu(mst:med,12,5)*x(mst:med,5)              &
     &                - band_lu(mst:med,11,6)*x(mst:med,6)              &
     &                - band_lu(mst:med,10,7)*x(mst:med,7)
        do i = 9, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(mst:med,17,i-8)*x(mst:med,i-8)        &
     &                  - band_lu(mst:med,16,i-7)*x(mst:med,i-7)        &
     &                  - band_lu(mst:med,15,i-6)*x(mst:med,i-6)        &
     &                  - band_lu(mst:med,14,i-5)*x(mst:med,i-5)        &
     &                  - band_lu(mst:med,13,i-4)*x(mst:med,i-4)        &
     &                  - band_lu(mst:med,12,i-3)*x(mst:med,i-3)        &
     &                  - band_lu(mst:med,11,i-2)*x(mst:med,i-2)        &
     &                  - band_lu(mst:med,10,i-1)*x(mst:med,i-1)
        end do
!
!        x(mst:med,n) =   x(mst:med,n)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(mst:med,8,n  ) * x(mst:med,n  ))
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(mst:med,8,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,7,n  ) * x(mst:med,n  ))
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(mst:med,8,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,7,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,6,n  ) * x(mst:med,n  ))
        x(mst:med,n-4) = (x(mst:med,n-4)                                &
     &                   - band_lu(mst:med,8,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,7,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,6,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,5,n  ) * x(mst:med,n  ))
        x(mst:med,n-5) = (x(mst:med,n-5)                                &
     &                   - band_lu(mst:med,8,n-4) * x(mst:med,n-4)      &
     &                   - band_lu(mst:med,7,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,6,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,5,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,4,n  ) * x(mst:med,n  ))
        x(mst:med,n-6) = (x(mst:med,n-6)                                &
     &                   - band_lu(mst:med,8,n-5) * x(mst:med,n-5)      &
     &                   - band_lu(mst:med,7,n-4) * x(mst:med,n-4)      &
     &                   - band_lu(mst:med,6,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,5,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,4,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,3,n  ) * x(mst:med,n  ))
        x(mst:med,n-7) = (x(mst:med,n-7)                                &
     &                   - band_lu(mst:med,8,n-6) * x(mst:med,n-6)      &
     &                   - band_lu(mst:med,7,n-5) * x(mst:med,n-5)      &
     &                   - band_lu(mst:med,6,n-4) * x(mst:med,n-4)      &
     &                   - band_lu(mst:med,5,n-3) * x(mst:med,n-3)      &
     &                   - band_lu(mst:med,4,n-2) * x(mst:med,n-2)      &
     &                   - band_lu(mst:med,3,n-1) * x(mst:med,n-1)      &
     &                   - band_lu(mst:med,2,n  ) * x(mst:med,n  ))
        do i = n-8, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(mst:med,8,i+1) * x(mst:med,i+1)      &
     &                   - band_lu(mst:med,7,i+2) * x(mst:med,i+2)      &
     &                   - band_lu(mst:med,6,i+3) * x(mst:med,i+3)      &
     &                   - band_lu(mst:med,5,i+4) * x(mst:med,i+4)      &
     &                   - band_lu(mst:med,4,i+5) * x(mst:med,i+5)      &
     &                   - band_lu(mst:med,3,i+6) * x(mst:med,i+6)      &
     &                   - band_lu(mst:med,2,i+7) * x(mst:med,i+7)      &
     &                   - band_lu(mst:med,1,i+8) * x(mst:med,i+8))
        end do
!
        do i = 1, n
          x(mst:med,i) =   x(mst:med,i) / band_lu(mst:med,9,i)
        end do
      end do
!$omp end parallel do
!
      end subroutine lubksb_9band_mul2
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_11band_mul2(Msmp, Msmp_stack, mcomp, n,         &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(mcomp,n)
      real(kind = kreal), intent(in) :: band_lu(mcomp,21,n)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(m,i)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                 - band_lu(mst:med,12,1)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3)                                     &
     &                 - band_lu(mst:med,13,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,12,2)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4)                                     &
     &                 - band_lu(mst:med,14,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,13,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,12,3)*x(mst:med,3)
        x(mst:med,5) = x(mst:med,5)                                     &
     &                 - band_lu(mst:med,15,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,14,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,13,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,12,4)*x(mst:med,4)
        x(mst:med,6) = x(mst:med,6)                                     &
     &                 - band_lu(mst:med,16,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,15,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,14,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,13,4)*x(mst:med,4)             &
     &                 - band_lu(mst:med,12,5)*x(mst:med,5)
        x(mst:med,7) = x(mst:med,7)                                     &
     &                 - band_lu(mst:med,17,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,16,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,15,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,14,4)*x(mst:med,4)             &
     &                 - band_lu(mst:med,13,5)*x(mst:med,5)             &
     &                 - band_lu(mst:med,12,6)*x(mst:med,6)
        x(mst:med,8) = x(mst:med,8)                                     &
     &                 - band_lu(mst:med,18,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,17,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,16,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,15,4)*x(mst:med,4)             &
     &                 - band_lu(mst:med,14,5)*x(mst:med,5)             &
     &                 - band_lu(mst:med,13,6)*x(mst:med,6)             &
     &                 - band_lu(mst:med,12,7)*x(mst:med,7)
        x(mst:med,9) =  x(mst:med,9)                                    &
     &                 - band_lu(mst:med,19,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,18,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,17,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,16,4)*x(mst:med,4)             &
     &                 - band_lu(mst:med,15,5)*x(mst:med,5)             &
     &                 - band_lu(mst:med,14,6)*x(mst:med,6)             &
     &                 - band_lu(mst:med,13,7)*x(mst:med,7)             &
     &                 - band_lu(mst:med,12,8)*x(mst:med,8)
        x(mst:med,10) =  x(mst:med,10)                                  &
     &                 - band_lu(mst:med,20,1)*x(mst:med,1)             &
     &                 - band_lu(mst:med,19,2)*x(mst:med,2)             &
     &                 - band_lu(mst:med,18,3)*x(mst:med,3)             &
     &                 - band_lu(mst:med,17,4)*x(mst:med,4)             &
     &                 - band_lu(mst:med,16,5)*x(mst:med,5)             &
     &                 - band_lu(mst:med,15,6)*x(mst:med,6)             &
     &                 - band_lu(mst:med,14,7)*x(mst:med,7)             &
     &                 - band_lu(mst:med,13,8)*x(mst:med,8)             &
     &                 - band_lu(mst:med,12,9)*x(mst:med,9)
        do i = 11, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(mst:med,21,i-10)*x(mst:med,i-10)      &
     &                  - band_lu(mst:med,20,i-9 )*x(mst:med,i-9 )      &
     &                  - band_lu(mst:med,19,i-8 )*x(mst:med,i-8 )      &
     &                  - band_lu(mst:med,18,i-7 )*x(mst:med,i-7 )      &
     &                  - band_lu(mst:med,17,i-6 )*x(mst:med,i-6 )      &
     &                  - band_lu(mst:med,16,i-5 )*x(mst:med,i-5 )      &
     &                  - band_lu(mst:med,15,i-4 )*x(mst:med,i-4 )      &
     &                  - band_lu(mst:med,14,i-3 )*x(mst:med,i-3 )      &
     &                  - band_lu(mst:med,13,i-2 )*x(mst:med,i-2 )      &
     &                  - band_lu(mst:med,12,i-1 )*x(mst:med,i-1 )
        end do
!
!        x(mst:med,n) =   x(mst:med,n)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(mst:med,10,n  ) * x(mst:med,n  ))
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(mst:med,10,n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med, 9,n  ) * x(mst:med,n  ))
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(mst:med,10,n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,9, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,8, n  ) * x(mst:med,n  ))
        x(mst:med,n-4) = (x(mst:med,n-4)                                &
     &                   - band_lu(mst:med,10,n-3) * x(mst:med,n-3)     &
     &                   - band_lu(mst:med,9, n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,8, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,7, n  ) * x(mst:med,n  ))
        x(mst:med,n-5) = (x(mst:med,n-5)                                &
     &                   - band_lu(mst:med,10,n-4) * x(mst:med,n-4)     &
     &                   - band_lu(mst:med,9, n-3) * x(mst:med,n-3)     &
     &                   - band_lu(mst:med,8, n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,7, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,6, n  ) * x(mst:med,n  ))
        x(mst:med,n-6) = (x(mst:med,n-6)                                &
     &                   - band_lu(mst:med,10,n-5) * x(mst:med,n-5)     &
     &                   - band_lu(mst:med,9, n-4) * x(mst:med,n-4)     &
     &                   - band_lu(mst:med,8, n-3) * x(mst:med,n-3)     &
     &                   - band_lu(mst:med,7, n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,6, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,5, n  ) * x(mst:med,n  ))
        x(mst:med,n-7) = (x(mst:med,n-7)                                &
     &                   - band_lu(mst:med,10,n-6) * x(mst:med,n-6)     &
     &                   - band_lu(mst:med,9, n-5) * x(mst:med,n-5)     &
     &                   - band_lu(mst:med,8, n-4) * x(mst:med,n-4)     &
     &                   - band_lu(mst:med,7, n-3) * x(mst:med,n-3)     &
     &                   - band_lu(mst:med,6, n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,5, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,4, n  ) * x(mst:med,n  ))
        x(mst:med,n-8) = (x(mst:med,n-8)                                &
     &                   - band_lu(mst:med,10,n-7) * x(mst:med,n-7)     &
     &                   - band_lu(mst:med,9, n-6) * x(mst:med,n-6)     &
     &                   - band_lu(mst:med,8, n-5) * x(mst:med,n-5)     &
     &                   - band_lu(mst:med,7, n-4) * x(mst:med,n-4)     &
     &                   - band_lu(mst:med,6, n-3) * x(mst:med,n-3)     &
     &                   - band_lu(mst:med,5, n-2) * x(mst:med,n-2)     &
     &                   - band_lu(mst:med,4, n-1) * x(mst:med,n-1)     &
     &                   - band_lu(mst:med,3, n  ) * x(mst:med,n  ))
        x(mst:med,n-9) = (x(mst:med,n-9)                                &
     &                   - band_lu(mst:med,10,n-8 ) * x(mst:med,n-8)    &
     &                   - band_lu(mst:med,9, n-7 ) * x(mst:med,n-7)    &
     &                   - band_lu(mst:med,8, n-6 ) * x(mst:med,n-6)    &
     &                   - band_lu(mst:med,7, n-5 ) * x(mst:med,n-5)    &
     &                   - band_lu(mst:med,6, n-4 ) * x(mst:med,n-4)    &
     &                   - band_lu(mst:med,5, n-3 ) * x(mst:med,n-3)    &
     &                   - band_lu(mst:med,4, n-2 ) * x(mst:med,n-2)    &
     &                   - band_lu(mst:med,3, n-1 ) * x(mst:med,n-1)    &
     &                   - band_lu(mst:med,2, n   ) * x(mst:med,n  ))
        do i = n-10, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(mst:med,10,i+1 ) * x(mst:med,i+1)    &
     &                   - band_lu(mst:med,9, i+2 ) * x(mst:med,i+2)    &
     &                   - band_lu(mst:med,8, i+3 ) * x(mst:med,i+3)    &
     &                   - band_lu(mst:med,7, i+4 ) * x(mst:med,i+4)    &
     &                   - band_lu(mst:med,6, i+5 ) * x(mst:med,i+5)    &
     &                   - band_lu(mst:med,5, i+6 ) * x(mst:med,i+6)    &
     &                   - band_lu(mst:med,4, i+7 ) * x(mst:med,i+7)    &
     &                   - band_lu(mst:med,3, i+8 ) * x(mst:med,i+8)    &
     &                   - band_lu(mst:med,2, i+9 ) * x(mst:med,i+9)    &
     &                   - band_lu(mst:med,1, i+10) * x(mst:med,i+10))
        end do
!
        do i = 1, n
          x(mst:med,i) =   x(mst:med,i) / band_lu(mst:med,11,i)
        end do
      end do
!$omp end parallel do
!
      end subroutine lubksb_11band_mul2
!
! ----------------------------------------------------------------------
!
      end module lubksb_357band_mul2
