!mat_product_3band_mul.f90
!      module mat_product_3band_mul
!
!     Written by H. Matsui on Apr, 2009
!
!
!*
!*               | a(2,1)  a(1,2)  ........     0         0     |
!*               | a(3,1)  a(2,2)  ........     .         .     |
!*               |   0     a(3,2)  ........     .         .     |
!*    a(i,j)  =  |   .       0     ........     0         .     |
!*               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!*               |   .       .     ........  a(1,N-2)     0     |
!*               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!*               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!
!   Original band matrix
!      band_a(i-j+iband+1,j) = a(i,j)
!      band_a(k,j) = a(k+j-iband-1,j)
!   3-band matrix
!      band_a(i-j+2,j) = a(i,j)
!      band_a(k,j) = a(k+j-2,j)
!
!
!      subroutine cal_mat_product_3band_mul(n, mcomp, kr_st, kr_ed,     &
!     &          a_left, a_right, a_prod)
!
!       Evaluate product of matrix  (a_prod) = (a_left)(a_right)
!
      module mat_product_3band_mul
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
!      call cal_mat_product_3band_mul(nidx_rj(1), nidx_rj(2), kr_st, kr_ed, &
!     &wt_evo_mat, vs_poisson_mat, vp_evo_mat)
      subroutine cal_mat_product_3band_mul(n, mcomp, kr_st, kr_ed,      &
     &          a_left, a_right,  a_prod)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: n, mcomp
      real(kind = kreal), intent(in) :: a_left(3,n,mcomp)
      real(kind = kreal), intent(in) :: a_right(3,n,mcomp)
!
      real(kind = kreal), intent(inout) :: a_prod(5,n,mcomp)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_st+2, kr_ed-2
        do j = 1, mcomp
          a_prod(5,k-2,j) =  a_left(3,k-1,j) * a_right(3,k-2,j)
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
          a_prod(1,k+2,j) =  a_left(1,k+1,j) * a_right(1,k+2,j)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j)
        do j = 1, mcomp
          k = kr_st
          a_prod(3,k,  j) = one
          a_prod(2,k+1,j) = zero
          a_prod(1,k+2,j) = zero
!
          k = kr_st + 1
!          a_prod(5,k-2,j) =  zero
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
          a_prod(1,k+2,j)  = a_left(1,k+1,j) * a_right(1,k+2,j)
!
          k = kr_ed - 1
          a_prod(5,k-2,j) =  a_left(3,k-1,j) * a_right(3,k-2,j)
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
!          a_prod(1,k+2,j) = zero
!
          k = kr_ed
          a_prod(5,k-2,j) = zero
          a_prod(4,k-1,j) = zero
          a_prod(3,k,  j) = one
        end do
!$omp end parallel do
!
      end subroutine cal_mat_product_3band_mul
!
! -----------------------------------------------------------------------
!
      end module mat_product_3band_mul
