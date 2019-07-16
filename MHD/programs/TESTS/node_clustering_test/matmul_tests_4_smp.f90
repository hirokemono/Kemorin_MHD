!>@file   matmul_for_legendre_trans.F90
!!@brief  module matmul_for_legendre_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Matrix products for Legendre transforms
!!
!!@verbatim
!!
!!@endverbatim
!!
!
      module matmul_tests_4_smp
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
      subroutine simplest_add_matmul(n, m, l, a, b, coef, c)
!
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: i, j, k
!
!
      do j = 1, m
        do i = 1, n
          c(i,j) = coef * c(i,j)
          do k = 1, l
            c(i,j) = c(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do
!
      end subroutine simplest_add_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine simple_add_matmul_smp(np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: i, j, k
      integer(kind= kint) :: kst, knum, kp
!
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      c(1:n,1:m) = coef * c(1:n,1:m)
!
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do i = 1, n
            do k = 1, knum
              c(i,j) = c(i,j) + a(i,k+kst) * b(k+kst,j)
            end do
          end do
        end do
      end do
!
      deallocate(lstack_smp)
!
      end subroutine simple_add_matmul_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_matmul_by_c_smp(np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: i, j, k
      integer(kind= kint) :: kst, knum, kp
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
      real(kind = kreal), allocatable :: c0(:,:,:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      allocate(c0(n,m,np_smp))
!
      c0(1:n,1:m,1:np_smp) = 0.0d0
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do i = 1, n
            do k = 1, knum
              c0(i,j,kp) = c0(i,j,kp) + a(i,k+kst) * b(k+kst,j)
            end do
          end do
        end do
      end do
!
      c(1:n,1:m) = coef * c(1:n,1:m)
      do kp = 1, np_smp
        c(1:n,1:m) = c(1:n,1:m) + c0(1:n,1:m,kp)
      end do
!
      deallocate(c0)
      deallocate(lstack_smp)
!
      end subroutine add_matmul_by_c_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_matmul_by_tmp_smp(np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: i, j, k
      integer(kind= kint) :: kst, knum, kp
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
      real(kind = kreal), allocatable :: a0(:,:,:)
      real(kind = kreal), allocatable :: b0(:,:,:)
      real(kind = kreal), allocatable :: c0(:,:,:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      allocate(a0(n,l,np_smp))
      allocate(c0(l,m,np_smp))
      allocate(c0(n,m,np_smp))
!
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do k = 1, knum
          do i = 1, n
            a0(i,k,kp) =  a(i,k+kst)
          end do
        end do
      end do
!
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do k = 1, knum
            b0(k,j,kp) = b(k+kst,j)
          end do
        end do
      end do
!
!
      c0(1:n,1:m,1:np_smp) = 0.0d0
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do i = 1, n
            do k = 1, knum
              c0(i,j,kp) = c0(i,j,kp) + a0(i,k,kp) * b0(k,j,kp)
            end do
          end do
        end do
      end do
!
      c(1:n,1:m) = coef * c(1:n,1:m)
      do kp = 1, np_smp
        c(1:n,1:m) = c(1:n,1:m) + c0(1:n,1:m,kp)
      end do
!
      deallocate(a0,b0,c0)
      deallocate(lstack_smp)
!
      end subroutine add_matmul_by_tmp_smp
!
!-----------------------------------------------------------------------
!
      subroutine add_matmul_by_tmp_smp2(np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: i, j, k
      integer(kind= kint) :: kst, knum, kp
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
      real(kind = kreal), allocatable :: a0(:,:,:)
      real(kind = kreal), allocatable :: b0(:,:,:)
      real(kind = kreal), allocatable :: c0(:,:,:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      allocate(a0(n,l0,np_smp))
      allocate(b0(l0,m,np_smp))
      allocate(c0(n,m,np_smp))
!
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do k = 1, knum
          a0(1:n,k,kp) =  a(1:n,k+kst)
        end do
        do k =  knum+1, l0
          a0(1:n,k,kp) = 0.0d0
        end do
      end do
!
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do k = 1, knum
            b0(k,j,kp) = b(k+kst,j)
          end do
          do k =  knum+1, l0
            b0(k,j,kp) = 0.0d0
          end do
        end do
      end do
!
!
      c0(1:n,1:m,1:np_smp) = 0.0d0
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do i = 1, n
            do k = 1, knum
              c0(i,j,kp) = c0(i,j,kp) + a0(i,k,kp) * b0(k,j,kp)
            end do
          end do
        end do
      end do
!
      c(1:n,1:m) = coef * c(1:n,1:m)
      do kp = 1, np_smp
        c(1:n,1:m) = c(1:n,1:m) + c0(1:n,1:m,kp)
      end do
!
      deallocate(a0,b0,c0)
      deallocate(lstack_smp)
!
      end subroutine add_matmul_by_tmp_smp2
!
!-----------------------------------------------------------------------
!
      subroutine sel_add_matmul_by_smp                                  &
     &         (iflag_matmul, np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
      use matmul_for_legendre_trans
!
      integer(kind= kint), intent(in) :: iflag_matmul
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: j, k
      integer(kind= kint) :: kst, knum, kp
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
      real(kind = kreal), allocatable :: a0(:,:,:)
      real(kind = kreal), allocatable :: b0(:,:,:)
      real(kind = kreal), allocatable :: c0(:,:,:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      allocate(a0(n,l0,np_smp))
      allocate(b0(l0,m,np_smp))
      allocate(c0(n,m,np_smp))
!
!$omp parallel do private(kp,kst,knum,k)
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do k = 1, knum
          a0(1:n,k,kp) =  a(1:n,k+kst)
        end do
        do k =  knum+1, l0
          a0(1:n,k,kp) = 0.0d0
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(kp,kst,knum,j,k)
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do k = 1, knum
            b0(k,j,kp) = b(k+kst,j)
          end do
        end do
!
        do k =  knum+1, l0
          do j = 1, m
            b0(k,j,kp) = 0.0d0
          end do
        end do
      end do
!$omp end parallel do
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          c0(1:n,1:m,kp) = matmul(a0(1:n,1:l0,kp), b0(1:l0,1:m,kp))
        end do
!$omp end parallel do
      end if
!
      if(iflag_matmul .eq. iflag_DGEMM) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          c0(1:n,1:m,kp) = 0.0d0
          call DGEMM('N', 'N', int(n), int(m), int(l0), one,            &
     &        a0(1,1,kp), int(n), b0(1,1,kp), int(l0), one,             &
     &        c0(1,1,kp), int(n))
        end do
!$omp end parallel do
      end if
!
      if(iflag_matmul .eq. iflag_MATPROD) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          call matmat_leg_trans                                         &
     &       (n, m, l0, a0(1,1,kp), b0(1,1,kp), c0(1,1,kp))
        end do
!$omp end parallel do
      end if
!
!
!$omp parallel
!$omp workshare
      c(1:n,1:m) = coef * c(1:n,1:m)
!$omp end workshare nowait
!
      do kp = 1, np_smp
!$omp workshare
        c(1:n,1:m) = c(1:n,1:m) + c0(1:n,1:m,kp)
!$omp end workshare nowait
      end do
!$omp end parallel
!
!
      deallocate(a0,b0,c0)
      deallocate(lstack_smp)
!
      end subroutine sel_add_matmul_by_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_add_matmul_by_smp2                                 &
     &         (iflag_matmul, np_smp, n, m, l, a, b, coef, c)
!
      use cal_minmax_and_stacks
      use matmul_for_legendre_trans
!
      integer(kind= kint), intent(in) :: iflag_matmul
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: l, m, n
      real(kind = kreal), intent(in) :: a(n,l), b(l,m)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: l0
      integer(kind= kint), allocatable :: lstack_smp(:)
      real(kind = kreal), allocatable :: a0(:,:,:)
      real(kind = kreal), allocatable :: b0(:,:,:)
      real(kind = kreal), allocatable :: c0(:,:,:)
!
!
      allocate(lstack_smp(0:np_smp))
      call count_number_4_smp(np_smp, ione, l, lstack_smp, l0)
!
      allocate(a0(n,l0,np_smp))
      allocate(b0(l0,m,np_smp))
      allocate(c0(n,m,np_smp))
!
      call init_a0_matmul_4_smp(np_smp, lstack_smp, l0, n, l, a, a0)
      call init_b0_matmul_4_smp(np_smp, lstack_smp, l0, m, l, b, b0)
!
      call sel_add_matmul_smp(iflag_matmul, np_smp,                     &
     &    n, m, l0, a0, b0, coef, c0, c)
!
      deallocate(a0,b0,c0)
      deallocate(lstack_smp)
!
      end subroutine sel_add_matmul_by_smp2
!
!-----------------------------------------------------------------------
!
      subroutine init_a0_matmul_4_smp                                   &
     &         (np_smp, lstack_smp, l0, n, l, a, a0)
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: lstack_smp(0:np_smp)
      integer(kind= kint), intent(in) :: l0
      integer(kind= kint), intent(in) :: l, n
      real(kind = kreal), intent(in) :: a(n,l)
      real(kind = kreal), intent(inout) :: a0(n,l0,np_smp)
!
      integer(kind= kint) :: k
      integer(kind= kint) :: kst, knum, kp
!
!
!$omp parallel do private(kp,kst,knum,k)
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do k = 1, knum
          a0(1:n,k,kp) =  a(1:n,k+kst)
        end do
        do k =  knum+1, l0
          a0(1:n,k,kp) = 0.0d0
        end do
      end do
!$omp end parallel do
!
      end subroutine init_a0_matmul_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine init_b0_matmul_4_smp                                   &
     &         (np_smp, lstack_smp, l0, m, l, b, b0)
!
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: lstack_smp(0:np_smp)
      integer(kind= kint), intent(in) :: l0
      integer(kind= kint), intent(in) :: l, m
      real(kind = kreal), intent(in) ::  b(l,m)
      real(kind = kreal), intent(inout) :: b0(l0,m,np_smp)
!
      integer(kind= kint) :: j, k
      integer(kind= kint) :: kst, knum, kp
!
!
!$omp parallel do private(kp,kst,knum,j,k)
      do kp = 1, np_smp
        kst  = lstack_smp(kp-1)
        knum = lstack_smp(kp) - lstack_smp(kp-1)
        do j = 1, m
          do k = 1, knum
            b0(k,j,kp) = b(k+kst,j)
          end do
        end do
!
        do k =  knum+1, l0
          do j = 1, m
            b0(k,j,kp) = 0.0d0
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine init_b0_matmul_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine sel_add_matmul_smp(iflag_matmul, np_smp,               &
     &          n, m, l0, a0, b0, coef, c0, c)
!
      use cal_minmax_and_stacks
      use matmul_for_legendre_trans
!
      integer(kind= kint), intent(in) :: iflag_matmul
      integer(kind= kint), intent(in) :: np_smp
      integer(kind= kint), intent(in) :: m, n, l0
      real(kind = kreal), intent(in) :: a0(n,l0,np_smp)
      real(kind = kreal), intent(in) :: b0(l0,m,np_smp)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: c0(n,m,np_smp)
      real(kind = kreal), intent(inout) :: c(n,m)
!
      integer(kind= kint) :: kp
!
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          c0(1:n,1:m,kp) = matmul(a0(1:n,1:l0,kp), b0(1:l0,1:m,kp))
        end do
!$omp end parallel do
      end if
!
      if(iflag_matmul .eq. iflag_DGEMM) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          c0(1:n,1:m,kp) = 0.0d0
          call DGEMM('N', 'N', int(n), int(m), int(l0), one,            &
     &        a0(1,1,kp), int(n), b0(1,1,kp), int(l0), one,             &
     &        c0(1,1,kp), int(n))
        end do
!$omp end parallel do
      end if
!
      if(iflag_matmul .eq. iflag_MATPROD) then
!$omp parallel do private(kp)
        do kp = 1, np_smp
          call matmat_leg_trans                                         &
     &       (n, m, l0, a0(1,1,kp), b0(1,1,kp), c0(1,1,kp))
        end do
!$omp end parallel do
      end if
!
!
!$omp parallel
!$omp workshare
      c(1:n,1:m) = coef * c(1:n,1:m)
!$omp end workshare nowait
!
      do kp = 1, np_smp
!$omp workshare
        c(1:n,1:m) = c(1:n,1:m) + c0(1:n,1:m,kp)
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine sel_add_matmul_smp
!
!-----------------------------------------------------------------------
!
      end module matmul_tests_4_smp
