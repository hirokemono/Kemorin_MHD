!>@file   cal_minmax_4_vector_smp.f90
!!        module cal_minmax_4_vector_smp
!!
!!@date  Programmed by H.Matsui on Aug., 2006
!!
!>@brief Find minimum and maximum values
!!
!!@verbatim
!!      subroutine s_cal_minmax_4_vector_smp(nnod, np_smp, istack_smp,  &
!!     &          min_vec, max_vec, vect)
!!      subroutine s_cal_minmax_4_scalar_smp(nnod, np_smp, istack_smp,  &
!!     &          min_vec, max_vec, scalar)
!!
!!      subroutine cal_minmax_4_vector(nnod, min_vec, max_vec, vect)
!!      subroutine cal_minmax_4_scalar(nnod, min_vec, max_vec, scalar)
!!@endverbatim
!!
!!@n @param nnod                   Number of data points
!!@n @param np_smp                 Number of SMP processes
!!@n @param istack_smp(0:np_smp)   End point for each SMP process
!!@n @param min_vec(3)             Minimum value for each component
!!@n @param max_vec(3)             Maximum value for each component
!!@n @param vect(nnod,3)           Vector data
!!@n @param scalar(nnod)           Scalar data
!
      module   cal_minmax_4_vector_smp
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: max_vec_smp(:,:)
      real(kind = kreal), allocatable :: min_vec_smp(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_minmax_4_vector_smp(nnod, np_smp, istack_smp,    &
     &          min_vec, max_vec, vect)
!
      integer(kind = kint), intent(in) :: np_smp, nnod
      integer(kind = kint), intent(in) :: istack_smp(0:np_smp)
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      real(kind = kreal), intent(inout) :: min_vec(3), max_vec(3)
!
      integer(kind = kint) :: ip, ist, ied, inum
!
!
      allocate(max_vec_smp(np_smp,3))
      allocate(min_vec_smp(np_smp,3))
      max_vec_smp = 0.0d0
      min_vec_smp = 0.0d0
!
!$omp parallel do private (ist,ied,inum)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        max_vec_smp(ip,1) = vect(ist,1)
        max_vec_smp(ip,2) = vect(ist,2)
        max_vec_smp(ip,3) = vect(ist,3)
        min_vec_smp(ip,1) = vect(ist,1)
        min_vec_smp(ip,2) = vect(ist,2)
        min_vec_smp(ip,3) = vect(ist,3)
        do inum = ist+1, ied
          max_vec_smp(ip,1) = max(max_vec_smp(ip,1),vect(inum,1))
          max_vec_smp(ip,2) = max(max_vec_smp(ip,2),vect(inum,2))
          max_vec_smp(ip,3) = max(max_vec_smp(ip,3),vect(inum,3))
          min_vec_smp(ip,1) = min(min_vec_smp(ip,1),vect(inum,1))
          min_vec_smp(ip,2) = min(min_vec_smp(ip,2),vect(inum,2))
          min_vec_smp(ip,3) = min(min_vec_smp(ip,3),vect(inum,3))
        end do
      end do
!$omp end parallel do
!
      max_vec(1) = max_vec_smp(1,1)
      max_vec(2) = max_vec_smp(1,2)
      max_vec(3) = max_vec_smp(1,3)
      min_vec(1) = min_vec_smp(1,1)
      min_vec(2) = min_vec_smp(1,2)
      min_vec(3) = min_vec_smp(1,3)
      do ip = 2, np_smp
        max_vec(1) = max(max_vec(1),max_vec_smp(ip,1))
        max_vec(2) = max(max_vec(2),max_vec_smp(ip,2))
        max_vec(3) = max(max_vec(3),max_vec_smp(ip,3))
        min_vec(1) = max(min_vec(1),min_vec_smp(ip,1))
        min_vec(2) = max(min_vec(2),min_vec_smp(ip,2))
        min_vec(3) = max(min_vec(3),min_vec_smp(ip,3))
      end do
!
      deallocate(max_vec_smp)
      deallocate(min_vec_smp)
!
      end subroutine s_cal_minmax_4_vector_smp
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_minmax_4_scalar_smp(nnod, np_smp, istack_smp,    &
     &          min_vec, max_vec, scalar)
!
      integer(kind = kint), intent(in) :: np_smp, nnod
      integer(kind = kint), intent(in) :: istack_smp(0:np_smp)
      real(kind = kreal), intent(in) :: scalar(nnod)
!
      real(kind = kreal), intent(inout) :: min_vec, max_vec
!
      integer(kind = kint) :: ip, ist, ied, inum
!
!
      allocate(max_vec_smp(np_smp,1))
      allocate(min_vec_smp(np_smp,1))
      max_vec_smp = 0.0d0
      min_vec_smp = 0.0d0
!
!$omp parallel do private (ist,ied,inum)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        max_vec_smp(ip,1) = scalar(ist)
        min_vec_smp(ip,1) = scalar(ist)
        do inum = ist+1, ied
          max_vec_smp(ip,1) = max(max_vec_smp(ip,1),scalar(inum))
          min_vec_smp(ip,1) = min(min_vec_smp(ip,1),scalar(inum))
        end do
      end do
!$omp end parallel do
!
      max_vec = max_vec_smp(1,1)
      min_vec = min_vec_smp(1,1)
      do ip = 2, np_smp
        max_vec = max(max_vec,max_vec_smp(ip,1))
        min_vec = max(min_vec,min_vec_smp(ip,1))
      end do
!
      deallocate(max_vec_smp)
      deallocate(min_vec_smp)
!
      end subroutine s_cal_minmax_4_scalar_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_4_vector(nnod, min_vec, max_vec, vect)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      real(kind = kreal), intent(inout) :: min_vec(3), max_vec(3)
!
      integer(kind = kint) :: istack_smp(0:1)
!
!
      istack_smp(0) = izero
      istack_smp(1) = nnod
      call s_cal_minmax_4_vector_smp(nnod, ione, istack_smp,            &
     &    min_vec, max_vec, vect)
!
      end subroutine cal_minmax_4_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_4_scalar(nnod, min_vec, max_vec, scalar)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: scalar(nnod)
!
      real(kind = kreal), intent(inout) :: min_vec, max_vec
!
      integer(kind = kint) :: istack_smp(0:1)
!
!
      istack_smp(0) = izero
      istack_smp(1) = nnod
      call s_cal_minmax_4_scalar_smp(nnod, ione, istack_smp,            &
     &    min_vec, max_vec, scalar)
!
      end subroutine cal_minmax_4_scalar
!
!------------------------------------------------------------------
!
      end module cal_minmax_4_vector_smp
