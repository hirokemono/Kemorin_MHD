!cal_subtract_smp.f90
!     module cal_subtract_smp
!
!      Written by H. Matsui
!
!      subroutine subtract_scalars_smp(np_smp, nnod, inod_smp_stack,    &
!     &          source, subt, sol)
!      subroutine subtract_vectors_smp(np_smp, nnod, inod_smp_stack,    &
!     &          source, subt, sol)
!      subroutine subtract_tensors_smp(np_smp, nnod, inod_smp_stack,    &
!     &          source, subt, sol)
!
!         sol(inod,:) =  source(inod,:) - subt(inod,i_v2)
!
      module cal_subtract_smp
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine subtract_scalars_smp(np_smp, nnod, inod_smp_stack,     &
     &          source, subt, sol)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: source(nnod),  subt(nnod)
      real (kind=kreal), intent(inout) :: sol(nnod)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          sol(inod) = source(inod) - subt(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine subtract_scalars_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_vectors_smp(np_smp, nnod, inod_smp_stack,     &
     &          source, subt, sol)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: source(nnod,3),  subt(nnod,3)
      real (kind=kreal), intent(inout) :: sol(nnod,3)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          sol(inod,1) = source(inod,1) - subt(inod,1)
          sol(inod,2) = source(inod,2) - subt(inod,2)
          sol(inod,3) = source(inod,3) - subt(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine subtract_vectors_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_tensors_smp(np_smp, nnod, inod_smp_stack,     &
     &          source, subt, sol)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: source(nnod,6),  subt(nnod,6)
      real (kind=kreal), intent(inout) :: sol(nnod,6)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          sol(inod,1) = source(inod,1) - subt(inod,1)
          sol(inod,2) = source(inod,2) - subt(inod,2)
          sol(inod,3) = source(inod,3) - subt(inod,3)
          sol(inod,4) = source(inod,4) - subt(inod,4)
          sol(inod,5) = source(inod,5) - subt(inod,5)
          sol(inod,6) = source(inod,6) - subt(inod,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine subtract_tensors_smp
!
!-----------------------------------------------------------------------
!
      end module cal_subtract_smp
