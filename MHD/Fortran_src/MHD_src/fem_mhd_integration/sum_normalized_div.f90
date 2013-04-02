!
!      module sum_normalized_div
!
!      Written by H. Matsui
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine sum_norm_of_div(numele, np_smp, iele_fsmp_stack,      &
!     &          e_multi, sk1, bulk_d)
!
      module sum_normalized_div
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
      subroutine sum_norm_of_div(numele, np_smp, iele_fsmp_stack,       &
     &          e_multi, sk1, bulk_d)
!
      integer (kind=kint), intent(in) :: numele, np_smp
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: e_multi(numele)
      real (kind=kreal), intent(in) :: sk1(numele)
      real (kind=kreal), intent(inout) :: bulk_d(1)
!
      real (kind=kreal) :: bulk_d_smp(np_smp)
!
      integer(kind=kint) :: iproc, iele, istart, iend
!
!
      bulk_d_smp = 0.0d0
!
!$omp parallel do private(iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1) +1
        iend = iele_fsmp_stack(iproc)
!cdir nodep
!voption, indep, vec
         do iele = istart, iend
           bulk_d_smp(iproc) = bulk_d_smp(iproc) + sk1(iele)            &
     &                        * e_multi(iele)
         end do
       end do
!$omp end parallel do
!
       bulk_d(1) = bulk_d_smp(1)
       do iproc = 2, np_smp
         bulk_d(1) = bulk_d(1) + bulk_d_smp(iproc)
       end do
!
      end subroutine sum_norm_of_div
!
! ----------------------------------------------------------------------
!
      end module sum_normalized_div
