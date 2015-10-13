!
!     module sum_volume_of_domain
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!
!!       subroutine allocate_volume_4_smp
!!       subroutine deallocate_volume_4_smp
!!
!!      subroutine sum_4_volume(numele, interior_ele, iele_fsmp_stack,  &
!!     &          volume_ele, vol_local)
!!       subroutine sum_of_volume_by_ele_table(numele, interior_ele,    &
!!     &           volume_ele, numele_field, iele_fsmp_stack,           &
!!     &           iele_field, vol_local)
!
      module sum_volume_of_domain
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      real(kind=kreal) :: vol_local
      real(kind=kreal), allocatable :: xvol_smp(:)
      private :: xvol_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_volume_4_smp
!
       allocate ( xvol_smp(np_smp) )
       xvol_smp = 0.0d0
!
       end subroutine allocate_volume_4_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_volume_4_smp
!
       deallocate ( xvol_smp )
!
       end subroutine deallocate_volume_4_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sum_4_volume(numele, interior_ele, iele_fsmp_stack,    &
     &          volume_ele, vol_local)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: volume_ele(numele)
!
      real (kind=kreal), intent(inout) :: vol_local
!
      integer (kind=kint) :: iproc, iele
      integer (kind=kint) :: istart, iend
!
!
      vol_local = 0.0d0
      xvol_smp = 0.0d0
!
!$omp parallel do private(iele,istart,iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
        do iele = istart, iend
!
          xvol_smp(iproc) = xvol_smp(iproc)                             &
     &                     + volume_ele(iele)*dble(interior_ele(iele))
!
        end do
      end do
!$omp end parallel do
!
!cdir noconcur
      do iproc = 1, np_smp
        vol_local = vol_local + xvol_smp(iproc)
      end do
!
      end subroutine sum_4_volume
!
!-----------------------------------------------------------------------
!
       subroutine sum_of_volume_by_ele_table(numele, interior_ele,      &
     &           volume_ele, numele_field, iele_fsmp_stack,             &
     &           iele_field, vol_local)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
      real (kind=kreal), intent(in) :: volume_ele(numele)
!
      integer (kind=kint), intent(in) :: numele_field
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: iele_field(numele_field)
!
      real (kind=kreal), intent(inout) :: vol_local
!
      integer (kind=kint) :: iproc, inum, iele
      integer (kind=kint) :: istart, iend
!
!
      vol_local = 0.0d0
      xvol_smp = 0.0d0
!
!$omp parallel do private(inum,iele,istart,iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
        do inum = istart, iend
!
          iele = iele_field(inum)
          xvol_smp(iproc) = xvol_smp(iproc)                             &
     &                      + volume_ele(iele)*dble(interior_ele(iele))
!
        end do
       end do
!$omp end parallel do
!
!poption noparallel
!cdir noconcur
      do iproc = 1, np_smp
        vol_local = vol_local + xvol_smp(iproc)
      end do
!
      end subroutine sum_of_volume_by_ele_table
!
!-----------------------------------------------------------------------
!
      end module sum_volume_of_domain
