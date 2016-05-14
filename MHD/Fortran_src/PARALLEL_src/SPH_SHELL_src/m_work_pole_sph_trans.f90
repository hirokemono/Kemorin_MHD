!
!      module m_work_pole_sph_trans
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine allocate_num_pole_sph_trans(nri_global_rtp)
!!      subroutine deallocate_num_pole_sph_trans
!!
!
      module m_work_pole_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
      integer(kind = kint) :: nnod_pole
!
      integer(kind = kint), allocatable :: istack_npole_smp(:)
!
      integer(kind = kint) :: max_npole_smp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_num_pole_sph_trans(nri_global_rtp)
!
      integer(kind = kint), intent(in) :: nri_global_rtp
!
!
      nnod_pole = 2*nri_global_rtp + 1
      allocate(istack_npole_smp(0:np_smp) )
      istack_npole_smp = 0
!
      end subroutine allocate_num_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_num_pole_sph_trans
!
!
      deallocate(istack_npole_smp)
!
      end subroutine deallocate_num_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      end module m_work_pole_sph_trans
