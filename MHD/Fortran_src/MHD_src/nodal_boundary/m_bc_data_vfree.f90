!
!     module m_bc_data_vfree
!.......................................................................
!
!      Written by H. Matsui
!
      module m_bc_data_vfree
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_velo_fr(:)
      integer (kind=kint), allocatable :: ibc2_velo_fr(:)
!
      integer (kind=kint) :: num_bc_fr_nod
      integer (kind=kint), allocatable :: ibc_fr_id(:)
      real (kind=kreal)  , allocatable :: bc_fr_id_apt(:)
!
      integer (kind=kint) :: num_index_ibc_vfr
      integer (kind=kint) :: num_index_ibc2_vfr
      integer (kind=kint), allocatable :: ele_bc_vfr_id(:)
      integer (kind=kint), allocatable :: nod_bc_vfr_id(:)
      integer (kind=kint), allocatable :: ele_bc2_vfr_id(:)
      integer (kind=kint), allocatable :: nod_bc2_vfr_id(:)
!
      integer (kind=kint) :: ibc_vfr_end
      integer (kind=kint), allocatable :: ibc_vfr_shape(:)
      integer (kind=kint), allocatable :: ibc_vfr_stack(:)
      integer (kind=kint), allocatable :: ibc_vfr_stack_smp(:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vfr
!
       use m_geometry_parameter
!
       allocate(ibc_velo_fr(numnod))
       allocate(ibc2_velo_fr(numnod))
!
       ibc_velo_fr=0
       ibc2_velo_fr=0
! 
       allocate(ibc_fr_id(num_bc_fr_nod))
       allocate(bc_fr_id_apt(num_bc_fr_nod))
       if (num_bc_fr_nod/=0) then
        ibc_fr_id=0
        bc_fr_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_vfr
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vfr_4_ele
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_vfr_id(num_index_ibc_vfr) )
        allocate ( nod_bc_vfr_id(num_index_ibc_vfr) )
!
        allocate ( ele_bc2_vfr_id(num_index_ibc2_vfr) )
        allocate ( nod_bc2_vfr_id(num_index_ibc2_vfr) )
!
        allocate ( ibc_vfr_stack(0:nnod_4_ele) )
        allocate ( ibc_vfr_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_vfr_shape(nnod_4_ele) )
!
        if ( num_index_ibc_vfr.gt.0) then
         ele_bc_vfr_id = 0
         nod_bc_vfr_id = 0
         ibc_vfr_stack = 0
         ibc_vfr_shape = 0
         ibc_vfr_stack_smp = 0
        end if
!
        if ( num_index_ibc2_vfr.gt.0) then
         ele_bc2_vfr_id = 0
         nod_bc2_vfr_id = 0
        end if
!
       end subroutine allocate_bc_vfr_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_vfr
!
        deallocate( ibc_velo_fr )
        deallocate( ibc2_velo_fr )
!
       end subroutine deallocate_ibc_4_vfr
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_vfr
!
        deallocate ( ele_bc2_vfr_id )
        deallocate ( nod_bc2_vfr_id )
!
       end subroutine deallocate_bc2_vfr
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_vfree
