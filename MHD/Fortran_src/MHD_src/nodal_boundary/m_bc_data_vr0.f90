!
!     module m_bc_data_vr0
!.......................................................................
!
!      Written by H. Matsui
!
      module m_bc_data_vr0
!
      use m_precision
!
      implicit  none
!
! 
      integer (kind=kint), allocatable :: ibc_velo_r0(:)
      integer (kind=kint), allocatable :: ibc2_velo_r0(:)
!
      integer (kind=kint) :: num_bc_vr0_nod
      integer (kind=kint), allocatable :: ibc_vr0_id(:)
      real (kind=kreal)  , allocatable :: bc_vr0_id_apt(:)
!
      integer (kind=kint) :: num_index_ibc_vr0
      integer (kind=kint) :: num_index_ibc2_vr0
      integer (kind=kint), allocatable :: ele_bc_vr0_id(:)
      integer (kind=kint), allocatable :: nod_bc_vr0_id(:)
      integer (kind=kint), allocatable :: ele_bc2_vr0_id(:)
      integer (kind=kint), allocatable :: nod_bc2_vr0_id(:)
!
      integer (kind=kint) :: ibc_vr0_end
      integer (kind=kint), allocatable :: ibc_vr0_shape(:)
      integer (kind=kint), allocatable :: ibc_vr0_stack(:)
      integer (kind=kint), allocatable :: ibc_vr0_stack_smp(:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vr0
!
       use m_geometry_parameter
!
       allocate(ibc_velo_r0(numnod))
       allocate(ibc2_velo_r0(numnod))
!
       ibc_velo_r0=0
       ibc2_velo_r0=0
! 
       allocate(ibc_vr0_id(num_bc_vr0_nod))
       allocate(bc_vr0_id_apt(num_bc_vr0_nod))
       if (num_bc_vr0_nod/=0) then
        ibc_vr0_id=0 
        bc_vr0_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_vr0
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vr0_4_ele
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_vr0_id(num_index_ibc_vr0) )
        allocate ( nod_bc_vr0_id(num_index_ibc_vr0) )
!
        allocate ( ele_bc2_vr0_id(num_index_ibc2_vr0) )
        allocate ( nod_bc2_vr0_id(num_index_ibc2_vr0) )
!
        allocate ( ibc_vr0_stack(0:nnod_4_ele) )
        allocate ( ibc_vr0_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_vr0_shape(nnod_4_ele) )
!
        if ( num_index_ibc2_vr0.gt.0) then
         ele_bc_vr0_id = 0
         nod_bc_vr0_id = 0
         ibc_vr0_stack = 0
         ibc_vr0_shape = 0
         ibc_vr0_stack_smp = 0
        end if
!
        if ( num_index_ibc2_vr0.gt.0) then
         ele_bc2_vr0_id = 0
         nod_bc2_vr0_id = 0
        end if
!
       end subroutine allocate_bc_vr0_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_vr0
!
        deallocate( ibc_velo_r0 )
        deallocate( ibc2_velo_r0 )
!
       end subroutine deallocate_ibc_4_vr0
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_vr0
!
        deallocate ( ele_bc2_vr0_id )
        deallocate ( nod_bc2_vr0_id )
!
       end subroutine deallocate_bc2_vr0
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_vr0
