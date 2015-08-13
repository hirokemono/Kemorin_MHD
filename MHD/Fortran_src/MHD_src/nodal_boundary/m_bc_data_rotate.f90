!
!     module m_bc_data_rotate
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_bc_rot(numnod)
!       subroutine allocate_bc_rot_4_ele(nnod_4_ele)
!       subroutine deallocate_ibc_4_rot
!       subroutine deallocate_bc2_rot
!
      module m_bc_data_rotate
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: ibc_velo_rot(:)
      integer (kind=kint), allocatable :: ibc2_velo_rot(:)
!
      integer (kind=kint) :: num_bc_v10_nod
      integer (kind=kint), allocatable :: ibc_v10_id(:)
      real (kind=kreal)  , allocatable :: bc_v10_id_apt(:,:)
!
      integer (kind=kint) :: num_index_ibc_vrot
      integer (kind=kint) :: num_index_ibc2_vrot
      integer (kind=kint), allocatable :: ele_bc_vrot_id(:)
      integer (kind=kint), allocatable :: nod_bc_vrot_id(:)
      integer (kind=kint), allocatable :: ele_bc2_vrot_id(:)
      integer (kind=kint), allocatable :: nod_bc2_vrot_id(:)
!
      integer (kind=kint) :: ibc_vrot_end
      integer (kind=kint), allocatable :: ibc_vrot_shape(:)
      integer (kind=kint), allocatable :: ibc_vrot_stack(:)
      integer (kind=kint), allocatable :: ibc_vrot_stack_smp(:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_rot(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_velo_rot(numnod))
       allocate(ibc2_velo_rot(numnod))
!
       if(numnod .gt. 0) then
         ibc_velo_rot=0
         ibc2_velo_rot=0
       end if
! 
       allocate(ibc_v10_id(num_bc_v10_nod))
       allocate(bc_v10_id_apt(num_bc_v10_nod,3))
       if (num_bc_v10_nod .gt. 0) then
         ibc_v10_id=0
         bc_v10_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_rot
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_rot_4_ele(nnod_4_ele)
!
        use m_machine_parameter
!
       integer(kind = kint), intent(in) :: nnod_4_ele
!
!
        allocate ( ele_bc_vrot_id(num_index_ibc_vrot) )
        allocate ( nod_bc_vrot_id(num_index_ibc_vrot) )
!
        allocate ( ele_bc2_vrot_id(num_index_ibc2_vrot) )
        allocate ( nod_bc2_vrot_id(num_index_ibc2_vrot) )
!
        allocate ( ibc_vrot_stack(0:nnod_4_ele) )
        allocate ( ibc_vrot_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_vrot_shape(nnod_4_ele) )
!
        if ( num_index_ibc_vrot.gt.0) then
         ele_bc_vrot_id = 0
         nod_bc_vrot_id = 0
         ibc_vrot_stack = 0
         ibc_vrot_shape = 0
         ibc_vrot_stack_smp = 0
        end if
!
        if ( num_index_ibc2_vrot.gt.0) then
         ele_bc2_vrot_id = 0
         nod_bc2_vrot_id = 0
        end if
!
       end subroutine allocate_bc_rot_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_rot
!
        deallocate( ibc_velo_rot )
        deallocate( ibc2_velo_rot )
!
       end subroutine deallocate_ibc_4_rot
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_rot
!
        deallocate ( ele_bc2_vrot_id )
        deallocate ( nod_bc2_vrot_id )
!
       end subroutine deallocate_bc2_rot
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_rotate
