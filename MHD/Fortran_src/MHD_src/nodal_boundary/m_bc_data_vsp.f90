!
!     module m_bc_data_vsp
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_bc_vsp(numnod)
!
      module m_bc_data_vsp
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: ibc_velo_vsp(:)
      integer (kind=kint), allocatable :: ibc2_velo_vsp(:)
!
      integer (kind=kint) :: num_bc_vsp_nod
      integer (kind=kint), allocatable :: ibc_vsp_id(:)
      real (kind=kreal)  , allocatable :: bc_vsp_id_apt(:)
!
      integer (kind=kint) :: num_index_ibc_vsp
      integer (kind=kint) :: num_index_ibc2_vsp
      integer (kind=kint), allocatable :: ele_bc_vsp_id(:)
      integer (kind=kint), allocatable :: nod_bc_vsp_id(:)
      integer (kind=kint), allocatable :: ele_bc2_vsp_id(:)
      integer (kind=kint), allocatable :: nod_bc2_vsp_id(:)
!
      integer (kind=kint) :: ibc_vsp_end
      integer (kind=kint), allocatable :: ibc_vsp_shape(:)
      integer (kind=kint), allocatable :: ibc_vsp_stack(:)
      integer (kind=kint), allocatable :: ibc_vsp_stack_smp(:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vsp(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_velo_vsp(numnod))
       allocate(ibc2_velo_vsp(numnod))
!
       if(numnod .gt. 0) then
         ibc_velo_vsp = 0
         ibc2_velo_vsp = 0
       end if
! 
       allocate(ibc_vsp_id(num_bc_vsp_nod))
       allocate(bc_vsp_id_apt(num_bc_vsp_nod))
       if (num_bc_vsp_nod .gt. 0) then
        ibc_vsp_id=0
        bc_vsp_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_vsp
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vsp_4_ele
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_vsp_id(num_index_ibc_vsp) )
        allocate ( nod_bc_vsp_id(num_index_ibc_vsp) )
!
        allocate ( ele_bc2_vsp_id(num_index_ibc2_vsp) )
        allocate ( nod_bc2_vsp_id(num_index_ibc2_vsp) )
!
        allocate ( ibc_vsp_stack(0:nnod_4_ele) )
        allocate ( ibc_vsp_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_vsp_shape(nnod_4_ele) )
!
        if ( num_index_ibc_vsp.gt.0) then
         ele_bc_vsp_id = 0
         nod_bc_vsp_id = 0
         ibc_vsp_stack = 0
         ibc_vsp_shape = 0
         ibc_vsp_stack_smp = 0
        end if
!
        if ( num_index_ibc2_vsp.gt.0) then
         ele_bc2_vsp_id = 0
         nod_bc2_vsp_id = 0
        end if
!
       end subroutine allocate_bc_vsp_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_vsp
!
        deallocate( ibc_velo_vsp )
        deallocate( ibc2_velo_vsp )
!
       end subroutine deallocate_ibc_4_vsp
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_vsp
!
        deallocate ( ele_bc2_vsp_id )
        deallocate ( nod_bc2_vsp_id )
!
       end subroutine deallocate_bc2_vsp
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_vsp
