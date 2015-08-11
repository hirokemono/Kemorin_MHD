!
!     module m_bc_temp_sgs
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!       subroutine allocate_bc_t_sgs(numnod)
!
      module m_bc_temp_sgs
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_t_sgs(:)
      integer (kind=kint), allocatable :: ibc2_t_sgs(:)
! 
!
      integer (kind=kint) :: num_bc_t_sgs_nod
      integer (kind=kint), allocatable :: ibc_t_sgs_id(:)
      real (kind=kreal)  , allocatable :: bc_t_sgs_id_apt(:)
! 
!
      integer (kind=kint) :: num_index_ibc_t_sgs
      integer (kind=kint) :: num_index_ibc2_t_sgs
      integer (kind=kint), allocatable :: ele_bc_t_sgs_id(:)
      integer (kind=kint), allocatable :: nod_bc_t_sgs_id(:)
      integer (kind=kint), allocatable :: ele_bc2_t_sgs_id(:)
      integer (kind=kint), allocatable :: nod_bc2_t_sgs_id(:)
! 
!
      integer (kind=kint) :: ibc_t_sgs_end
      integer (kind=kint), allocatable :: ibc_t_sgs_shape(:)
      integer (kind=kint), allocatable :: ibc_t_sgs_stack(:)
      integer (kind=kint), allocatable :: ibc_t_sgs_stack_smp(:)
! 
!  ---------------------------------------------------------------------
!
      contains 
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_bc_t_sgs(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_t_sgs(numnod))
       allocate(ibc2_t_sgs(numnod))
!
       if(numnod .gt. 0) then
         ibc_t_sgs=0
         ibc2_t_sgs=0
       end if
! 
       allocate(ibc_t_sgs_id(num_bc_t_sgs_nod))
       allocate(bc_t_sgs_id_apt(num_bc_t_sgs_nod))
       if (num_bc_t_sgs_nod .gt. 0) then
         ibc_t_sgs_id=0 
         bc_t_sgs_id_apt=0.0d00 
       end if
!
       end subroutine allocate_bc_t_sgs
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_bc_t_sgs_4_ele
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_t_sgs_id(num_index_ibc_t_sgs) )
        allocate ( nod_bc_t_sgs_id(num_index_ibc_t_sgs) )
!
        allocate ( ele_bc2_t_sgs_id(num_index_ibc2_t_sgs) )
        allocate ( nod_bc2_t_sgs_id(num_index_ibc2_t_sgs) )
!
        allocate ( ibc_t_sgs_stack(nnod_4_ele) )
        allocate ( ibc_t_sgs_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_t_sgs_shape(nnod_4_ele) )
!
        if ( num_index_ibc_t_sgs.gt.0) then
         ele_bc_t_sgs_id = 0
         nod_bc_t_sgs_id = 0
         ibc_t_sgs_stack = 0
         ibc_t_sgs_shape = 0
         ibc_t_sgs_stack_smp = 0
        end if
!
        if ( num_index_ibc2_t_sgs.gt.0) then
         ele_bc2_t_sgs_id = 0
         nod_bc2_t_sgs_id = 0
        end if
!
       end subroutine allocate_bc_t_sgs_4_ele
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_t_sgs
!
        deallocate( ibc_t_sgs )
        deallocate( ibc2_t_sgs )
!
       end subroutine deallocate_ibc_4_t_sgs
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_bc2_t_sgs
!
        deallocate ( ele_bc2_t_sgs_id )
        deallocate ( nod_bc2_t_sgs_id )
!
       end subroutine deallocate_bc2_t_sgs
!
      end module m_bc_temp_sgs
