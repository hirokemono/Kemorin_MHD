!m_bc_data_composition.f90
!     module m_bc_data_composition
!.......................................................................
!
!    Written by kemorin
!
!       subroutine allocate_bc_composition
!       subroutine allocate_bc_composit_4_element
!
!       subroutine deallocate_ibc_4_composit
!       subroutine deallocate_bc2_composit
!
      module m_bc_data_composition
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_composit(:)
! 
      integer (kind=kint), allocatable :: ibc2_composit(:)
! 
!
      integer (kind=kint) :: num_bc_composition_nod
! 
      integer (kind=kint), allocatable :: ibc_composit_id(:)
      real (kind=kreal)  , allocatable ::  bc_composit_id_apt(:)
!
!
      integer (kind=kint) :: num_index_ibc_compsition
      integer (kind=kint) :: num_index_ibc2_compsition
! 
      integer (kind=kint), allocatable :: ele_bc_composit_id(:)
      integer (kind=kint), allocatable :: nod_bc_composit_id(:)
! 
      integer (kind=kint), allocatable :: ele_bc2_composit_id(:)
      integer (kind=kint), allocatable :: nod_bc2_composit_id(:)
! 
!
      integer (kind=kint) :: ibc_composition_end
! 
      integer (kind=kint), allocatable :: ibc_composit_shape(:)
      integer (kind=kint), allocatable :: ibc_composit_stack(:)
      integer (kind=kint), allocatable :: ibc_composit_stack_smp(:)
! 
!
      real (kind=kreal) :: sum_phi, sum_phi0
!
!  ---------------------------------------------------------------------
!
      contains 
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_bc_composition(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_composit(numnod))
       allocate(ibc2_composit(numnod))
!
       ibc_composit=0
       ibc2_composit=0
! 
        allocate(ibc_composit_id(num_bc_composition_nod))
        allocate(bc_composit_id_apt(num_bc_composition_nod))
       if (num_bc_composition_nod .gt. 0) then
         ibc_composit_id=0 
         bc_composit_id_apt=0.0d00 
       end if
!
       end subroutine allocate_bc_composition
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_bc_composit_4_element
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_composit_id(num_index_ibc_compsition) )
        allocate ( nod_bc_composit_id(num_index_ibc_compsition) )
!
        allocate ( ele_bc2_composit_id(num_index_ibc2_compsition) )
        allocate ( nod_bc2_composit_id(num_index_ibc2_compsition) )
!
        allocate ( ibc_composit_stack(nnod_4_ele) )
        allocate ( ibc_composit_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_composit_shape(nnod_4_ele) )
!
        if (num_index_ibc_compsition .gt. 0) then
         ele_bc_composit_id = 0
         nod_bc_composit_id = 0
!
         ibc_composit_stack = 0
         ibc_composit_shape = 0
         ibc_composit_stack_smp = 0
        end if
!
        if (num_index_ibc2_compsition .gt. 0) then
         ele_bc2_composit_id = 0
         nod_bc2_composit_id = 0
       end if
!
       end subroutine allocate_bc_composit_4_element
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_composit
!
        deallocate( ibc_composit )
        deallocate( ibc2_composit )
!
       end subroutine deallocate_ibc_4_composit
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_bc2_composit
!
       deallocate ( ele_bc2_composit_id )
       deallocate ( nod_bc2_composit_id )
!
       end subroutine deallocate_bc2_composit
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_composition
