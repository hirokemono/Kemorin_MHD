!
!     module m_bc_data_press
!.......................................................................
!
!     Written by Kemorin
!
!       subroutine allocate_bc_press(numnod)
!       subroutine allocate_bc_press_4_element
!       subroutine deallocate_ibc_4_press
!       subroutine deallocate_bc2_press
!
      module m_bc_data_press
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_press(:)
      integer (kind=kint), allocatable :: ibc2_press(:)
! 
!
      integer (kind=kint) :: num_bc_p_nod
      integer (kind=kint), allocatable :: ibc_p_id(:)
      real (kind=kreal),   allocatable :: bc_p_id_apt(:)
! 
!
      integer (kind=kint) :: num_index_ibc_press
      integer (kind=kint) :: num_index_ibc2_press
      integer (kind=kint), allocatable :: ele_bc_p_id(:)
      integer (kind=kint), allocatable :: nod_bc_p_id(:)
      integer (kind=kint), allocatable :: ele_bc2_p_id(:)
      integer (kind=kint), allocatable :: nod_bc2_p_id(:)
! 
!
      integer (kind=kint) :: ibc_p_end
      integer (kind=kint), allocatable :: ibc_p_shape(:)
      integer (kind=kint), allocatable :: ibc_p_stack(:)
      integer (kind=kint), allocatable :: ibc_p_stack_smp(:)
! 
!
      real (kind=kreal) :: sum_phi, sum_phi0
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_press(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_press(numnod))
       allocate(ibc2_press(numnod))
!
       if(numnod .gt. 0) then
         ibc_press=0
         ibc2_press=0
       end if
! 
       allocate(ibc_p_id(num_bc_p_nod))
       allocate(bc_p_id_apt(num_bc_p_nod))
!
       if (num_bc_p_nod .gt. 0) then
        ibc_p_id=0
        bc_p_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_press
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_press_4_element
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_machine_parameter
!
        allocate ( ele_bc_p_id(num_index_ibc_press) )
        allocate ( nod_bc_p_id(num_index_ibc_press) )
        allocate ( ele_bc2_p_id(num_index_ibc2_press) )
        allocate ( nod_bc2_p_id(num_index_ibc2_press) )
!
        allocate ( ibc_p_stack(num_t_linear) )
        allocate ( ibc_p_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_p_shape(num_t_linear) )
!
        if ( num_index_ibc_press.gt.0) then
         ele_bc_p_id = 0
         nod_bc_p_id = 0
         ibc_p_stack = 0
         ibc_p_shape = 0
         ibc_p_stack_smp = 0
        end if
!
        if ( num_index_ibc2_press.gt.0) then
         ele_bc2_p_id = 0
         nod_bc2_p_id = 0
        end if
!
       end subroutine allocate_bc_press_4_element
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_press
!
        deallocate( ibc_press )
        deallocate( ibc2_press )
!
       end subroutine deallocate_ibc_4_press
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_press
!
        deallocate ( ele_bc2_p_id )
        deallocate ( nod_bc2_p_id )
!
       end subroutine deallocate_bc2_press
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_press
