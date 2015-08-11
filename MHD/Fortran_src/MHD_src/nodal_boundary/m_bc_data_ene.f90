!
!     module m_bc_data_ene
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!       subroutine allocate_bc_ene(numnod)
!       subroutine allocate_bc_temp_4_element
!       subroutine deallocate_ibc_4_temp
!       subroutine deallocate_bc2_temp
!
!
      module m_bc_data_ene
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: ibc_temp(:)
      integer (kind=kint), allocatable :: ibc2_temp(:)
! 
!
      integer (kind=kint) :: num_bc_e_nod
      integer (kind=kint), allocatable :: ibc_e_id(:)
      real (kind=kreal),   allocatable :: bc_e_id_apt(:)
! 
!
      integer (kind=kint) :: num_index_ibc_temp
      integer (kind=kint) :: num_index_ibc2_temp
      integer (kind=kint), allocatable :: ele_bc_temp_id(:)
      integer (kind=kint), allocatable :: nod_bc_temp_id(:)
      integer (kind=kint), allocatable :: ele_bc2_temp_id(:)
      integer (kind=kint), allocatable :: nod_bc2_temp_id(:)
! 
!
      integer (kind=kint) :: ibc_temp_end
      integer (kind=kint), allocatable :: ibc_temp_shape(:)
      integer (kind=kint), allocatable :: ibc_temp_stack(:)
      integer (kind=kint), allocatable :: ibc_temp_stack_smp(:)
! 
!  ---------------------------------------------------------------------
!
      contains 
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_bc_ene(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(ibc_temp(numnod))
      allocate(ibc2_temp(numnod))
!
      if (numnod .gt. 0) then
        ibc_temp=0
        ibc2_temp=0
      end if
! 
      allocate(ibc_e_id(num_bc_e_nod))
      allocate(bc_e_id_apt(num_bc_e_nod))
      if (num_bc_e_nod .gt. 0) then
        ibc_e_id=0 
        bc_e_id_apt=0.0d00 
      end if
!
      end subroutine allocate_bc_ene
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_bc_temp_4_element
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_temp_id(num_index_ibc_temp) )
        allocate ( nod_bc_temp_id(num_index_ibc_temp) )
!
        allocate ( ele_bc2_temp_id(num_index_ibc2_temp) )
        allocate ( nod_bc2_temp_id(num_index_ibc2_temp) )
!
        allocate ( ibc_temp_stack(nnod_4_ele) )
        allocate ( ibc_temp_stack_smp(0:nnod_4_ele*np_smp) )
        allocate ( ibc_temp_shape(nnod_4_ele) )
!
        if ( num_index_ibc_temp.gt.0) then
         ele_bc_temp_id = 0
         nod_bc_temp_id = 0
         ibc_temp_stack = 0
         ibc_temp_shape = 0
         ibc_temp_stack_smp = 0
        end if
!
        if ( num_index_ibc2_temp.gt.0) then
         ele_bc2_temp_id = 0
         nod_bc2_temp_id = 0
        end if
!
       end subroutine allocate_bc_temp_4_element
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_temp
!
        deallocate( ibc_temp )
        deallocate( ibc2_temp )
!
       end subroutine deallocate_ibc_4_temp
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_bc2_temp
!
        deallocate ( ele_bc2_temp_id )
        deallocate ( nod_bc2_temp_id )
!
       end subroutine deallocate_bc2_temp
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_ene
