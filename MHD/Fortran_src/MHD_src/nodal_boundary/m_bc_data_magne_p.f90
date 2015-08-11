!
!     module m_bc_data_magne_p
!.......................................................................
!
!      Written by Kemorin
!
!       subroutine allocate_bc_magne_p(numnod)
!       subroutine allocate_bc_magne_p_4_element
!       subroutine deallocate_ibc_4_magne_p
!       subroutine deallocate_bc2_magne_p
!
      module m_bc_data_magne_p
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_mag_p(:)
      integer (kind=kint), allocatable :: ibc2_mag_p(:)
! 
!
      integer (kind=kint) :: num_bc_mag_p_nod
      integer (kind=kint), allocatable :: ibc_mag_p_id(:)
      real (kind=kreal), allocatable :: bc_mag_p_id_apt(:)
!
!
      integer (kind=kint) :: num_index_ibc_mag_p
      integer (kind=kint) :: num_index_ibc2_mag_p
      integer (kind=kint), allocatable :: ele_bc_mag_p_id(:)
      integer (kind=kint), allocatable :: nod_bc_mag_p_id(:)
      integer (kind=kint), allocatable :: ele_bc2_mag_p_id(:)
      integer (kind=kint), allocatable :: nod_bc2_mag_p_id(:)
! 
!
      integer (kind=kint) :: ibc_mag_p_end
      integer (kind=kint), allocatable :: ibc_mag_p_shape(:)
      integer (kind=kint), allocatable :: ibc_mag_p_stack(:)
      integer (kind=kint), allocatable :: ibc_mag_p_stack_smp(:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_p(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(ibc_mag_p(numnod))
       allocate(ibc2_mag_p(numnod))
!
       if(numnod .gt. 0) then
         ibc_mag_p=0
         ibc2_mag_p=0
       end if
! 
       allocate(ibc_mag_p_id(num_bc_mag_p_nod))
       allocate(bc_mag_p_id_apt(num_bc_mag_p_nod))
       if (num_bc_mag_p_nod .gt. 0) then
        ibc_mag_p_id=0
        bc_mag_p_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_magne_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_magne_p_4_element
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_machine_parameter
!
        allocate ( ele_bc_mag_p_id(num_index_ibc_mag_p) )
        allocate ( nod_bc_mag_p_id(num_index_ibc_mag_p) )
!
        allocate ( ele_bc2_mag_p_id(num_index_ibc2_mag_p) )
        allocate ( nod_bc2_mag_p_id(num_index_ibc2_mag_p) )
!
        allocate ( ibc_mag_p_stack(num_t_linear) )
        allocate ( ibc_mag_p_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_mag_p_shape(num_t_linear) )
!
!
        if ( num_index_ibc_mag_p.gt.0) then
         ele_bc_mag_p_id = 0
         nod_bc_mag_p_id = 0
         ibc_mag_p_stack = 0
         ibc_mag_p_shape = 0
         ibc_mag_p_stack_smp = 0
        end if
!
        if ( num_index_ibc2_mag_p.gt.0) then
         ele_bc2_mag_p_id = 0
         nod_bc2_mag_p_id = 0
        end if
!
!
       end subroutine allocate_bc_magne_p_4_element
!
! -----------------------------------------------------------------------
!
       subroutine  deallocate_ibc_4_magne_p
!
       deallocate(ibc_mag_p)
       deallocate(ibc2_mag_p)
!
       end subroutine  deallocate_ibc_4_magne_p
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_magne_p
!
       deallocate ( ele_bc2_mag_p_id )
       deallocate ( nod_bc2_mag_p_id )
!
       end subroutine deallocate_bc2_magne_p
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_magne_p
