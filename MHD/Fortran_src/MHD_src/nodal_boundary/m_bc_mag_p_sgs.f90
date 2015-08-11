!
!     module m_bc_mag_p_sgs
!.......................................................................
!
!      Written by Kemorin
!
!       subroutine allocate_bc_magp_sgs(numnod)
!
      module m_bc_mag_p_sgs
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: ibc_mp_sgs(:)
      integer (kind=kint), allocatable :: ibc2_mp_sgs(:)
! 
!
      integer (kind=kint) :: num_bc_mp_sgs_nod
      integer (kind=kint), allocatable :: ibc_mp_sgs_id(:)
      real (kind=kreal), allocatable :: bc_mp_sgs_id_apt(:)
!
!
      integer (kind=kint) :: num_index_ibc_mp_sgs
      integer (kind=kint) :: num_index_ibc2_mp_sgs
      integer (kind=kint), allocatable :: ele_bc_mp_sgs_id(:)
      integer (kind=kint), allocatable :: nod_bc_mp_sgs_id(:)
      integer (kind=kint), allocatable :: ele_bc2_mp_sgs_id(:)
      integer (kind=kint), allocatable :: nod_bc2_mp_sgs_id(:)
! 
!
      integer (kind=kint) :: ibc_mp_sgs_end
      integer (kind=kint), allocatable :: ibc_mp_sgs_shape(:)
      integer (kind=kint), allocatable :: ibc_mp_sgs_stack(:)
      integer (kind=kint), allocatable :: ibc_mp_sgs_stack_smp(:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magp_sgs(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_mp_sgs(numnod))
       allocate(ibc2_mp_sgs(numnod))
!
       if(numnod .gt. 0) then
         ibc_mp_sgs=0
         ibc2_mp_sgs=0
       end if
! 
       allocate(ibc_mp_sgs_id(num_bc_mp_sgs_nod))
       allocate(bc_mp_sgs_id_apt(num_bc_mp_sgs_nod))
       if (num_bc_mp_sgs_nod .gt. 0) then
        ibc_mp_sgs_id=0
        bc_mp_sgs_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_magp_sgs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_magp_sgs_4_ele
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_constants
!
        allocate ( ele_bc_mp_sgs_id(num_index_ibc_mp_sgs) )
        allocate ( nod_bc_mp_sgs_id(num_index_ibc_mp_sgs) )
!
        allocate ( ele_bc2_mp_sgs_id(num_index_ibc2_mp_sgs) )
        allocate ( nod_bc2_mp_sgs_id(num_index_ibc2_mp_sgs) )
!
        allocate ( ibc_mp_sgs_stack(num_t_linear) )
        allocate ( ibc_mp_sgs_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_mp_sgs_shape(num_t_linear) )
!
!
        if ( num_index_ibc_mp_sgs.gt.0) then
         ele_bc_mp_sgs_id = 0
         nod_bc_mp_sgs_id = 0
         ibc_mp_sgs_stack = 0
         ibc_mp_sgs_shape = 0
         ibc_mp_sgs_stack_smp = 0
        end if
!
        if ( num_index_ibc2_mp_sgs.gt.0) then
         ele_bc2_mp_sgs_id = 0
         nod_bc2_mp_sgs_id = 0
        end if
!
!
       end subroutine allocate_bc_magp_sgs_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine  deallocate_ibc_4_magp_sgs
!
       deallocate(ibc_mp_sgs)
       deallocate(ibc2_mp_sgs)
!
       end subroutine  deallocate_ibc_4_magp_sgs
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_magp_sgs
!
       deallocate ( ele_bc2_mp_sgs_id )
       deallocate ( nod_bc2_mp_sgs_id )
!
       end subroutine deallocate_bc2_magp_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_mag_p_sgs
