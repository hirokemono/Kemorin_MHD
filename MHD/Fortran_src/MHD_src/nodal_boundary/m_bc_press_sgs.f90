!
!     module m_bc_press_sgs
!.......................................................................
!
!     Written by Kemorin
!
      module m_bc_press_sgs
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: ibc_press_sgs(:)
      integer (kind=kint), allocatable :: ibc2_press_sgs(:)
! 
!
      integer (kind=kint) :: num_bc_ps_nod
      integer (kind=kint), allocatable :: ibc_ps_id(:)
      real (kind=kreal), allocatable :: bc_ps_id_apt(:)
! 
!
      integer (kind=kint) :: num_index_ibc_p_sgs
      integer (kind=kint) :: num_index_ibc2_p_sgs
      integer (kind=kint), allocatable :: ele_bc_ps_id(:)
      integer (kind=kint), allocatable :: nod_bc_ps_id(:)
      integer (kind=kint), allocatable :: ele_bc2_ps_id(:)
      integer (kind=kint), allocatable :: nod_bc2_ps_id(:)
! 
!
      integer (kind=kint) :: ibc_ps_end
      integer (kind=kint), allocatable :: ibc_ps_shape(:)
      integer (kind=kint), allocatable :: ibc_ps_stack(:)
      integer (kind=kint), allocatable :: ibc_ps_stack_smp(:)
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
       subroutine allocate_bc_p_sgs
!
       use m_geometry_parameter
!
       allocate(ibc_press_sgs(numnod))
       allocate(ibc2_press_sgs(numnod))
!
       ibc_press_sgs=0
       ibc2_press_sgs=0
! 
       allocate(ibc_ps_id(num_bc_ps_nod))
       allocate(bc_ps_id_apt(num_bc_ps_nod))
!
       if (num_bc_ps_nod/=0) then
        ibc_ps_id=0
        bc_ps_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_p_sgs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_p_sgs_4_ele
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_machine_parameter
!
        allocate ( ele_bc_ps_id(num_index_ibc_p_sgs) )
        allocate ( nod_bc_ps_id(num_index_ibc_p_sgs) )
        allocate ( ele_bc2_ps_id(num_index_ibc2_p_sgs) )
        allocate ( nod_bc2_ps_id(num_index_ibc2_p_sgs) )
!
        allocate ( ibc_ps_stack(num_t_linear) )
        allocate ( ibc_ps_stack_smp(0:np_smp*num_t_linear) )
        allocate ( ibc_ps_shape(num_t_linear) )
!
        if ( num_index_ibc_p_sgs.gt.0) then
         ele_bc_ps_id = 0
         nod_bc_ps_id = 0
         ibc_ps_stack = 0
         ibc_ps_shape = 0
         ibc_ps_stack_smp = 0
        end if
!
        if ( num_index_ibc2_p_sgs.gt.0) then
         ele_bc2_ps_id = 0
         nod_bc2_ps_id = 0
        end if
!
       end subroutine allocate_bc_p_sgs_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_p_sgs
!
        deallocate( ibc_press_sgs )
        deallocate( ibc2_press_sgs )
!
       end subroutine deallocate_ibc_4_p_sgs
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_p_sgs
!
        deallocate ( ele_bc2_ps_id )
        deallocate ( nod_bc2_ps_id )
!
       end subroutine deallocate_bc2_p_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_press_sgs
