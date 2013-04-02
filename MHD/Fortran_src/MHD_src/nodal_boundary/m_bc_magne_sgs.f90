!
!     module m_bc_magne_sgs
!.......................................................................
!
!     Written by Kemorin
!
      module m_bc_magne_sgs
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_b_sgs(:,:)
      integer (kind=kint), allocatable :: ibc2_b_sgs(:,:)
!
!
      integer (kind=kint) :: nmax_bc_b_sgs_nod
      integer (kind=kint) :: num_bc_b_sgs_nod(3)
      integer (kind=kint), allocatable :: ibc_b_sgs_id(:,:)
      real (kind=kreal),   allocatable :: bc_b_sgs_id_apt(:,:)
! 
!
      integer (kind=kint) :: nmax_idx_ibc_b_sgs
      integer (kind=kint) :: num_idx_ibc_b_sgs(3)
      integer (kind=kint), allocatable :: ele_bc_b_sgs_id(:,:)
      integer (kind=kint), allocatable :: nod_bc_b_sgs_id(:,:)
!
      integer (kind=kint) :: nmax_idx_ibc2_b_sgs
      integer (kind=kint) :: num_idx_ibc2_b_sgs(3)
      integer (kind=kint), allocatable :: ele_bc2_b_sgs_id(:,:)
      integer (kind=kint), allocatable :: nod_bc2_b_sgs_id(:,:)
!
!
      integer (kind=kint) :: ibc_b_sgs_end(3)
      integer (kind=kint), allocatable :: ibc_b_sgs_shape(:,:)
      integer (kind=kint), allocatable :: ibc_b_sgs_stack(:,:)
      integer (kind=kint), allocatable :: ibc_b_sgs_stack_smp(:,:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_b_sgs
!
       use m_geometry_parameter
!
       allocate(ibc_b_sgs(numnod,3))
       allocate(ibc2_b_sgs(numnod,3))
!
       ibc_b_sgs=0
       ibc2_b_sgs=0
!
       allocate(ibc_b_sgs_id(nmax_bc_b_sgs_nod,3))
       allocate(bc_b_sgs_id_apt(nmax_bc_b_sgs_nod,3))
       if (nmax_bc_b_sgs_nod/=0) then
        ibc_b_sgs_id=0 
        bc_b_sgs_id_apt=0.0d00
       end if
!
!
       end subroutine allocate_bc_b_sgs
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_b_sgs_ele
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_b_sgs_id(nmax_idx_ibc_b_sgs,3) )
        allocate ( nod_bc_b_sgs_id(nmax_idx_ibc_b_sgs,3) )
!
        allocate ( ele_bc2_b_sgs_id(nmax_idx_ibc2_b_sgs,3) )
        allocate ( nod_bc2_b_sgs_id(nmax_idx_ibc2_b_sgs,3) )
!
        allocate ( ibc_b_sgs_stack(0:nnod_4_ele,3) )
        allocate ( ibc_b_sgs_stack_smp(0:nnod_4_ele*np_smp,3) )
        allocate ( ibc_b_sgs_shape(nnod_4_ele,3) )
!
!
        if ( nmax_idx_ibc_b_sgs.gt.0) then
         ele_bc_b_sgs_id = 0
         nod_bc_b_sgs_id = 0
         ibc_b_sgs_shape = 0
         ibc_b_sgs_stack = 0
         ibc_b_sgs_stack_smp = 0
        end if

        if ( nmax_idx_ibc2_b_sgs.gt.0) then
         ele_bc2_b_sgs_id = 0
         nod_bc2_b_sgs_id = 0
        end if
!
       end subroutine allocate_bc_b_sgs_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_b_sgs
!
        deallocate( ibc_b_sgs )
        deallocate( ibc2_b_sgs )
!
       end subroutine deallocate_ibc_4_b_sgs
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_b_sgs
!
        deallocate ( ele_bc2_b_sgs_id )
        deallocate ( nod_bc2_b_sgs_id )
!
       end subroutine deallocate_bc2_b_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_magne_sgs
