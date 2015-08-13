!
!     module m_bc_velo_sgs
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_bc_v_sgs(numnod)
!       subroutine allocate_bc_vsgs_4_ele(nnod_4_ele)
!
      module m_bc_velo_sgs
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_v_sgs(:,:)
      integer (kind=kint), allocatable :: ibc2_v_sgs(:,:)
!
!
      integer (kind=kint) :: nmax_bc_v_sgs_nod
      integer (kind=kint) :: num_bc_v_sgs_nod(3)
      integer (kind=kint), allocatable :: ibc_v_sgs_id(:,:)
      real (kind=kreal)  , allocatable :: bc_v_sgs_apt(:,:)
!
!
      integer (kind=kint) :: nmax_idx_ibc_v_sgs
      integer (kind=kint) :: num_idx_ibc_v_sgs(3)
      integer (kind=kint), allocatable :: ele_bc_v_sgs_id(:,:)
      integer (kind=kint), allocatable :: nod_bc_v_sgs_id(:,:)
!
      integer (kind=kint) :: nmax_idx_ibc2_v_sgs
      integer (kind=kint) :: num_idx_ibc2_v_sgs(3)
      integer (kind=kint), allocatable :: ele_bc2_v_sgs_id(:,:)
      integer (kind=kint), allocatable :: nod_bc2_v_sgs_id(:,:)
!
!
      integer (kind=kint) :: ibc_v_sgs_end(3)
      integer (kind=kint), allocatable :: ibc_v_sgs_shape(:,:)
      integer (kind=kint), allocatable :: ibc_v_sgs_stack(:,:)
      integer (kind=kint), allocatable :: ibc_v_sgs_stack_smp(:,:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_v_sgs(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_v_sgs(numnod,3) )
       allocate(ibc2_v_sgs(numnod,3))
!
       if(numnod .gt. 0) then
         ibc_v_sgs =  0
         ibc2_v_sgs = 0
       end if
! 
       allocate(ibc_v_sgs_id(nmax_bc_v_sgs_nod,3))
       allocate(bc_v_sgs_apt(nmax_bc_v_sgs_nod,3))
       if (nmax_bc_v_sgs_nod .gt. 0) then
        ibc_v_sgs_id=0 
        bc_v_sgs_apt=0.0d00
       end if
!
       end subroutine allocate_bc_v_sgs
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vsgs_4_ele(nnod_4_ele)
!
       use m_machine_parameter
!
       integer(kind = kint), intent(in) :: nnod_4_ele
!
!
        allocate ( ele_bc_v_sgs_id(nmax_idx_ibc_v_sgs,3) )
        allocate ( nod_bc_v_sgs_id(nmax_idx_ibc_v_sgs,3) )
        allocate ( ele_bc2_v_sgs_id(nmax_idx_ibc2_v_sgs,3) )
        allocate ( nod_bc2_v_sgs_id(nmax_idx_ibc2_v_sgs,3) )
!
!
        allocate ( ibc_v_sgs_stack(0:nnod_4_ele,3) )
        allocate ( ibc_v_sgs_stack_smp(0:nnod_4_ele*np_smp,3) )
        allocate ( ibc_v_sgs_shape(nnod_4_ele,3) )
!
        if ( nmax_idx_ibc_v_sgs.gt.0) then
         ele_bc_v_sgs_id = 0
         nod_bc_v_sgs_id = 0
         ibc_v_sgs_shape = 0
         ibc_v_sgs_stack = 0
         ibc_v_sgs_stack_smp = 0
        end if
!
        if ( nmax_idx_ibc2_v_sgs.gt.0) then
         ele_bc2_v_sgs_id = 0
         nod_bc2_v_sgs_id = 0
        end if
!
       end subroutine allocate_bc_vsgs_4_ele
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_vsgs
!
        deallocate( ibc_v_sgs )
        deallocate( ibc2_v_sgs )
!
       end subroutine deallocate_ibc_4_vsgs
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_vsgs
!
        deallocate ( ele_bc2_v_sgs_id )
        deallocate ( nod_bc2_v_sgs_id )
!
       end subroutine deallocate_bc2_vsgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_velo_sgs
