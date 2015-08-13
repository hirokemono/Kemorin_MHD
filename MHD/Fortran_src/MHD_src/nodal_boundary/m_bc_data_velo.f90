!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_bc_velo(numnod)
!       subroutine allocate_bc_velo_4_element(nnod_4_ele)
!       subroutine deallocate_ibc_4_velo
!       subroutine deallocate_bc2_velo
!
      module m_bc_data_velo
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_velo(:,:)
      integer (kind=kint), allocatable :: ibc2_velo(:,:)
!
!
      integer (kind=kint) :: nmax_bc_v_nod
      integer (kind=kint) :: num_bc_v_nod(3)
      integer (kind=kint), allocatable :: ibc_v_id(:,:)
      real (kind=kreal),   allocatable :: bc_v_id_apt(:,:)
!
!
      integer (kind=kint) :: nmax_idx_ibc_v
      integer (kind=kint) :: num_idx_ibc_v(3)
      integer (kind=kint), allocatable :: ele_bc_v_id(:,:)
      integer (kind=kint), allocatable :: nod_bc_v_id(:,:)
!
      integer (kind=kint) :: nmax_idx_ibc2_v
      integer (kind=kint) :: num_idx_ibc2_v(3)
      integer (kind=kint), allocatable :: ele_bc2_v_id(:,:)
      integer (kind=kint), allocatable :: nod_bc2_v_id(:,:)
!
!
      integer (kind=kint) :: ibc_v_end(3)
      integer (kind=kint), allocatable :: ibc_v_shape(:,:)
      integer (kind=kint), allocatable :: ibc_v_stack(:,:)
      integer (kind=kint), allocatable :: ibc_v_stack_smp(:,:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_velo(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_velo(numnod,3))
       allocate(ibc2_velo(numnod,3))
!
       if(numnod .gt. 0) then
         ibc_velo=0
         ibc2_velo=0
       end if
! 
       allocate(ibc_v_id(nmax_bc_v_nod,3))
       allocate(bc_v_id_apt(nmax_bc_v_nod,3))
       if (nmax_bc_v_nod/=0) then
        ibc_v_id=0 
        bc_v_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_velo
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_velo_4_element(nnod_4_ele)
!
        use m_machine_parameter
!
       integer(kind = kint), intent(in) :: nnod_4_ele
!
!
        allocate ( ele_bc_v_id(nmax_idx_ibc_v,3) )
        allocate ( nod_bc_v_id(nmax_idx_ibc_v,3) )
!
        allocate ( ele_bc2_v_id(nmax_idx_ibc2_v,3) )
        allocate ( nod_bc2_v_id(nmax_idx_ibc2_v,3) )
!
!
        allocate ( ibc_v_shape(nnod_4_ele,3) )
        allocate ( ibc_v_stack(0:nnod_4_ele,3) )
        allocate ( ibc_v_stack_smp(0:nnod_4_ele*np_smp,3) )
!
        if ( nmax_idx_ibc_v.gt.0) then
         ele_bc_v_id = 0
         nod_bc_v_id = 0
         ibc_v_stack = 0
         ibc_v_shape = 0
         ibc_v_stack_smp = 0
        end if
!
        if ( nmax_idx_ibc2_v.gt.0) then
         ele_bc2_v_id = 0
         nod_bc2_v_id = 0
        end if
!
       end subroutine allocate_bc_velo_4_element
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_velo
!
        deallocate( ibc_velo )
        deallocate( ibc2_velo )
!
       end subroutine deallocate_ibc_4_velo
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_velo
!
        deallocate ( ele_bc2_v_id )
        deallocate ( nod_bc2_v_id )
!
       end subroutine deallocate_bc2_velo
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_velo
