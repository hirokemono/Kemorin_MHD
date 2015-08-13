!
!     module m_bc_data_vect_p
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine allocate_bc_vect_p(numnod)
!       subroutine allocate_bc_vect_p_4_element(nnod_4_ele)
!
      module m_bc_data_vect_p
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: ibc_vp(:,:)
      integer (kind=kint), allocatable :: ibc2_vp(:,:)
!
      integer (kind=kint) :: nmax_bc_vp_nod
      integer (kind=kint) :: num_bc_vp_nod(3)
      integer (kind=kint), allocatable :: ibc_vp_id(:,:)
      real (kind=kreal),   allocatable :: bc_vp_id_apt(:,:)
!
!
      integer (kind=kint) :: nmax_idx_ibc_vp
      integer (kind=kint) :: num_idx_ibc_vp(3)
      integer (kind=kint), allocatable :: ele_bc_vp_id(:,:)
      integer (kind=kint), allocatable :: nod_bc_vp_id(:,:)
!
      integer (kind=kint) :: nmax_idx_ibc2_vp
      integer (kind=kint) :: num_idx_ibc2_vp(3)
      integer (kind=kint), allocatable :: ele_bc2_vp_id(:,:)
      integer (kind=kint), allocatable :: nod_bc2_vp_id(:,:)
!
!
      integer (kind=kint) :: ibc_vp_end(3)
      integer (kind=kint), allocatable :: ibc_vp_shape(:,:)
      integer (kind=kint), allocatable :: ibc_vp_stack(:,:)
      integer (kind=kint), allocatable :: ibc_vp_stack_smp(:,:)
!
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vect_p(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_vp(numnod,3))
       allocate(ibc2_vp(numnod,3))
!
       if(numnod .gt. 0) then
         ibc_vp = 0
         ibc2_vp = 0
       end if
! 
       allocate(ibc_vp_id(nmax_bc_vp_nod,3))
       allocate(bc_vp_id_apt(nmax_bc_vp_nod,3))
       if (nmax_bc_vp_nod .gt. 0) then
        ibc_vp_id=0 
        bc_vp_id_apt=0.0d00
       end if
!
       end subroutine allocate_bc_vect_p
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vect_p_4_element(nnod_4_ele)
!
        use m_machine_parameter
!
       integer(kind = kint), intent(in) :: nnod_4_ele
!
!
        allocate ( ele_bc_vp_id(nmax_idx_ibc_vp,3) )
        allocate ( nod_bc_vp_id(nmax_idx_ibc_vp,3) )
!
        allocate ( ele_bc2_vp_id(nmax_idx_ibc2_vp,3) )
        allocate ( nod_bc2_vp_id(nmax_idx_ibc2_vp,3) )
!
        allocate ( ibc_vp_stack(0:nnod_4_ele,3) )
        allocate ( ibc_vp_stack_smp(0:nnod_4_ele*np_smp,3) )
        allocate ( ibc_vp_shape(nnod_4_ele,3) )
!
        if ( nmax_idx_ibc_vp.gt.0) then
         ele_bc_vp_id = 0
         nod_bc_vp_id = 0
         ibc_vp_stack = 0
         ibc_vp_shape = 0
         ibc_vp_stack_smp = 0
        end if

        if ( nmax_idx_ibc2_vp.gt.0) then
         ele_bc2_vp_id = 0
         nod_bc2_vp_id = 0
        end if
!
       end subroutine allocate_bc_vect_p_4_element
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_vect_p
!
        deallocate( ibc_vp )
        deallocate( ibc2_vp )
!
       end subroutine deallocate_ibc_4_vect_p
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_vect_p
!
        deallocate ( ele_bc2_vp_id )
        deallocate ( nod_bc2_vp_id )
!
       end subroutine deallocate_bc2_vect_p
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_vect_p
