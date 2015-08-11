!
!     module m_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
!       subroutine allocate_bc_magne(numnod)
!       subroutine allocate_bc_magne_4_element
!
!       subroutine deallocate_ibc_4_magne
!       subroutine deallocate_bc2_magne
!
      module m_bc_data_magne
!
      use m_precision
!
      implicit  none
!
!
!
      integer (kind=kint), allocatable :: ibc_magne(:,:)
      integer (kind=kint), allocatable :: ibc2_magne(:,:)
!
!
      integer (kind=kint) :: nmax_bc_b_nod
      integer (kind=kint) :: num_bc_b_nod(3)
      integer (kind=kint), allocatable :: ibc_b_id(:,:)
      real (kind=kreal)  , allocatable :: bc_b_id_apt(:,:)
!
!
      integer (kind=kint) :: nmax_idx_ibc_b
      integer (kind=kint) :: num_idx_ibc_b(3)
      integer (kind=kint), allocatable :: ele_bc_b_id(:,:)
      integer (kind=kint), allocatable :: nod_bc_b_id(:,:)
!
      integer (kind=kint) :: nmax_idx_ibc2_b
      integer (kind=kint) :: num_idx_ibc2_b(3)
      integer (kind=kint), allocatable :: ele_bc2_b_id(:,:)
      integer (kind=kint), allocatable :: nod_bc2_b_id(:,:)
!
!
      integer (kind=kint) :: ibc_b_end(3)
      integer (kind=kint), allocatable :: ibc_b_shape(:,:)
      integer (kind=kint), allocatable :: ibc_b_stack(:,:)
      integer (kind=kint), allocatable :: ibc_b_stack_smp(:,:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       allocate(ibc_magne(numnod,3))
       allocate(ibc2_magne(numnod,3))
!
       if(numnod .gt. 0) then
         ibc_magne = 0
         ibc2_magne = 0
       end if
! 
       allocate(ibc_b_id(nmax_bc_b_nod,3))
       allocate(bc_b_id_apt(nmax_bc_b_nod,3))
       if (nmax_bc_b_nod .gt. 0) then
        ibc_b_id=0 
        bc_b_id_apt=0.0d00
       end if
!
!
       end subroutine allocate_bc_magne
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_4_element
!
        use m_geometry_parameter
        use m_machine_parameter
!
        allocate ( ele_bc_b_id(nmax_idx_ibc_b,3) )
        allocate ( nod_bc_b_id(nmax_idx_ibc_b,3) )
!
        allocate ( ele_bc2_b_id(nmax_idx_ibc2_b,3) )
        allocate ( nod_bc2_b_id(nmax_idx_ibc2_b,3) )
!
!
        allocate ( ibc_b_shape(nnod_4_ele,3) )
        allocate ( ibc_b_stack(0:nnod_4_ele,3) )
        allocate ( ibc_b_stack_smp(0:nnod_4_ele*np_smp,3) )
!
!
        if ( nmax_idx_ibc_b.gt.0) then
         ele_bc_b_id = 0
         nod_bc_b_id = 0
         ibc_b_stack = 0
         ibc_b_shape = 0
         ibc_b_stack_smp = 0
        end if
!
        if ( nmax_idx_ibc2_b.gt.0) then
         ele_bc2_b_id = 0
         nod_bc2_b_id = 0
        end if

!
       end subroutine allocate_bc_magne_4_element
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_ibc_4_magne
!
        deallocate( ibc_magne  )
        deallocate( ibc2_magne )
!
       end subroutine deallocate_ibc_4_magne
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc2_magne
!
        deallocate ( ele_bc2_b_id )
        deallocate ( nod_bc2_b_id )
!
       end subroutine deallocate_bc2_magne
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_magne
