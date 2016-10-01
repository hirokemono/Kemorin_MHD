!t_nodal_bc_data.f90
!      module t_nodal_bc_data
!
!        programmed by H.Matsui on Jan., 2009
!
!>  @brief Structure for nodal boundary data for MHD dynamo model
!!
!!      subroutine alloc_scalar_nod_bc_type(numnod, scalar_bc)
!!      subroutine alloc_nod_bc_scalar_ele_type(np_smp, nnod_4_ele,     &
!!      &         scalar_bc)
!!      subroutine alloc_vector_nod_bc_type(numnod, vector_bc)
!!      subroutine alloc_nod_bc_vector_ele_type(np_smp, nnod_4_ele,     &
!!      &         vector_bc)
!!      subroutine alloc_rotate_nod_bc_type(numnod, rot_bc)
!!      subroutine alloc_nod_bc_rotate_ele_type(np_smp, nnod_4_ele,     &
!!      &         rot_bc)
!!
!!      subroutine dealloc_scalar_ibc_type(scalar_bc)
!!      subroutine dealloc_scalar_nod_bc_type(scalar_bc)
!!      subroutine dealloc_nod_bc_scalar_ele_type(scalar_bc)
!!      subroutine dealloc_vector_ibc_type(vector_bc)
!!      subroutine dealloc_vector_nod_bc_type(vector_bc)
!!      subroutine dealloc_nod_bc_vector_ele_type(vector_bc)
!!      subroutine dealloc_rotate_ibc_type(rot_bc)
!!      subroutine dealloc_rotate_nod_bc_type(rot_bc)
!!      subroutine dealloc_nod_bc_rotate_ele_type(rot_bc)
!!
!!      subroutine alloc_ref_temp_nod_bc( numnod, nod_bc_list )
!
      module t_nodal_bc_data
!
      use m_precision
!
      implicit none
!
      type scaler_fixed_nod_bc_type
        character(len=kchara) :: scalar_bc_name
        integer (kind=kint), allocatable :: ibc(:)
        integer (kind=kint), allocatable :: ibc2(:)
!
        integer (kind=kint) :: num_bc_nod
        integer (kind=kint), allocatable :: ibc_id(:)
        real (kind=kreal),   allocatable :: bc_apt(:)
!
        integer (kind=kint) :: num_idx_ibc
        integer (kind=kint), allocatable :: ele_bc_id(:)
        integer (kind=kint), allocatable :: nod_bc_id(:)
!
        integer (kind=kint) :: num_idx_ibc2
        integer (kind=kint), allocatable :: ele_bc2_id(:)
        integer (kind=kint), allocatable :: nod_bc2_id(:)
!
        integer (kind=kint) :: ibc_end
        integer (kind=kint), allocatable :: ibc_shape(:)
        integer (kind=kint), allocatable :: ibc_stack(:)
        integer (kind=kint), allocatable :: ibc_stack_smp(:)
      end type scaler_fixed_nod_bc_type
!
!
      type vect_fixed_nod_bc_type
        character(len=kchara) :: vect_bc_name(3)
        integer (kind=kint), allocatable :: ibc(:,:)
        integer (kind=kint), allocatable :: ibc2(:,:)
!
        integer (kind=kint) :: nmax_bc
        integer (kind=kint) :: num_bc_nod(3)
        integer (kind=kint), allocatable :: ibc_id(:,:)
        real (kind=kreal),   allocatable :: bc_apt(:,:)
!
        integer (kind=kint) :: nmax_idx_ibc
        integer (kind=kint) :: num_idx_ibc(3)
        integer (kind=kint), allocatable :: ele_bc_id(:,:)
        integer (kind=kint), allocatable :: nod_bc_id(:,:)
!
        integer (kind=kint) :: nmax_idx_ibc2
        integer (kind=kint) :: num_idx_ibc2(3)
        integer (kind=kint), allocatable :: ele_bc2_id(:,:)
        integer (kind=kint), allocatable :: nod_bc2_id(:,:)
!
        integer (kind=kint) :: ibc_end(3)
        integer (kind=kint), allocatable :: ibc_shape(:,:)
        integer (kind=kint), allocatable :: ibc_stack(:,:)
        integer (kind=kint), allocatable :: ibc_stack_smp(:,:)
      end type vect_fixed_nod_bc_type
!
      type scaler_rotaion_nod_bc_type
        integer (kind=kint), allocatable :: ibc(:)
        integer (kind=kint), allocatable :: ibc2(:)
!
        integer (kind=kint) :: num_bc_nod
        integer (kind=kint), allocatable :: ibc_id(:)
        real (kind=kreal),   allocatable :: bc_rot_apt(:,:)
!
        integer (kind=kint) :: num_idx_ibc
        integer (kind=kint), allocatable :: ele_bc_id(:)
        integer (kind=kint), allocatable :: nod_bc_id(:)
!
        integer (kind=kint) :: num_idx_ibc2
        integer (kind=kint), allocatable :: ele_bc2_id(:)
        integer (kind=kint), allocatable :: nod_bc2_id(:)
!
        integer (kind=kint) :: ibc_end
        integer (kind=kint), allocatable :: ibc_shape(:)
        integer (kind=kint), allocatable :: ibc_stack(:)
        integer (kind=kint), allocatable :: ibc_stack_smp(:)
      end type scaler_rotaion_nod_bc_type
!
      type scaler_current_nod_bc_type
        integer (kind=kint), allocatable :: ibc(:,:)
        integer (kind=kint), allocatable :: ibc2(:,:)
!
        integer (kind=kint) :: nmax_bc
        integer (kind=kint) :: num_bc_nod(3)
        integer (kind=kint), allocatable :: ibc_id(:,:)
!        real (kind=kreal),   allocatable :: bc_apt(:,:)
      end type scaler_current_nod_bc_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_scalar_nod_bc_type(numnod, scalar_bc)
!
      integer(kind = kint), intent(in) :: numnod
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      allocate(scalar_bc%ibc(numnod))
      allocate(scalar_bc%ibc2(numnod))
!
      if (numnod .gt. 0) then
        scalar_bc%ibc =  0
        scalar_bc%ibc2 = 0
      end if
! 
      allocate(scalar_bc%ibc_id(scalar_bc%num_bc_nod))
      allocate(scalar_bc%bc_apt(scalar_bc%num_bc_nod))
      if (scalar_bc%num_bc_nod .gt. 0) then
        scalar_bc%ibc_id = 0 
        scalar_bc%bc_apt = 0.0d00
      end if
!
      end subroutine alloc_scalar_nod_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_nod_bc_scalar_ele_type(np_smp, nnod_4_ele,       &
      &         scalar_bc)
!
      integer(kind = kint), intent(in) :: np_smp, nnod_4_ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      allocate ( scalar_bc%ele_bc_id(scalar_bc%num_idx_ibc) )
      allocate ( scalar_bc%nod_bc_id(scalar_bc%num_idx_ibc) )
!
      allocate ( scalar_bc%ele_bc2_id(scalar_bc%num_idx_ibc2) )
      allocate ( scalar_bc%nod_bc2_id(scalar_bc%num_idx_ibc2) )
!
      allocate ( scalar_bc%ibc_stack(0:nnod_4_ele) )
      allocate ( scalar_bc%ibc_stack_smp(0:nnod_4_ele*np_smp) )
      allocate ( scalar_bc%ibc_shape(nnod_4_ele) )
!
      if ( scalar_bc%num_idx_ibc.gt.0) then
        scalar_bc%ele_bc_id = 0
        scalar_bc%nod_bc_id = 0
        scalar_bc%ibc_stack = 0
        scalar_bc%ibc_shape = 0
        scalar_bc%ibc_stack_smp = 0
      end if
!
      if ( scalar_bc%num_idx_ibc2.gt.0) then
        scalar_bc%ele_bc2_id = 0
        scalar_bc%nod_bc2_id = 0
      end if
!
      end subroutine alloc_nod_bc_scalar_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_vector_nod_bc_type(numnod, vector_bc)
!
      integer(kind = kint), intent(in) :: numnod
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      allocate(vector_bc%ibc(numnod,3))
      allocate(vector_bc%ibc2(numnod,3))
!
      if (numnod .gt. 0) then
        vector_bc%ibc =  0
        vector_bc%ibc2 = 0
      end if
! 
      allocate(vector_bc%ibc_id(vector_bc%nmax_bc,3))
      allocate(vector_bc%bc_apt(vector_bc%nmax_bc,3))
      if (vector_bc%nmax_bc .gt. 0) then
        vector_bc%ibc_id = 0 
        vector_bc%bc_apt = 0.0d00
      end if
!
      end subroutine alloc_vector_nod_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_nod_bc_vector_ele_type(np_smp, nnod_4_ele,       &
      &         vector_bc)
!
      integer(kind = kint), intent(in) :: np_smp, nnod_4_ele
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!
      allocate ( vector_bc%ele_bc_id(vector_bc%nmax_idx_ibc,3) )
      allocate ( vector_bc%nod_bc_id(vector_bc%nmax_idx_ibc,3) )
!
      allocate ( vector_bc%ele_bc2_id(vector_bc%nmax_idx_ibc2,3) )
      allocate ( vector_bc%nod_bc2_id(vector_bc%nmax_idx_ibc2,3) )
!
      allocate ( vector_bc%ibc_stack(0:nnod_4_ele,3) )
      allocate ( vector_bc%ibc_stack_smp(0:nnod_4_ele*np_smp,3) )
      allocate ( vector_bc%ibc_shape(nnod_4_ele,3) )
!
      if ( vector_bc%nmax_idx_ibc.gt.0) then
        vector_bc%ele_bc_id = 0
        vector_bc%nod_bc_id = 0
        vector_bc%ibc_stack = 0
        vector_bc%ibc_shape = 0
        vector_bc%ibc_stack_smp = 0
      end if
!
      if ( vector_bc%nmax_idx_ibc2.gt.0) then
        vector_bc%ele_bc2_id = 0
        vector_bc%nod_bc2_id = 0
      end if
!
      end subroutine alloc_nod_bc_vector_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_rotate_nod_bc_type(numnod, rot_bc)
!
      integer(kind = kint), intent(in) :: numnod
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rot_bc
!
      allocate(rot_bc%ibc(numnod))
      allocate(rot_bc%ibc2(numnod))
!
      if (numnod .gt. 0) then
        rot_bc%ibc =  0
        rot_bc%ibc2 = 0
      end if
! 
      allocate(rot_bc%ibc_id(rot_bc%num_bc_nod))
      allocate(rot_bc%bc_rot_apt(rot_bc%num_bc_nod,3))
      if (rot_bc%num_bc_nod .gt. 0) then
        rot_bc%ibc_id = 0 
        rot_bc%bc_rot_apt = 0.0d00
      end if
!
      end subroutine alloc_rotate_nod_bc_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_bc_rotate_ele_type(np_smp, nnod_4_ele,       &
      &         rot_bc)
!
      integer(kind = kint), intent(in) :: np_smp, nnod_4_ele
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rot_bc
!
!
      allocate ( rot_bc%ele_bc_id(rot_bc%num_idx_ibc) )
      allocate ( rot_bc%nod_bc_id(rot_bc%num_idx_ibc) )
!
      allocate ( rot_bc%ele_bc2_id(rot_bc%num_idx_ibc2) )
      allocate ( rot_bc%nod_bc2_id(rot_bc%num_idx_ibc2) )
!
      allocate ( rot_bc%ibc_stack(0:nnod_4_ele) )
      allocate ( rot_bc%ibc_stack_smp(0:nnod_4_ele*np_smp) )
      allocate ( rot_bc%ibc_shape(nnod_4_ele) )
!
      if ( rot_bc%num_idx_ibc.gt.0) then
        rot_bc%ele_bc_id = 0
        rot_bc%nod_bc_id = 0
        rot_bc%ibc_stack = 0
        rot_bc%ibc_shape = 0
        rot_bc%ibc_stack_smp = 0
      end if
!
      if ( rot_bc%num_idx_ibc2.gt.0) then
        rot_bc%ele_bc2_id = 0
        rot_bc%nod_bc2_id = 0
      end if
!
       end subroutine alloc_nod_bc_rotate_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_scalar_ibc_type(scalar_bc)
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      deallocate(scalar_bc%ibc)
      deallocate(scalar_bc%ibc2)
!
      end subroutine dealloc_scalar_ibc_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_scalar_nod_bc_type(scalar_bc)
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      deallocate(scalar_bc%ibc_id)
!      deallocate(scalar_bc%bc_apt)
!
      end subroutine dealloc_scalar_nod_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nod_bc_scalar_ele_type(scalar_bc)
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      deallocate ( scalar_bc%ele_bc_id )
      deallocate ( scalar_bc%nod_bc_id )
!
      deallocate ( scalar_bc%ele_bc2_id )
      deallocate ( scalar_bc%nod_bc2_id )
!
      deallocate ( scalar_bc%ibc_stack )
      deallocate ( scalar_bc%ibc_stack_smp )
      deallocate ( scalar_bc%ibc_shape )
!
      end subroutine dealloc_nod_bc_scalar_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_vector_ibc_type(vector_bc)
!
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      deallocate(vector_bc%ibc )
      deallocate(vector_bc%ibc2)
!
      end subroutine dealloc_vector_ibc_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_vector_nod_bc_type(vector_bc)
!
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      deallocate(vector_bc%ibc_id)
!      deallocate(vector_bc%bc_apt)
!
      end subroutine dealloc_vector_nod_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nod_bc_vector_ele_type(vector_bc)
!
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!
      deallocate ( vector_bc%ele_bc_id )
      deallocate ( vector_bc%nod_bc_id )
!
      deallocate ( vector_bc%ele_bc2_id )
      deallocate ( vector_bc%nod_bc2_id )
!
      deallocate ( vector_bc%ibc_stack )
      deallocate ( vector_bc%ibc_stack_smp )
      deallocate ( vector_bc%ibc_shape )
!
      end subroutine dealloc_nod_bc_vector_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_rotate_ibc_type(rot_bc)
!
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rot_bc
!
      deallocate(rot_bc%ibc )
      deallocate(rot_bc%ibc2)
!
      end subroutine dealloc_rotate_ibc_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rotate_nod_bc_type(rot_bc)
!
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rot_bc
!
      deallocate(rot_bc%ibc_id)
      deallocate(rot_bc%bc_rot_apt)
!
      end subroutine dealloc_rotate_nod_bc_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_bc_rotate_ele_type(rot_bc)
!
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rot_bc
!
!
      deallocate ( rot_bc%ele_bc_id )
      deallocate ( rot_bc%nod_bc_id )
!
      deallocate ( rot_bc%ele_bc2_id )
      deallocate ( rot_bc%nod_bc2_id )
!
      deallocate ( rot_bc%ibc_stack )
      deallocate ( rot_bc%ibc_stack_smp )
      deallocate ( rot_bc%ibc_shape )
!
       end subroutine dealloc_nod_bc_rotate_ele_type
!
! -----------------------------------------------------------------------
!
      end module t_nodal_bc_data
