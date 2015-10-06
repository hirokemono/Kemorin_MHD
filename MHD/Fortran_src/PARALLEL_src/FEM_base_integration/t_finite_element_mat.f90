!t_finite_element_mat.f90
!     module t_finite_element_mat
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler
!
!      subroutine alloc_fem_mat_base_type(numnod, numele, nnod_4_ele,   &
!     &          np_smp, maxnod_4_smp, rhs_mat)
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: numnod, maxnod_4_smp, np_smp
!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!      subroutine alloc_type_fem_mat_work(numele, nnod_4_ele, fem_wk)
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!      subroutine alloc_type_fem_lumped_mass(num, lump)
!        integer(kind = kint), intent(in) :: num
!        type(lumped_mass_matrices), intent(inout) :: lump
!      subroutine alloc_type_fem_matrices(numdir, numnod, maxnod_4_smp, &
!     &          np_smp, rhs)
!        integer(kind = kint), intent(in) :: numdir, np_smp
!        integer(kind = kint), intent(in) :: numnod, maxnod_4_smp
!        type(finite_ele_mat_node), intent(inout) :: rhs
!
!      subroutine reset_ff_smps_type(numdir, maxnod_4_smp, np_smp, rhs)
!      subroutine reset_ff_type(numdir, numnod, rhs)
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: numdir, numnod
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!      subroutine dealloc_fem_mat_base_type(rhs_mat)
!
!      subroutine dealloc_type_fem_mat_work(fem_wk)
!      subroutine dealloc_type_fem_lumped_mass(lump)
!      subroutine dealloc_type_fem_matrices(rhs)
!
!
      module t_finite_element_mat
!
      use m_precision
!
      implicit  none
!
!
      type lumped_mass_matrices
        real (kind=kreal), pointer  ::  ml(:)
        real (kind=kreal), pointer  ::  ml_o(:)
      end type lumped_mass_matrices
!
      type finite_ele_mat_node
        real (kind=kreal), pointer  :: ff(:,:)
        real (kind=kreal), pointer  :: ff_smp(:,:,:)
      end type finite_ele_mat_node
!
!
      type work_finite_element_mat
        real (kind=kreal), pointer  ::  sk6(:,:,:)
!
        real(kind=kreal), pointer  ::  scalar_1(:)
        real(kind=kreal), pointer  ::  vector_1(:,:)
        real(kind=kreal), pointer  ::  tensor_1(:,:)
!
        real(kind=kreal), pointer  ::  sgs_v(:,:)
        real(kind=kreal), pointer  ::  sgs_t(:,:)
!
        real(kind=kreal), pointer  ::  vxe(:,:)
!
        real(kind=kreal), pointer  ::  me_diag(:)
      end type work_finite_element_mat
!
!>      Work array for FEM assemble in MHD model
      type work_MHD_fe_mat
!>        assembled velocity in each element
        real (kind=kreal), allocatable ::  velo_1(:,:)
!>        assembled magnetic field in each element
        real (kind=kreal), allocatable ::  magne_1(:,:)
!>        assembled vewctor potential in each element
        real (kind=kreal), allocatable ::  vecp_1(:,:)
      end type work_MHD_fe_mat
!
!
      type finite_ele_matrices
        type (finite_ele_mat_node) :: f_l
        type (finite_ele_mat_node) :: f_nl
        type (finite_ele_mat_node) :: f_m
        type (finite_ele_mat_node) :: f_t
      end type finite_ele_matrices
!
      type arrays_finite_element_mat
        type(lumped_mass_matrices) ::    m_lump
        type(finite_ele_matrices) ::     fem_rhs
        type(work_finite_element_mat) :: fem_wk
      end type arrays_finite_element_mat
!
      private :: alloc_type_fem_matrices
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_base_type(numnod, numele, nnod_4_ele,    &
     &          np_smp, maxnod_4_smp, rhs_mat)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: numnod, maxnod_4_smp, np_smp
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call alloc_type_fem_mat_work(numele, nnod_4_ele, rhs_mat%fem_wk)
!
      call alloc_type_fem_lumped_mass(numnod, rhs_mat%m_lump)
!
      call alloc_type_fem_matrices(n_vector, numnod, maxnod_4_smp,      &
     &          np_smp, rhs_mat%fem_rhs%f_l)
      call alloc_type_fem_matrices(n_vector, numnod, maxnod_4_smp,      &
     &          np_smp, rhs_mat%fem_rhs%f_nl)
      call alloc_type_fem_matrices(n_vector, numnod, maxnod_4_smp,      &
     &          np_smp, rhs_mat%fem_rhs%f_m)
!
      call alloc_type_fem_matrices(n_sym_tensor, numnod, maxnod_4_smp,  &
     &          np_smp, rhs_mat%fem_rhs%f_t)
!
      end subroutine alloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine alloc_type_fem_mat_work(numele, nnod_4_ele, fem_wk)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      allocate( fem_wk%sk6(numele,n_sym_tensor,nnod_4_ele) )
      allocate( fem_wk%scalar_1(numele) )
      allocate( fem_wk%vector_1(numele,3) )
      allocate( fem_wk%tensor_1(numele,6) )
!
      allocate( fem_wk%sgs_v(numele,3) )
      allocate( fem_wk%sgs_t(numele,6) )
!
      allocate( fem_wk%vxe(numele,3) )
!
      allocate( fem_wk%me_diag(numele) )
!
      if (numele .gt. 0) then
        fem_wk%sk6 = 0.0d0
!
        fem_wk%scalar_1 = 0.0d0
        fem_wk%vector_1 = 0.0d0
        fem_wk%tensor_1 = 0.0d0
!
        fem_wk%sgs_v = 0.0d0
        fem_wk%sgs_t = 0.0d0
!
        fem_wk%vxe = 0.0d0
        fem_wk%me_diag = 0.0d0
      end if
!
      end subroutine alloc_type_fem_mat_work
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_type_fem_lumped_mass(num, lump)
!
      integer(kind = kint), intent(in) :: num
      type(lumped_mass_matrices), intent(inout) :: lump
!
!
      allocate( lump%ml(num) )
      allocate( lump%ml_o(num) )
!
      if (num .gt. 0) then
        lump%ml =   0.0d0
        lump%ml_o = 0.0d0
      end if
!
      end subroutine alloc_type_fem_lumped_mass
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_type_fem_matrices(numdir, numnod, maxnod_4_smp,  &
     &          np_smp, rhs)
!
      integer(kind = kint), intent(in) :: numdir, np_smp
      integer(kind = kint), intent(in) :: numnod, maxnod_4_smp
      type(finite_ele_mat_node), intent(inout) :: rhs
!
!
      allocate( rhs%ff(numnod,numdir) )
      allocate( rhs%ff_smp(maxnod_4_smp,numdir,np_smp) )
!
      if (numnod .gt. 0) then
        rhs%ff =       0.0d0
        rhs%ff_smp =   0.0d0
      end if
!
      end subroutine alloc_type_fem_matrices
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smps_type(numdir, maxnod_4_smp, np_smp, rhs)
!
      integer(kind = kint), intent(in) :: maxnod_4_smp, np_smp
      integer(kind = kint), intent(in) :: numdir
      type(finite_ele_mat_node), intent(inout) :: rhs
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        rhs%ff_smp(1:maxnod_4_smp,1:numdir,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_smps_type
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_type(numdir, numnod, rhs)

      integer(kind = kint), intent(in) :: numdir, numnod
      type(finite_ele_mat_node), intent(inout) :: rhs
!
      integer(kind = kint) :: inod, nd
!
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, numnod
          rhs%ff(inod,nd) = 0.0d0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_ff_type
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_fem_mat_base_type(rhs_mat)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call dealloc_type_fem_mat_work(rhs_mat%fem_wk)
!
      call dealloc_type_fem_lumped_mass(rhs_mat%m_lump)
!
      call dealloc_type_fem_matrices(rhs_mat%fem_rhs%f_l)
      call dealloc_type_fem_matrices(rhs_mat%fem_rhs%f_nl)
      call dealloc_type_fem_matrices(rhs_mat%fem_rhs%f_m)
!
      end subroutine dealloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_type_fem_mat_work(fem_wk)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      deallocate(fem_wk%sk6)
      deallocate(fem_wk%scalar_1, fem_wk%vector_1, fem_wk%tensor_1 )
      deallocate(fem_wk%sgs_v, fem_wk%sgs_t)
      deallocate(fem_wk%vxe, fem_wk%me_diag)
!
      end subroutine dealloc_type_fem_mat_work
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_type_fem_lumped_mass(lump)
!
      type(lumped_mass_matrices), intent(inout) :: lump
!
!
      deallocate( lump%ml )
      deallocate( lump%ml_o )
!
      end subroutine dealloc_type_fem_lumped_mass
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_type_fem_matrices(rhs)
!
      type(finite_ele_mat_node), intent(inout) :: rhs
!
!
      deallocate( rhs%ff )
      deallocate( rhs%ff_smp )
!
      end subroutine dealloc_type_fem_matrices
!
!   ---------------------------------------------------------------------
!
      end module t_finite_element_mat
