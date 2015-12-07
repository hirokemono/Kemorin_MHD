!t_finite_element_mat.f90
!     module t_finite_element_mat
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler
!
!      subroutine alloc_fem_mat_base_type(node, ele, rhs_mat)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!      subroutine alloc_finite_elem_mat                                 &
!     &         (node, ele, m_lump, fem_wk, f_l, f_nl)
!
!
!      subroutine alloc_type_fem_mat_work(ele, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!      subroutine alloc_type_fem_lumped_mass(num, lump)
!        integer(kind = kint), intent(in) :: num
!        type(lumped_mass_matrices), intent(inout) :: lump
!      subroutine alloc_type_fem_matrices(numdir, node, rhs)
!        type(finite_ele_mat_node), intent(inout) :: rhs
!
!      subroutine reset_ff_smps_type(numdir, max_nod_smp, rhs)
!      subroutine reset_ff_type(numdir, numnod, rhs)
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: numdir, numnod
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!      subroutine dealloc_fem_mat_base_type(rhs_mat)
!      subroutine dealloc_finite_elem_mat(m_lump, fem_wk, f_l, f_nl)
!
!      subroutine dealloc_type_fem_mat_work(fem_wk)
!      subroutine dealloc_type_fem_lumped_mass(lump)
!      subroutine dealloc_type_fem_matrices(rhs)
!
!      subroutine reset_ff(numnod, ffs)
!      subroutine reset_ff_smp(max_nod_smp, ffs)
!        type(finite_ele_mat_node), intent(inout) :: ffs
!      subroutine reset_ff_smps(max_nod_smp, f_l, f_nl)
!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!      subroutine check_mass_martix(my_rank, numnod, lump)
!        type(lumped_mass_matrices), intent(inout) :: lump
!      subroutine check_ff(my_rank, numdir, numnod, ffs)
!      subroutine check_ff_smp(my_rank, numdir, max_nod_smp, ffs)
!        type(finite_ele_mat_node), intent(inout) :: ffs
!      subroutine check_sk6(my_rank, ele, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(work_finite_element_mat), intent(in) :: fem_wk
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
        real(kind=kreal), pointer  ::  vxe(:,:)
!
        real(kind=kreal), pointer  ::  me_diag(:)
      end type work_finite_element_mat
!
!
      type finite_ele_matrices
        type(finite_ele_mat_node) :: f_l
        type(finite_ele_mat_node) :: f_nl
      end type finite_ele_matrices
!
      type arrays_finite_element_mat
        type(lumped_mass_matrices) ::    m_lump
        type(finite_ele_matrices) ::     fem_rhs
        type(work_finite_element_mat) :: fem_wk
      end type arrays_finite_element_mat
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_base_type(node, ele, rhs_mat)
!
      use t_geometry_data
      use m_phys_constants
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call alloc_finite_elem_mat(node, ele, rhs_mat%m_lump,             &
     &    rhs_mat%fem_wk, rhs_mat%fem_rhs%f_l, rhs_mat%fem_rhs%f_nl)
!
      end subroutine alloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_finite_elem_mat                                  &
     &         (node, ele, m_lump, fem_wk, f_l, f_nl)
!
      use t_geometry_data
      use m_phys_constants
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(lumped_mass_matrices), intent(inout) :: m_lump
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call alloc_type_fem_mat_work(ele, fem_wk)
      call alloc_type_fem_lumped_mass(node%numnod, m_lump)
!
      call alloc_type_fem_matrices(n_vector, node, f_l)
      call alloc_type_fem_matrices(n_vector, node, f_nl)
!
      end subroutine alloc_finite_elem_mat
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine alloc_type_fem_mat_work(ele, fem_wk)
!
      use t_geometry_data
      use m_phys_constants
!
      type(element_data), intent(in) :: ele
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      allocate( fem_wk%sk6(ele%numele,n_sym_tensor,ele%nnod_4_ele))
      allocate( fem_wk%scalar_1(ele%numele) )
      allocate( fem_wk%vector_1(ele%numele,n_vector) )
      allocate( fem_wk%tensor_1(ele%numele,n_sym_tensor) )
!
      allocate( fem_wk%me_diag(ele%numele) )
!
      if (ele%numele .gt. 0) then
        fem_wk%sk6 = 0.0d0
!
        fem_wk%scalar_1 = 0.0d0
        fem_wk%vector_1 = 0.0d0
        fem_wk%tensor_1 = 0.0d0
!
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
      subroutine alloc_type_fem_matrices(numdir, node, rhs)
!
      use m_machine_parameter
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: rhs
!
!
      allocate( rhs%ff(node%numnod,numdir) )
      allocate( rhs%ff_smp(node%max_nod_smp,numdir,np_smp) )
!
      if (node%numnod .eq. 0) return
      call reset_ff_type(numdir, node%numnod, rhs)
      call reset_ff_smps_type(numdir, node%max_nod_smp, rhs)
!
      end subroutine alloc_type_fem_matrices
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smps_type(numdir, max_nod_smp, rhs)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: max_nod_smp
      integer(kind = kint), intent(in) :: numdir
      type(finite_ele_mat_node), intent(inout) :: rhs
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        rhs%ff_smp(1:max_nod_smp,1:numdir,ip) =   0.0d0
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
      call dealloc_finite_elem_mat(rhs_mat%m_lump, rhs_mat%fem_wk,      &
     &    rhs_mat%fem_rhs%f_l, rhs_mat%fem_rhs%f_nl)
!
      end subroutine dealloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_finite_elem_mat(m_lump, fem_wk, f_l, f_nl)
!
      type(lumped_mass_matrices), intent(inout) :: m_lump
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call dealloc_type_fem_mat_work(fem_wk)
!
      call dealloc_type_fem_lumped_mass(m_lump)
!
      call dealloc_type_fem_matrices(f_l)
      call dealloc_type_fem_matrices(f_nl)
!
      end subroutine dealloc_finite_elem_mat
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
      deallocate(fem_wk%me_diag)
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
      deallocate( lump%ml, lump%ml_o )
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
!   ---------------------------------------------------------------------
!
      subroutine reset_ff(numnod, ffs)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod
      type(finite_ele_mat_node), intent(inout) :: ffs
!
!
      call reset_ff_type(n_vector, numnod, ffs)
!
      end subroutine reset_ff
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smp(max_nod_smp, ffs)
!
      use m_phys_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: max_nod_smp
      type(finite_ele_mat_node), intent(inout) :: ffs
!
!
      call reset_ff_smps_type(n_vector, max_nod_smp, ffs)
!
      end subroutine reset_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smps(max_nod_smp, f_l, f_nl)
!
      integer(kind = kint), intent(in) :: max_nod_smp
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call reset_ff_smp(max_nod_smp, f_l)
      call reset_ff_smp(max_nod_smp, f_nl)
!
      end subroutine reset_ff_smps
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix(my_rank, numnod, lump)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(lumped_mass_matrices), intent(inout) :: lump
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml, ml_o'
      do inod = 1, numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &        inod, lump%ml(inod), lump%ml_o(inod)
      end do
!
      end subroutine check_mass_martix
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff(my_rank, numdir, numnod, ffs)
!
      integer(kind = kint), intent(in) :: my_rank, numdir, numnod
      type(finite_ele_mat_node), intent(inout) :: ffs
!
      integer(kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'inod, ff', numdir
      do inod = 1, numnod
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         inod, (ffs%ff(inod,nd),nd=1, numdir)
      end do
!
      end subroutine check_ff
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_smp(my_rank, numdir, max_nod_smp, ffs)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank, numdir, max_nod_smp
      type(finite_ele_mat_node), intent(inout) :: ffs
!
      integer(kind = kint) :: ip, inod, nd
!
      write(50+my_rank,*) 'ip, inod, ff_smp', numdir
      do ip = 1, np_smp
        do inod = 1, max_nod_smp
          write(50+my_rank,'(2i16,1p10e25.14)')                         &
     &         ip, inod, (ffs%ff_smp(inod,nd,ip),nd=1, numdir)
        end do
      end do
!
      end subroutine check_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine check_sk6(my_rank, ele, fem_wk)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(element_data), intent(in) :: ele
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      integer(kind = kint) :: iele, k1
!
      write(50+my_rank,*) 'k1, iele, sk6'
      do k1 = 1, ele%nnod_4_ele
        do iele = 1, ele%numele
          write(50+my_rank,'(2i16,1p10e25.14)')                         &
     &         k1, iele, fem_wk%sk6(iele,1:6,k1)
        end do
      end do
!
      end subroutine check_sk6
!
!   ---------------------------------------------------------------------
!
      end module t_finite_element_mat
