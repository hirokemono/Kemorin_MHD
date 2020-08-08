!t_finite_element_mat.f90
!     module t_finite_element_mat
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler
!!
!!@verbatim
!!      subroutine alloc_type_fem_mat_work(ele, fem_wk)
!!        type(element_data), intent(in) :: ele
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!      subroutine alloc_type_fem_lumped_mass(node, lump)
!!        type(node_data), intent(in) :: node
!!        type(lumped_mass_matrices), intent(inout) :: lump
!!      subroutine alloc_type_fem_matrices(numdir, node, rhs)
!!        type(finite_ele_mat_node), intent(inout) :: rhs
!!
!!      subroutine reset_ff_smp(numdir, node, rhs)
!!      subroutine reset_ff(numdir, node, rhs)
!!        integer(kind = kint), intent(in) :: numdir
!!        type(node_data), intent(in) :: node
!!        type(finite_ele_mat_node), intent(inout) :: rhs
!!
!!      subroutine dealloc_type_fem_mat_work(fem_wk)
!!      subroutine dealloc_type_fem_lumped_mass(lump)
!!      subroutine dealloc_type_fem_matrices(rhs)
!!
!!      subroutine reset_ff_smps(node, f_l, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(finite_ele_mat_node), intent(inout) :: ffs
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!
!!      subroutine check_mass_martix(id_rank, node, lump)
!!        type(lumped_mass_matrices), intent(inout) :: lump
!!      subroutine check_ff(id_rank, numdir, node, ffs)
!!      subroutine check_ff_smp(id_rank, numdir, node, ffs)
!!        type(node_data), intent(in) :: node
!!        type(finite_ele_mat_node), intent(inout) :: ffs
!!      subroutine check_sk6(id_rank, ele, fem_wk)
!!        type(element_data), intent(in) :: ele
!!        type(work_finite_element_mat), intent(in) :: fem_wk
!!@endverbatim
!
!
      module t_finite_element_mat
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
      use t_geometry_data
!
      implicit  none
!
!
      type lumped_mass_matrices
        real (kind=kreal), allocatable  ::  ml(:)
        real (kind=kreal), allocatable  ::  ml_o(:)
      end type lumped_mass_matrices
!
      type finite_ele_mat_node
        real (kind=kreal), allocatable  :: ff(:,:)
        real (kind=kreal), allocatable  :: ff_smp(:,:,:)
      end type finite_ele_mat_node
!
!
      type work_finite_element_mat
        real (kind=kreal), allocatable  ::  sk6(:,:,:)
!
        real(kind=kreal), allocatable  ::  scalar_1(:)
        real(kind=kreal), allocatable  ::  vector_1(:,:)
        real(kind=kreal), allocatable  ::  tensor_1(:,:)
!
        real(kind=kreal), allocatable  ::  vxe(:,:)
!
        real(kind=kreal), allocatable  ::  me_diag(:)
      end type work_finite_element_mat
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_type_fem_mat_work(ele, fem_wk)
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
      subroutine alloc_type_fem_lumped_mass(node, lump)
!
      type(node_data), intent(in) :: node
      type(lumped_mass_matrices), intent(inout) :: lump
!
!
      allocate( lump%ml(node%numnod) )
      allocate( lump%ml_o(node%numnod) )
!
      if(node%numnod .gt. 0) then
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
      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: rhs
!
!
      allocate( rhs%ff(node%numnod,numdir) )
      allocate( rhs%ff_smp(node%max_nod_smp,numdir,np_smp) )
!
      if (node%numnod .eq. 0) return
      call reset_ff(numdir, node, rhs)
      call reset_ff_smp(numdir, node, rhs)
!
      end subroutine alloc_type_fem_matrices
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smp(numdir, node, rhs)
!
      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: rhs
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        rhs%ff_smp(1:node%max_nod_smp,1:numdir,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff(numdir, node, rhs)

      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: rhs
!
      integer(kind = kint) :: nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp workshare
        rhs%ff(1:node%numnod,nd) = 0.0d0
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine reset_ff
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
      subroutine reset_ff_smps(node, f_l, f_nl)
!
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      call reset_ff_smp(n_vector, node, f_l)
      call reset_ff_smp(n_vector, node, f_nl)
!
      end subroutine reset_ff_smps
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix(id_rank, node, lump)
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(lumped_mass_matrices), intent(inout) :: lump
!
      integer(kind = kint) :: inod
!
      write(50+id_rank,*) 'inod, ml, ml_o'
      do inod = 1, node%numnod
        write(50+id_rank,'(i16,1p2e25.14)')                             &
     &        inod, lump%ml(inod), lump%ml_o(inod)
      end do
!
      end subroutine check_mass_martix
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff(id_rank, numdir, node, ffs)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: ffs
!
      integer(kind = kint) :: inod, nd
!
      write(50+id_rank,*) 'inod, ff', numdir
      do inod = 1, node%numnod
        write(50+id_rank,'(i16,1p10e25.14)')                            &
     &         inod, (ffs%ff(inod,nd),nd=1, numdir)
      end do
!
      end subroutine check_ff
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_smp(id_rank, numdir, node, ffs)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numdir
      type(node_data), intent(in) :: node
      type(finite_ele_mat_node), intent(inout) :: ffs
!
      integer(kind = kint) :: ip, inod, nd
!
      write(50+id_rank,*) 'ip, inod, ff_smp', numdir
      do ip = 1, np_smp
        do inod = 1, node%max_nod_smp
          write(50+id_rank,'(2i16,1p10e25.14)')                         &
     &         ip, inod, (ffs%ff_smp(inod,nd,ip),nd=1, numdir)
        end do
      end do
!
      end subroutine check_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine check_sk6(id_rank, ele, fem_wk)
!
      integer, intent(in) :: id_rank
      type(element_data), intent(in) :: ele
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      integer(kind = kint) :: iele, k1
!
      write(50+id_rank,*) 'k1, iele, sk6'
      do k1 = 1, ele%nnod_4_ele
        do iele = 1, ele%numele
          write(50+id_rank,'(2i16,1p10e25.14)')                         &
     &         k1, iele, fem_wk%sk6(iele,1:6,k1)
        end do
      end do
!
      end subroutine check_sk6
!
!   ---------------------------------------------------------------------
!
      end module t_finite_element_mat
