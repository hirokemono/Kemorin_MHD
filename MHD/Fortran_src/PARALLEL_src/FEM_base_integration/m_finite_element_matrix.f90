!
!     module m_finite_element_matrix
!.......................................................................
!
!     Written by H. Matsui and H. Okuda on Jul. 2000
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine allocate_finite_elem_mt
!
!      subroutine reset_ff_smps
!      subroutine reset_ff(numnod)
!
!      subroutine deallocate_finite_elem_mt
!      subroutine deallocate_node_ff
!
      module m_finite_element_matrix
!
      use m_precision
      use t_finite_element_mat
!
      implicit  none
!
!>      Work area for FEM assemble
      type(work_finite_element_mat), save :: fem1_wk
!
      type(finite_ele_mat_node), save :: f1_l
!
      type(finite_ele_mat_node), save :: f1_nl
!
      real (kind=kreal), allocatable  ::  ml(:)
      real (kind=kreal), allocatable  ::  ml_o(:)
!
!      real (kind=kreal), allocatable  ::  ff(:,:)
!      real (kind=kreal), allocatable  ::  ff_nl(:,:)
! 
!      real (kind=kreal), allocatable  :: ff_smp(:,:,:)
!      real (kind=kreal), allocatable  :: ff_nl_smp(:,:,:)
!f1_nl%ff_smp
!      real (kind=kreal), allocatable  :: ff_m_smp(:,:,:)
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_finite_elem_mt
!
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
!
!
      call alloc_type_fem_matrices(n_vector, node1, f1_l)
      call alloc_type_fem_matrices(n_vector, node1, f1_nl)
!
      allocate(ml(node1%numnod))
      allocate(ml_o(node1%numnod))
!
      call alloc_type_fem_mat_work(ele1, fem1_wk)
!
      if(node1%numnod .gt. 0) then
        ml =  0.0d0
        ml_o =0.0d0
      end if
!
      end subroutine allocate_finite_elem_mt
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_finite_elem_mt
!
      deallocate(ml, ml_o)
!
      call dealloc_type_fem_mat_work(fem1_wk)
      call dealloc_type_fem_matrices(f1_nl)
      call dealloc_type_fem_matrices(f1_l)
!
      end subroutine deallocate_finite_elem_mt
!
!   ---------------------------------------------------------------------
!
      end module m_finite_element_matrix
