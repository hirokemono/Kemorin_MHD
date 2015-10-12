!cal_skv_to_ff_smp_1st.f90
!     module cal_skv_to_ff_smp_1st
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!
!      subroutine reset_sk6(numdir, ele、sk_v)
!
!  -------- Assemble routines from skv to ff_smp for scalar
!      subroutine add1_skv_to_ff_v_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_v_smp)
!      subroutine add1_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_v_smp)
!      subroutine sub1_skv_to_ff_v_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_v_smp)
!      subroutine sub1_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_v_smp)
!
!  -------- Assemble routines from skv to ff_smp for vector
!      subroutine add3_skv_to_ff_v_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_v_smp)
!      subroutine add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_v_smp)
!      subroutine sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_v_smp)
!      subroutine sub3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_v_smp)
!
!  -------- Assemble routines from skv to ff_smp for tensor
!      subroutine add6_skv_to_ff_t_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_t_smp)
!      subroutine add6_skv_coef_to_ff_t_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_t_smp)
!      subroutine sub6_skv_to_ff_t_smp(node, ele, rhs_tbl,              &
!     &          sk_v, ff_t_smp)
!      subroutine sub6_skv_coef_to_ff_t_smp(node, ele, rhs_tbl,         &
!     &          coef, sk_v, ff_t_smp)
!
      module cal_skv_to_ff_smp_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use t_geometry_data
      use t_table_FEM_const
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine reset_sk6(numdir, ele, sk_v)
!
      use cal_skv_to_ff_vector_smp
!
      integer(kind = kint), intent(in) :: numdir
      type(element_data), intent(in) :: ele
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call reset_skv_vector(ele%numele, ele%nnod_4_ele, numdir, sk_v)
!
      end subroutine reset_sk6
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add1_skv_to_ff_v_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_v_smp)
!
      use cal_skv_to_ff_scalar_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call add_skv_scalar_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_v_smp, sk_v)
!
      end subroutine add1_skv_to_ff_v_smp
!
! ----------------------------------------------------------------------
!
      subroutine add1_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_v_smp)
!
      use cal_skv_to_ff_scalar_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call add_skv_scalar_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      end subroutine add1_skv_coef_to_ff_v_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_to_ff_v_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_v_smp)
!
      use cal_skv_to_ff_scalar_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call sub_skv_scalar_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_v_smp, sk_v)
!
      end subroutine sub1_skv_to_ff_v_smp
!
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_v_smp)
!
      use cal_skv_to_ff_scalar_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call sub_skv_scalar_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      end subroutine sub1_skv_coef_to_ff_v_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add3_skv_to_ff_v_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_v_smp)
!
      use cal_skv_to_ff_vector_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call add_skv_vector_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_v_smp, sk_v)
!
      end subroutine add3_skv_to_ff_v_smp
!
! ----------------------------------------------------------------------
!
      subroutine add3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_v_smp)
!
      use cal_skv_to_ff_vector_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call add_skv_vector_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      end subroutine add3_skv_coef_to_ff_v_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_v_smp)
!
      use cal_skv_to_ff_vector_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call sub_skv_vector_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_v_smp, sk_v)
!
      end subroutine sub3_skv_to_ff_v_smp
!
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_coef_to_ff_v_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_v_smp)
!
      use cal_skv_to_ff_vector_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(node%max_nod_smp,n_vector,np_smp)
!
!
      call sub_skv_vector_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      end subroutine sub3_skv_coef_to_ff_v_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add6_skv_to_ff_t_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_t_smp)
!
      use cal_skv_to_ff_tensor_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(node%max_nod_smp,n_sym_tensor,np_smp)
!
!
      call add_skv_tensor_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_t_smp, sk_v)
!
      end subroutine add6_skv_to_ff_t_smp
!
! ----------------------------------------------------------------------
!
      subroutine add6_skv_coef_to_ff_t_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_t_smp)
!
      use cal_skv_to_ff_tensor_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(node%max_nod_smp,n_sym_tensor,np_smp)
!
!
      call add_skv_tensor_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_t_smp, sk_v)
!
      end subroutine add6_skv_coef_to_ff_t_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_to_ff_t_smp(node, ele, rhs_tbl,               &
     &          sk_v, ff_t_smp)
!
      use cal_skv_to_ff_tensor_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(node%max_nod_smp,n_sym_tensor,np_smp)
!
!
      call sub_skv_tensor_2_ff_smp                                      &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, ff_t_smp, sk_v)
!
      end subroutine sub6_skv_to_ff_t_smp
!
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_coef_to_ff_t_smp(node, ele, rhs_tbl,          &
     &          coef, sk_v, ff_t_smp)
!
      use cal_skv_to_ff_tensor_smp
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(node%max_nod_smp,n_sym_tensor,np_smp)
!
!
      call sub_skv_tensor_coef_2_ff_smp                                 &
     &   (ele%numele, ele%nnod_4_ele, np_smp, node%max_nod_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, coef, ff_t_smp, sk_v)
!
      end subroutine sub6_skv_coef_to_ff_t_smp
!
! ----------------------------------------------------------------------
!
      end module cal_skv_to_ff_smp_1st
