!
!      module int_vol_multi_pass
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!
!!      subroutine int_vol_multi_pass_scalar(num_int, iele_fsmp_stack,  &
!!     &          node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
!!      subroutine int_vol_multi_pass_vector(num_int, iele_fsmp_stack,  &
!!     &          node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
!!
!!      subroutine int_vol_multi_pass_scalar_upw                        &
!!     &         (num_int, iele_fsmp_stack, node, ele, jac_3d, rhs_tbl, &
!!     &          ncomp_ele, ie_up, d_ele, ff_m_smp, fem_wk, f_nl)
!!      subroutine int_vol_multi_pass_vector_upw                        &
!!     &         (num_int, iele_fsmp_stack, node, ele, jac_3d, rhs_tbl, &
!!     &          ncomp_ele, ie_up, d_ele, ff_m_smp, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_multi_pass
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use m_t_step_parameter
!
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_scalar(num_int, iele_fsmp_stack,    &
     &          node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_field_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_2_each_element(node, ele,                           &
     &      k2, f_nl%ff(1:node%numnod,1), fem_wk%scalar_1)
        call fem_skv_scalar_type(iele_fsmp_stack, num_int, k2,          &
     &      ele, jac_3d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call sub1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node%max_nod_smp, node%istack_nod_smp,                        &
     &    n_scalar, f_nl%ff_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_scalar
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_vector(num_int, iele_fsmp_stack,    &
     &          node, ele, jac_3d, rhs_tbl, ff_m_smp, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_field_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_2_each_element(node, ele,                           &
     &      k2, f_nl%ff, fem_wk%vector_1)
        call fem_skv_vector_type(iele_fsmp_stack, num_int, k2,          &
     &      ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node%max_nod_smp, node%istack_nod_smp,                        &
     &    n_vector, f_nl%ff_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_scalar_upw                          &
     &         (num_int, iele_fsmp_stack, node, ele, jac_3d, rhs_tbl,   &
     &          ncomp_ele, ie_up, d_ele, ff_m_smp, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_fld_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_ele, ie_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_2_each_element(node, ele,                           &
     &      k2, f_nl%ff(1:node%numnod,1), fem_wk%scalar_1)
        call fem_skv_scalar_field_upwind                                &
     &      (iele_fsmp_stack, num_int, k2, dt, d_ele(1,ie_up),          &
     &       ele, jac_3d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call sub1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node%max_nod_smp, node%istack_nod_smp,                        &
     &    n_scalar, f_nl%ff_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_scalar_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_multi_pass_vector_upw                          &
     &         (num_int, iele_fsmp_stack, node, ele, jac_3d, rhs_tbl,   &
     &          ncomp_ele, ie_up, d_ele, ff_m_smp, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use cal_for_ffs
      use fem_skv_nodal_fld_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_ele, ie_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: ff_m_smp(node%max_nod_smp,3,np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_2_each_element(node, ele,                           &
     &      k2, f_nl%ff, fem_wk%vector_1)
        call fem_skv_vector_field_upwind                                &
     &     (iele_fsmp_stack, num_int, k2, dt, d_ele(1,ie_up),           &
     &      ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
      call cal_multi_pass_2_ff_smp                                      &
     &   (node%max_nod_smp, node%istack_nod_smp,                        &
     &    n_vector, f_nl%ff_smp, ff_m_smp)
!
      end subroutine int_vol_multi_pass_vector_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_multi_pass
