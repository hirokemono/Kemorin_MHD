!int_vol_similarity_uxb.f90
!     module int_vol_similarity_uxb
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!!      subroutine int_simi_vp_induct(num_int, icomp_sgs_uxb,           &
!!     &          node, ele, conduct, iphys, nod_fld, jac_3d,           &
!!     &          rhs_tbl, sgs_coefs, fem_wk, f_nl)
!!      subroutine int_simi_vp_induct_upm(num_int, dt, icomp_sgs_uxb,   &
!!     &          node, ele, conduct, iphys, nod_fld, jac_3d, rhs_tbl,  &
!!     &          sgs_coefs, ncomp_ele, iele_magne, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_similarity_uxb
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct(num_int, icomp_sgs_uxb,             &
     &          node, ele, conduct, iphys, nod_fld, jac_3d,             &
     &          rhs_tbl, sgs_coefs, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_field_type
      use cal_products_within_skv
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: icomp_sgs_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys%i_sgs_simi, fem_wk%vector_1)
        call fem_skv_vector_type(conduct%istack_ele_fld_smp,            &
     &      num_int, k2, ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
        call scalar_prod_to_tensor_skv                                  &
     &     (ele, conduct%istack_ele_fld_smp, sgs_coefs%ntot_comp,       &
     &      icomp_sgs_uxb, sgs_coefs%ak, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_simi_vp_induct
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct_upm(num_int, dt, icomp_sgs_uxb,     &
     &          node, ele, conduct, iphys, nod_fld, jac_3d, rhs_tbl,    &
     &          sgs_coefs, ncomp_ele, iele_magne, d_ele, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_fld_upwind
      use cal_products_within_skv
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: icomp_sgs_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys%i_sgs_simi, fem_wk%vector_1)
!
        call fem_skv_vector_field_upwind(conduct%istack_ele_fld_smp,    &
     &      num_int, k2, dt, d_ele(1,iele_magne), ele, jac_3d,          &
     &      fem_wk%vector_1, fem_wk%sk6)
!
        call scalar_prod_to_tensor_skv(ele, conduct%istack_ele_fld_smp, &
     &      sgs_coefs%ntot_comp, icomp_sgs_uxb, sgs_coefs%ak,           &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_similarity_uxb
