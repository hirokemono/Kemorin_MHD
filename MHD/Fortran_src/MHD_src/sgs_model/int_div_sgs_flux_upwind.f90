!
!      module int_div_sgs_flux_upwind
!
!     Written by H. Matsui on July, 2005
!     modified by H. Matsui on July, 2007
!
!!      subroutine int_div_sgs_mf_simi_upwind(i_flux, i_vect, num_int,  &
!!     &          node, ele, fluid, nod_fld, jac_3d, rhs_tbl,           &
!!     &          ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!!      subroutine int_div_sgs_sf_simi_upw                              &
!!     &         (i_flux, i_vect, i_scalar, num_int,                    &
!!     &          node, ele, fluid, nod_fld, jac_3d, rhs_tbl,           &
!!     &          ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!!      subroutine int_div_sgs_idct_simi_upw(i_flux, i_v, i_b, num_int, &
!!     &          node, ele, conduct, nod_fld, jac_3d, rhs_tbl,         &
!!     &          ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      module int_div_sgs_flux_upwind
!
      use m_precision
!
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_div_sgs_mf_simi_upwind(i_flux, i_vect, num_int,    &
     &          node, ele, fluid, nod_fld, jac_3d, rhs_tbl,             &
     &          ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_vect_diff_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call SGS_m_flux_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_vect, i_flux, nod_fld%d_fld, fem_wk%tensor_1)
        call fem_skv_div_tsr_upw(fluid%istack_ele_fld_smp, num_int, k2, &
     &      d_ele(1,ie_upw), ele, jac_3d, fem_wk%tensor_1,              &
     &      fem_wk%sk6)
      end do
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_div_sgs_mf_simi_upwind
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_sf_simi_upw                                &
     &         (i_flux, i_vect, i_scalar, num_int,                      &
     &          node, ele, fluid, nod_fld, jac_3d, rhs_tbl,             &
     &          ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_vect_diff_upw_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call SGS_v_flux_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_vect, i_scalar, i_flux, nod_fld%d_fld, fem_wk%vector_1)
        call fem_skv_divergence_upw(fluid%istack_ele_fld_smp,           &
     &      num_int, k2, d_ele(1,iele_velo), ele, jac_3d,               &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_div_sgs_sf_simi_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_div_sgs_idct_simi_upw(i_flux, i_v, i_b, num_int,   &
     &          node, ele, conduct, nod_fld, jac_3d, rhs_tbl,           &
     &          ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
      use fem_skv_vect_diff_upw_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
        call SGS_induct_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_b, i_v, i_flux, nod_fld%d_fld, fem_wk%vector_1)
        call fem_skv_div_as_tsr_upw(conduct%istack_ele_fld_smp,         &
     &      num_int, k2, d_ele(1,iele_velo), ele, jac_3d,               &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_div_sgs_idct_simi_upw
!
! ----------------------------------------------------------------------
!
      end module int_div_sgs_flux_upwind
