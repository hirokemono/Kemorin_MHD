!
!     module int_vol_light_comp_ele
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_composition_ele                              &
!!     &         (node, ele, fluid, iphys, nod_fld, jac_3d,             &
!!     &          rhs_tbl, ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!!      subroutine int_vol_composition_ele_upw                          &
!!     &          (node, ele, fluid, iphys, nod_fld, jac_3d,            &
!!     &           rhs_tbl, ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_light_comp_ele
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_physical_property
!
!      use m_SGS_model_coefs
!      use m_SGS_address
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
!      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_composition_ele                                &
     &         (node, ele, fluid, iphys, nod_fld, jac_3d,               &
     &          rhs_tbl, ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
!      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_type
      use fem_skv_div_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_c .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, iphys%i_light, coef_nega_c, fem_wk%scalar_1)
!
!        if(iflag_SGS_comp_flux .ne. id_SGS_none                        &
!     &    .and. iflag_commute_c_flux .eq. id_SGS_commute_ON) then
!          call  SGS_const_vector_each_ele(node, ele, nod_fld,          &
!     &         k2, iphys%i_velo, iphys%i_light, iphys%i_SGS_c_flux,    &
!     &         coef_nega_c, mhd_fem1_wk%sgs_v1, fem_wk%vector_1)
!          call fem_skv_scl_inertia_modsgs_pg(fluid%istack_ele_fld_smp,&
!     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_cf),      &
!     &        ele, jac_3d, FEM_elens, fem_wk%scalar_1,                 &
!     &        mhd_fem1_wk%sgs_v1, fem_wk%vector_1,                     &
!     &        d_ele(1,iele_velo), fem_wk%sk6)
!        else if(iflag_SGS_comp_flux .ne. id_SGS_none) then
!          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,      &
!     &        iphys%i_SGS_c_flux, coef_nega_c, mhd_fem1_wk%sgs_v1)
!          call fem_skv_scl_inertia_sgs_pg(fluid%istack_ele_fld_smp,    &
!     &        num_int, k2, ele, jac_3d,                                &
!     &        fem_wk%scalar_1, mhd_fem1_wk%sgs_v1,                     &
!     &        d_ele(1,iele_velo), fem_wk%sk6)
!        else
          call fem_skv_scalar_inertia_type(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, fem_wk%scalar_1, d_ele(1,iele_velo),         &
     &        ele, jac_3d, fem_wk%sk6)
!        end if
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_composition_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_composition_ele_upw                            &
     &          (node, ele, fluid, iphys, nod_fld, jac_3d,              &
     &           rhs_tbl, ncomp_ele, iele_velo, d_ele, fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
!      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_upw_type
      use fem_skv_div_sgs_flux_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_c .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, iphys%i_light, coef_nega_c, fem_wk%scalar_1)
!
!        if(iflag_SGS_comp_flux .ne. id_SGS_none                        &
!     &    .and. iflag_commute_c_flux .eq. id_SGS_commute_ON) then
!          call SGS_const_vector_each_ele(node, ele, nod_fld,           &
!     &        k2, iphys%i_velo, iphys%i_light, iphys%i_SGS_c_flux,     &
!     &        coef_nega_c, mhd_fem1_wk%sgs_v1, fem_wk%vector_1)
!          call fem_skv_scl_inertia_msgs_upw(fluid%istack_ele_fld_smp,  &
!     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_cf),      &
!     &        ele, jac_3d, FEM_elens, fem_wk%scalar_1,                 &
!     &        mhd_fem1_wk%sgs_v1, fem_wk%vector_1,                     &
!     &        d_ele(1,iele_velo), d_ele(1,iele_velo), fem_wk%sk6)
!        else if(iflag_SGS_comp_flux .ne. id_SGS_none) then
!          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,      &
!     &        iphys%i_SGS_c_flux, coef_nega_c, mhd_fem1_wk%sgs_v1)
!          call fem_skv_scl_inertia_sgs_upwind                          &
!     &       (fluid%istack_ele_fld_smp, num_int, k2,                   &
!     &       ele, jac_3d, fem_wk%scalar_1, mhd_fem1_wk%sgs_v1,         &
!     &       d_ele(1,iele_velo), d_ele(1,iele_velo), fem_wk%sk6)
!        else
          call fem_skv_scalar_inertia_upwind(fluid%istack_ele_fld_smp,  &
     &       num_int, k2, fem_wk%scalar_1, d_ele(1,iele_velo),          &
     &       d_ele(1,iele_velo), ele, jac_3d, fem_wk%sk6)
!        end if
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_composition_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_light_comp_ele
