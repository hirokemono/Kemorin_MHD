!
!     module int_vol_thermal_ele
!
!     Written by H. Matsui on Aug., 2005
!
!!      subroutine int_vol_temp_ele                                     &
!!     &         (iflug_SGS_term, iflag_commute, ifilter_final, num_int,&
!!     &          i_field, i_velo, i_SGS_flux, iak_diff_flux,           &
!!     &          node, ele, fluid, property, nod_fld, jac_3d, rhs_tbl, &
!!     &          FEM_elens, diff_coefs, ncomp_ele, iele_velo, d_ele,   &
!!     &          iak_diff_hf, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_temp_ele_upw                                 &
!!     &         (iflug_SGS_term, iflag_commute, ifilter_final, num_int,&
!!     &          dt, i_field, i_velo, i_SGS_flux, iak_diff_flux,       &
!!     &          node, ele, fluid, property, nod_fld, jac_3d, rhs_tbl, &
!!     &          FEM_elens, diff_coefs, ncomp_ele, iele_velo, d_ele,   &
!!     &          mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_thermal_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
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
      subroutine int_vol_temp_ele                                       &
     &         (iflug_SGS_term, iflag_commute, ifilter_final, num_int,  &
     &          i_field, i_velo, i_SGS_flux, iak_diff_flux,             &
     &          node, ele, fluid, property, nod_fld, jac_3d, rhs_tbl,   &
     &          FEM_elens, diff_coefs, ncomp_ele, iele_velo, d_ele,     &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_type
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: iflug_SGS_term
      integer(kind = kint), intent(in) :: iflag_commute
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_field, i_velo, i_SGS_flux
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      integer(kind=kint), intent(in) :: iak_diff_flux
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!
      if (property%coef_nega_adv .eq. 0.0d0 ) return
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, property%coef_nega_adv, fem_wk%scalar_1)
!
        if(iflug_SGS_term .ne. id_SGS_none                              &
     &    .and. iflag_commute .eq. id_SGS_commute_ON) then
          call SGS_const_vector_each_ele(node, ele, nod_fld,            &
     &        k2, i_velo, i_field, i_SGS_flux,                          &
     &        property%coef_nega_adv, mhd_fem_wk%sgs_v1,                &
     &        fem_wk%vector_1)
          call fem_skv_scl_inertia_modsgs_pg                            &
     &       (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final,     &
     &        diff_coefs%num_field, iak_diff_flux, diff_coefs%ak,       &
     &        ele, g_FEM1, jac_3d, FEM_elens, fem_wk%scalar_1,          &
     &        mhd_fem_wk%sgs_v1, fem_wk%vector_1, d_ele(1,iele_velo),   &
     &        fem_wk%sk6)
        else if(iflug_SGS_term .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        i_SGS_flux, property%coef_nega_adv,                       &
     &        mhd_fem_wk%sgs_v1)
          call fem_skv_scl_inertia_sgs_pg(fluid%istack_ele_fld_smp,     &
     &        num_int, k2, ele, g_FEM1, jac_3d,                         &
     &        fem_wk%scalar_1, mhd_fem_wk%sgs_v1, d_ele(1,iele_velo),   &
     &        fem_wk%sk6)
        else
          call fem_skv_scalar_inertia_type(fluid%istack_ele_fld_smp,    &
     &        num_int, k2, fem_wk%scalar_1, d_ele(1,iele_velo),         &
     &        ele, g_FEM1, jac_3d, fem_wk%sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_temp_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_temp_ele_upw                                   &
     &         (iflug_SGS_term, iflag_commute, ifilter_final, num_int,  &
     &          dt, i_field, i_velo, i_SGS_flux, iak_diff_flux,         &
     &          node, ele, fluid, property, nod_fld, jac_3d, rhs_tbl,   &
     &          FEM_elens, diff_coefs, ncomp_ele, iele_velo, d_ele,     &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_upwind
      use fem_skv_div_sgs_flux_upw
!
      integer(kind = kint), intent(in) :: iflug_SGS_term
      integer(kind = kint), intent(in) :: iflag_commute
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_field, i_velo, i_SGS_flux
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind=kint), intent(in) :: iak_diff_flux
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!
      if (property%coef_nega_adv .eq. 0.0d0 ) return
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, property%coef_nega_adv, fem_wk%scalar_1)
!
        if(iflug_SGS_term .ne. id_SGS_none                              &
     &    .and. iflag_commute .eq. id_SGS_commute_ON) then
          call SGS_const_vector_each_ele(node, ele, nod_fld,            &
     &        k2, i_velo, i_field, i_SGS_flux,                          &
     &        property%coef_nega_adv, mhd_fem_wk%sgs_v1,                &
     &        fem_wk%vector_1)
          call fem_skv_scl_inertia_msgs_upw                             &
     &       (fluid%istack_ele_fld_smp, num_int, k2, ifilter_final, dt, &
     &        diff_coefs%num_field, iak_diff_flux, diff_coefs%ak,       &
     &        ele, g_FEM1, jac_3d, FEM_elens, fem_wk%scalar_1,          &
     &        mhd_fem_wk%sgs_v1, fem_wk%vector_1, d_ele(1,iele_velo),   &
     &        d_ele(1,iele_velo), fem_wk%sk6)
        else if(iflug_SGS_term .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        i_SGS_flux, property%coef_nega_adv, mhd_fem_wk%sgs_v1)
          call fem_skv_scl_inertia_sgs_upwind                           &
     &       (fluid%istack_ele_fld_smp, num_int, k2, dt,                &
     &        ele, g_FEM1, jac_3d, fem_wk%scalar_1, mhd_fem_wk%sgs_v1,  &
     &        d_ele(1,iele_velo), d_ele(1,iele_velo), fem_wk%sk6)
        else
          call fem_skv_scalar_inertia_upwind(fluid%istack_ele_fld_smp,  &
     &       num_int, k2, dt, fem_wk%scalar_1, d_ele(1,iele_velo),      &
     &       d_ele(1,iele_velo), ele, g_FEM1, jac_3d, fem_wk%sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_temp_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_thermal_ele
