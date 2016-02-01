!
!     module int_vol_temp_monitor
!
!     numerical integration for finite elememt equations of heat
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_ene_monitor(i_field, iak_diff_hf,            &
!!     &          node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld, &
!!     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_ene_monitor_upw(i_field, iak_diff_hf,        &
!!     &          node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld, &
!!     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_temp_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_physical_property
      use m_SGS_model_coefs
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor(i_field, iak_diff_hf,              &
     &          node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld,   &
     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
!
      integer (kind=kint), intent(in) :: i_field, iak_diff_hf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_h_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,   &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_par_temp, ele_fld%ntot_phys, iphys_ele%i_velo,      &
     &      ele_fld%d_fld, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_h_flux, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_ph_flux, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux                                 &
     &       (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,           &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf), coef_nega_t,       &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_w_const                                      &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_SGS_h_flux, coef_nega_t, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor_upw(i_field, iak_diff_hf,          &
     &          node, ele, fluid, iphys, nod_fld, iphys_ele, ele_fld,   &
     &          jac_3d, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
!
      integer (kind = kint), intent(in) :: i_field, iak_diff_hf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_h_advect)  then
        call int_vol_scalar_inertia_upw                                 &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,   &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,      &
     &      ele_fld%d_fld, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia_upw                                 &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_par_temp, ele_fld%ntot_phys, iphys_ele%i_velo,      &
     &      iphys_ele%i_velo, ele_fld%d_fld, coef_nega_t,               &
     &      fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_h_flux, ele_fld%ntot_phys, iphys_ele%i_velo,        &
     &      ele_fld%d_fld, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, intg_point_t_evo,                 &
     &      iphys%i_ph_flux, ele_fld%ntot_phys, iphys_ele%i_velo,       &
     &      ele_fld%d_fld, coef_nega_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux_upw                             &
     &       (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,           &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf),                    &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        coef_nega_t, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_w_const_upw                                  &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
              fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_SGS_h_flux, ele_fld%ntot_phys, iphys_ele%i_velo,  &
     &        ele_fld%d_fld, coef_nega_t, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
