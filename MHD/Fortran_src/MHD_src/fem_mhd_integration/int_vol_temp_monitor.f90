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
!      subroutine int_vol_ene_monitor(i_field)
!      subroutine int_vol_ene_monitor_upw(i_field)
!!        type(node_data), intent(in) :: node1
!!        type(element_data), intent(in) :: ele1
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld1
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: fld_ele1
!!        type(field_geometry_data), intent(in) :: fluid1
!!        type(jacobians_3d), intent(in) :: jac1_3d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!!        type(gradient_model_data_type), intent(in) :: FEM1_elen
!!        type(work_finite_element_mat), intent(inout) :: fem1_wk
!!        type(finite_ele_mat_node), intent(inout) :: f1_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
!
      module int_vol_temp_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
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
      subroutine int_vol_ene_monitor(i_field,                           &
     &          node1, ele1, fluid1, iphys, nod_fld1, iphys_ele, fld_ele1, &
     &          jac1_3d_q, rhs_tbl1, FEM1_elen,                         &
     &          mhd_fem1_wk, fem1_wk, f1_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
!
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld1
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(field_geometry_data), intent(in) :: fluid1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      integer (kind=kint), intent(in) :: i_field
!
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
!
!
      if (i_field .eq. iphys%i_h_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,  &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_par_temp, fld_ele1%ntot_phys, iphys_ele%i_velo,     &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_h_flux, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_ph_flux, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux                                 &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf), coef_nega_t,       &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_w_const                                      &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_h_flux, coef_nega_t, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor_upw(i_field,                       &
     &          node1, ele1, fluid1, iphys, nod_fld1, iphys_ele, fld_ele1, &
     &          jac1_3d_q, rhs_tbl1, FEM1_elen,                         &
     &          mhd_fem1_wk, fem1_wk, f1_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
!
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld1
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(field_geometry_data), intent(in) :: fluid1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      integer (kind = kint), intent(in) :: i_field
!
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
!
!
      if (i_field .eq. iphys%i_h_advect)  then
        call int_vol_scalar_inertia_upw                                 &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,  &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,     &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia_upw                                 &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_par_temp, fld_ele1%ntot_phys, iphys_ele%i_velo,     &
     &      iphys_ele%i_velo, fld_ele1%d_fld, coef_nega_t,              &
     &      fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_h_flux, fld_ele1%ntot_phys, iphys_ele%i_velo,       &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_ph_flux, fld_ele1%ntot_phys, iphys_ele%i_velo,      &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux_upw                             &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf),                    &
     &        fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,     &
     &        coef_nega_t, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_w_const_upw                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
              fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_h_flux, fld_ele1%ntot_phys, iphys_ele%i_velo, &
     &        fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
