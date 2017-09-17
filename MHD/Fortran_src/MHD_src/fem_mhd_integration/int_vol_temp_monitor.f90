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
!!      subroutine cal_terms_4_heat(i_SGS_div_flux,                     &
!!     &          i_velo, i_field, i_SGS_flux, iak_diff_flux,           &
!!     &          iflag_supg, num_int, ifilter_final, iflag_SGS_flux,   &
!!     &          iflag_commute_flux, iflag_commute_field, dt,          &
!!     &          FEM_prm, nod_comm, node, ele, surf, fluid, sf_grp,    &
!!     &          property, Snod_bcs, Ssf_bcs, iphys_ele, ele_fld,      &
!!     &          fem_int, FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, &
!!     &          rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Ssf_bcs
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type (lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!
      module int_vol_temp_monitor
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_bc_data_temp
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
!
      implicit none
!
      private :: sel_int_vol_div_sgs_flux
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_heat (i_SGS_div_flux,                      &
     &          i_velo, i_field, i_SGS_flux, iak_diff_flux,             &
     &          iflag_supg, num_int, ifilter_final, iflag_SGS_flux,     &
     &          iflag_commute_flux, iflag_commute_field, dt,            &
     &          FEM_prm, nod_comm, node, ele, surf, fluid, sf_grp,      &
     &          property, Snod_bcs, Ssf_bcs, iphys_ele, ele_fld,        &
     &          fem_int, FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk,   &
     &          rhs_mat, nod_fld)
!
      use m_fem_gauss_int_coefs
      use int_surf_div_fluxes_sgs
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_boundary_scalars
!
      integer(kind=kint), intent(in) :: iflag_supg, num_int
      integer(kind=kint), intent(in) :: ifilter_final, iflag_SGS_flux
      integer(kind=kint), intent(in) :: iflag_commute_flux
      integer(kind=kint), intent(in) :: iflag_commute_field
!
      integer (kind=kint), intent(in) :: i_SGS_div_flux
      integer(kind=kint), intent(in) :: i_velo, i_field, i_SGS_flux
      integer(kind=kint), intent(in) :: iak_diff_flux
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(scaler_surf_bc_type), intent(in) :: Ssf_bcs
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type (lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, rhs_mat%f_l, rhs_mat%f_nl)
!
      call sel_int_vol_div_sgs_flux                                     &
     &    (iflag_supg, num_int, ifilter_final, iflag_commute_flux,      &
     &     i_velo, i_field, i_SGS_flux, iak_diff_flux, dt,              &
     &     node, ele, fluid, property, nod_fld, iphys_ele, ele_fld,     &
     &     g_FEM1, fem_int%jcs%jac_3d, fem_int%rhs_tbl, FEM_elens,      &
     &     diff_coefs, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl)
!
      if(iflag_commute_field .ne. id_SGS_commute_OFF                    &
          .and. iflag_SGS_flux .ne. id_SGS_none) then
        call int_sf_skv_sgs_div_v_flux                                  &
     &    (node, ele, surf, sf_grp, nod_fld,                            &
     &     g_FEM1, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl, FEM_elens,  &
     &     num_int, Ssf_bcs%sgs%ngrp_sf_dat, Ssf_bcs%sgs%id_grp_sf_dat, &
     &     ifilter_final, i_SGS_flux, i_velo, i_field,                  &
     &     diff_coefs%num_field,iak_diff_flux, diff_coefs%ak,           &
     &     property%coef_advect, rhs_mat%fem_wk, rhs_mat%surf_wk,       &
     &     rhs_mat%f_nl)
      end if
!
      call cal_t_evo_4_scalar(iflag_supg, fluid%istack_ele_fld_smp, dt, &
     &    FEM_prm, mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld,   &
     &    g_FEM1, fem_int%jcs%jac_3d, fem_int%rhs_tbl,                  &
     &    mhd_fem_wk%ff_m_smp, rhs_mat%fem_wk,                          &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_boundary_rhs_scalar                                      &
     &   (node, Snod_bcs%nod_bc_s, rhs_mat%f_l, rhs_mat%f_nl)
!
!       call check_ff(my_rank, n_scalar, node%numnod, rhs_mat%f_nl)
!
      call cal_ff_2_scalar(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_nl%ff, mlump_fl%ml, nod_fld%ntot_phys,              &
     &    i_SGS_div_flux, nod_fld%d_fld)
!
!   communication
!
      call scalar_send_recv(i_SGS_div_flux, nod_comm, nod_fld)
!
      end subroutine cal_terms_4_heat
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_int_vol_div_sgs_flux                               &
     &         (iflag_supg, num_int, ifilter_final, iflag_commute_flux, &
     &          i_velo, i_field, i_SGS_flux, iak_diff_flux, dt,         &
     &          node, ele, fluid, property, nod_fld,                    &
     &          iphys_ele, ele_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_SGS_div_flux
      use int_vol_vect_cst_difference
      use int_vol_vect_cst_diff_upw
!
      integer(kind=kint), intent(in) :: iflag_supg, ifilter_final
      integer(kind=kint), intent(in) :: iflag_commute_flux, num_int
!
      integer(kind=kint), intent(in) :: i_velo, i_field, i_SGS_flux
      integer(kind=kint), intent(in) :: iak_diff_flux
      real(kind = kreal), intent(in) :: dt
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if(iflag_commute_flux .eq. id_SGS_commute_ON) then
        if(iflag_supg .gt. id_turn_OFF) then
          call int_vol_div_SGS_vec_flux_upw(node, ele, nod_fld,         &
     &       g_FEM, jac_3d, rhs_tbl, FEM_elens, diff_coefs,             &
     &       fluid%istack_ele_fld_smp, num_int, dt,                     &
     &       i_velo, i_field, i_SGS_flux, ifilter_final, iak_diff_flux, &
     &       ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,        &
     &       property%coef_nega_adv, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_SGS_vec_flux(node, ele, nod_fld,             &
     &       g_FEM, jac_3d, rhs_tbl, FEM_elens, diff_coefs,             &
     &       fluid%istack_ele_fld_smp, num_int,                         &
     &       i_velo, i_field, i_SGS_flux, ifilter_final, iak_diff_flux, &
     &       property%coef_nega_adv, fem_wk, mhd_fem_wk, f_nl)
        end if
      else
        if(iflag_supg .gt. id_turn_OFF) then
          call int_vol_div_w_const_upw                                  &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
              fluid%istack_ele_fld_smp, num_int, dt,                    &
     &        i_SGS_flux, ele_fld%ntot_phys, iphys_ele%i_velo,          &
     &        ele_fld%d_fld, property%coef_nega_adv, fem_wk, f_nl)
        else
          call int_vol_div_w_const                                      &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, num_int,                        &
     &        i_SGS_flux, property%coef_nega_adv, fem_wk, f_nl)
        end if
      end if
!
      end subroutine sel_int_vol_div_sgs_flux
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
