!cal_sgs_m_flux_sgs_buo.f90
!      module cal_sgs_m_flux_sgs_buo
!
!      written by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_mom_flux_with_sgs_buo                        &
!!     &         (nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,  &
!!     &          Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,           &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,            &
!!     &          FEM_elens, filtering, ifld_sgs, icomp_sgs,            &
!!     &          ifld_diff, iphys_elediff, sgs_coefs_nod, diff_coefs,  &
!!     &          wk_filter, wk_lsq, wk_sgs, mhd_fem_wk, fem_wk,        &
!!     &          surf_wk, f_l, f_nl, nod_fld, ele_fld, sgs_coefs)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      module cal_sgs_m_flux_sgs_buo
!
      use m_precision
!
      use m_phys_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_material_property
      use t_SGS_model_coefs
      use t_layering_ele_list
      use t_surface_bc_data
!
      implicit none
!
      private :: select_int_vol_sgs_buoyancy
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_mom_flux_with_sgs_buo                          &
     &         (nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,    &
     &          Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ak_MHD,             &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, filtering, ifld_sgs, icomp_sgs,              &
     &          ifld_diff, iphys_elediff, sgs_coefs_nod, diff_coefs,    &
     &          wk_filter, wk_lsq, wk_sgs, mhd_fem_wk, fem_wk,          &
     &          surf_wk, f_l, f_nl, nod_fld, ele_fld, sgs_coefs)
!
      use m_control_parameter
      use m_phys_constants
      use m_physical_property
!
      use cal_sgs_fluxes
      use cal_momentum_terms
      use products_nodal_fields_smp
      use sgs_buoyancy_flux
      use merge_dynamic_coefs
      use set_sgs_diff_model_coefs
      use int_rms_ave_ele_grps
      use modify_Csim_by_SGS_buo_ele
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(field_geometry_data), intent(in) :: fluid
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_terms_address), intent(in) :: ifld_sgs, icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint), parameter :: ncomp_sgs_buo= 6
!      integer(kind = kint) :: i, k
!
!
!   lead SGS momentum flux using original model coefficient
!
      call clear_model_coefs_2_ele                                      &
     &   (ele, n_sym_tensor, icomp_sgs%i_mom_flux, sgs_coefs%ntot_comp, &
     &    sgs_coefs%ak)
      call set_model_coefs_2_ele(ele, itype_SGS_m_flux_coef,            &
     &    n_sym_tensor, ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,      &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp,                     &
     &    wk_sgs%fld_clip, wk_sgs%comp_clip, sgs_coefs%ak)
!
      call cal_sgs_momentum_flux                                        &
     &   (icomp_sgs%i_mom_flux, iphys_elediff%i_velo,                   &
     &    nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,        &
     &    jac_3d_q, rhs_tbl, FEM_elens, filtering,                      &
     &    sgs_coefs, sgs_coefs_nod, wk_filter, mhd_fem_wk, fem_wk,      &
     &    f_l, f_nl, nod_fld)
!
!   lead work of Reynolds stress
!
      call cal_terms_4_momentum(iphys%i_SGS_div_m_flux,                 &
     &    ifld_diff%i_mom_flux, ifld_diff%i_lorentz,                    &
     &    nod_comm, node, ele, surf, fluid, sf_grp, Vsf_bcs, Bsf_bcs,   &
     &    iphys, iphys_ele, ak_MHD, jac_3d_q, jac_sf_grp_q, rhs_tbl,    &
     &    FEM_elens, diff_coefs, mhd_fem_wk, fem_wk, surf_wk,           &
     &    f_l, f_nl, nod_fld, ele_fld)
!
!$omp parallel
      call cal_phys_dot_product                                         &
     &   (iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk,    &
     &    nod_fld)
!$omp end parallel
!
!   lead SGS buoyancy flux
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
        call cal_SGS_gravity_flux(node, coef_buo,                       &
     &      iphys%i_SGS_h_flux, iphys%i_SGS_buo_wk, nod_fld)
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
        call cal_SGS_gravity_flux(node, coef_comp_buo,                  &
     &      iphys%i_SGS_c_flux, iphys%i_SGS_comp_buo_wk, nod_fld)
       end if
!
!   take RMS of SGS buoyancy flux and work of Reynolds stress
      call select_int_vol_sgs_buoyancy(node, ele, layer_tbl,            &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l,                           &
     &    wk_lsq%nlayer, wk_lsq%slocal)
!
      call sum_lsq_coefs_4_comps(ncomp_sgs_buo, wk_lsq)
!
!   Parameterize model coeffisient including SGS Buoyancy
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(wk_sgs%nlayer, ifive,      &
!     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                        &
!     &      ifld_sgs%i_buoyancy, icomp_sgs%i_buoyancy, wk_lsq%slsq,    &
!     &      wk_sgs%comp_coef, wk_sgs%fld_coef)
        call single_Csim_buo_by_mf_ratio(wk_sgs%nlayer, ifive,          &
     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                         &
     &      ifld_sgs%i_buoyancy, icomp_sgs%i_buoyancy, wk_lsq%slsq,     &
     &      wk_sgs%comp_coef, wk_sgs%fld_coef)
        call clippging_sgs_diff_coefs(ncomp_sgs_buo,                    &
     &      ifld_sgs%i_buoyancy, icomp_sgs%i_buoyancy, wk_sgs)
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(wk_sgs%nlayer, isix,       &
!     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                        &
!     &      ifld_sgs%i_comp_buoyancy, icomp_sgs%i_comp_buoyancy,       &
!     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call single_Csim_buo_by_mf_ratio(wk_sgs%nlayer, isix,           &
     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                         &
     &      ifld_sgs%i_comp_buoyancy, icomp_sgs%i_comp_buoyancy,        &
     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call clippging_sgs_diff_coefs(ncomp_sgs_buo,                    &
     &      ifld_sgs%i_buoyancy, icomp_sgs%i_buoyancy, wk_sgs)
      end if
!
      call mod_Csim_by_SGS_buoyancy_ele                                 &
     &   (ele, layer_tbl%e_grp, ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) 'sgs_f_coef, icomp_sgs_tbuo', ifld_sgs%i_buoyancy
!        do i = 1, wk_sgs%nlayer
!          write(*,'(i16,1pe20.12)')                                    &
!     &            i, wk_sgs%fld_coef(i,ifld_sgs%i_buoyancy)
!        end do
!        write(*,*) 'sgs_c_coef, icomp_sgs_tbuo',                       &
!     &            icomp_sgs%i_comp_buoyancy
!        k = icomp_sgs%i_buoyancy
!        do i = 1, wk_sgs%nlayer
!          write(*,'(i16,1p6e20.12)') i, wk_sgs%comp_coef(i,k:k+5)
!        end do
!      end if
!
      end subroutine cal_sgs_mom_flux_with_sgs_buo
!
!  ---------------------------------------------------------------------
!
      subroutine select_int_vol_sgs_buoyancy(node, ele, layer_tbl,      &
     &          iphys, nod_fld, jac_3d_q, jac_3d_l, n_layer_d, sgs_l)
!
      use m_control_parameter
!
      use int_rms_ave_ele_grps
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: n_layer_d
!
      real(kind= kreal), intent(inout) :: sgs_l(n_layer_d,18)
!
!
!
!   take RMS of SGS buoyancy flux and work of Reynolds stress
      if(iflag_4_gravity .gt. id_turn_OFF) then
        call int_vol_2rms_ave_ele_grps                                  &
     &     (node, ele, layer_tbl%e_grp, jac_3d_q, jac_3d_l,             &
     &      intg_point_t_evo, nod_fld%ntot_phys, iphys%i_reynolds_wk,   &
     &      nod_fld%d_fld, nod_fld%ntot_phys, iphys%i_SGS_buo_wk,       &
     &      nod_fld%d_fld, sgs_l(1,1), sgs_l(1,4), sgs_l(1,2),          &
     &      sgs_l(1,5) )
!
        if(iflag_4_composit_buo .gt. id_turn_OFF) then
          call int_vol_rms_ave_ele_grps                                 &
     &       (node, ele, layer_tbl%e_grp, jac_3d_q, jac_3d_l,           &
     &        intg_point_t_evo, nod_fld%ntot_phys,                      &
     &        iphys%i_SGS_comp_buo_wk, nod_fld%d_fld,                   &
     &        sgs_l(1,3), sgs_l(1,6))
        end if
      else if(iflag_4_composit_buo .gt. id_turn_OFF) then
        call int_vol_2rms_ave_ele_grps                                  &
     &     (node, ele, layer_tbl%e_grp, jac_3d_q, jac_3d_l,             &
     &      intg_point_t_evo, nod_fld%ntot_phys, iphys%i_reynolds_wk,   &
     &      nod_fld%d_fld, nod_fld%ntot_phys,                           &
     &      iphys%i_SGS_comp_buo_wk, nod_fld%d_fld,                     &
     &      sgs_l(1,1), sgs_l(1,4), sgs_l(1,3), sgs_l(1,6))
      end if
!
      end subroutine select_int_vol_sgs_buoyancy
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_sgs_buo
