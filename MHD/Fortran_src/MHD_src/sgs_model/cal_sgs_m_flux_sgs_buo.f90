!cal_sgs_m_flux_sgs_buo.f90
!      module cal_sgs_m_flux_sgs_buo
!
!      written by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_mom_flux_with_sgs_buo                        &
!!     &         (nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,  &
!!     &          Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,          &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,            &
!!     &          FEM_elens, filtering, sgs_coefs, sgs_coefs_nod,       &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MHD_coefficients_type), intent(in) :: sgs_coefs
!!        type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
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
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_material_property
      use t_layering_ele_list
      use t_surface_bc_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_mom_flux_with_sgs_buo                          &
     &         (nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,    &
     &          Vsf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,            &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, filtering, sgs_coefs, sgs_coefs_nod,         &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_control_parameter
      use m_phys_constants
      use m_SGS_address
      use m_physical_property
      use m_ele_info_4_dynamical
      use m_work_4_dynamic_model
!
      use cal_sgs_fluxes
      use cal_momentum_terms
      use products_nodal_fields_smp
      use sgs_buoyancy_flux
      use merge_dynamic_coefs
      use set_sgs_diff_model_coefs
      use int_rms_ave_ele_grps
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
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs
      type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      integer(kind = kint), parameter :: ncomp_sgs_buo= 6
!      integer(kind = kint) :: i
!
!
!   lead SGS momentum flux using original model coefficient
!
      call set_model_coefs_2_ele(itype_SGS_m_flux_coef, n_sym_tensor,   &
     &    iak_sgs_mf, icomp_sgs_mf, layer_tbl%e_grp%num_grp,            &
     &    layer_tbl%e_grp%num_item, layer_tbl%e_grp%istack_grp_smp,     &
     &    layer_tbl%e_grp%item_grp, ele)
!
      call cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx,                  &
     &    nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,        &
     &    jac_3d_q, rhs_tbl, FEM_elens, filtering,                      &
     &    sgs_coefs, sgs_coefs_nod, mhd_fem_wk, fem_wk,                 &
     &    f_l, f_nl, nod_fld)
!
!   lead work of Reynolds stress
!
      call cal_terms_4_momentum                                         &
     &   (iphys%i_SGS_div_m_flux, iak_diff_mf, iak_diff_lor,            &
     &    nod_comm, node, ele, surf, fluid, sf_grp, Vsf_bcs, Bsf_bcs,   &
     &    iphys, iphys_ele, ele_fld, jac_3d_q, jac_sf_grp_q,            &
     &    rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
!$omp parallel
      call cal_phys_dot_product(node, nod_fld,                          &
     &    iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk)
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
!
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
!
      call lsq_model_coefs_4_comps                                      &
     &   (layer_tbl%e_grp%num_grp, ncomp_sgs_buo)
!
!   Parameterize model coeffisient including SGS Buoyancy
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(nlayer_SGS, ifive,         &
!     &      sgs_c_coef(1,icomp_sgs_tbuo), sgs_f_coef(1,iak_sgs_tbuo) )
        call single_Csim_buo_by_mf_ratio(nlayer_SGS, ifive,             &
     &      sgs_c_coef(1,icomp_sgs_tbuo), sgs_f_coef(1,iak_sgs_tbuo) )
        call clippging_sgs_coefs(ncomp_sgs_buo,                         &
     &      iak_sgs_tbuo, icomp_sgs_tbuo)
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
!        call cal_Csim_buo_by_Reynolds_ratio(nlayer_SGS, isix,          &
!     &      sgs_c_coef(1,icomp_sgs_cbuo), sgs_f_coef(1,iak_sgs_cbuo) )
        call single_Csim_buo_by_mf_ratio(nlayer_SGS, isix,              &
     &      sgs_c_coef(1,icomp_sgs_cbuo), sgs_f_coef(1,iak_sgs_cbuo) )
        call clippging_sgs_coefs(ncomp_sgs_buo,                         &
     &      iak_sgs_tbuo, icomp_sgs_tbuo)
      end if
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) 'sgs_f_coef, icomp_sgs_tbuo', iak_sgs_tbuo
!        do i = 1, nlayer_SGS
!          write(*,'(i16,1pe20.12)') i, sgs_f_coef(i,iak_sgs_tbuo)
!        end do
!        write(*,*) 'sgs_f_coef, icomp_sgs_tbuo', icomp_sgs_cbuo
!        do i = 1, nlayer_SGS
!          write(*,'(i16,1p6e20.12)') i,                                &
!     &              sgs_c_coef(i,icomp_sgs_tbuo:icomp_sgs_tbuo+5)
!        end do
!      end if
!
      end subroutine cal_sgs_mom_flux_with_sgs_buo
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_sgs_buo
