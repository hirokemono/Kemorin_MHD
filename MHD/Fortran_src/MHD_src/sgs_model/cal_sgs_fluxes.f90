!
!      module cal_sgs_fluxes
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_heat_flux(iflag_supg, num_int, dt,           &
!!     &          iflag_SGS_flux, itype_Csym_flux,                      &
!!     &          ifleld, ifield_f, ivelo, ivelo_f, i_sgs,              &
!!     &          icomp_sgs_flux, ie_dvx, SGS_param, filter_param,      &
!!     &          nod_comm, node, ele, fluid, iphys_ele_base, ele_fld,  &
!!     &          jacs, rhs_tbl, FEM_elens, filtering,                  &
!!     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,        &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_momentum_flux(dt, FEM_prm, SGS_param,        &
!!     &          filter_param, nod_comm, node, ele, fluid,             &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
!!     &          filtering, icomp_sgs_term, iphys_elediff_vec,         &
!!     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,        &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_maxwell(dt, FEM_prm, SGS_param,              &
!!     &          filter_param, nod_comm, node, ele, fluid,             &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
!!     &          filtering, icomp_sgs_term, iphys_elediff_vec,         &
!!     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,        &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,               &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_magne_induction(dt, FEM_prm, SGS_param,      &
!!     &          filter_param, nod_comm, node, ele, conduct, cd_prop,  &
!!     &          iphys_base, iphys_fil, iphys_SGS,                     &
!!     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
!!     &          filtering, icomp_sgs_term, iphys_elediff_vec,         &
!!     &          sgs_coefs, sgs_coefs_nod, mlump_cd, wk_filter,        &
!!     &          mhd_fem_wk, fem_wk, f_l, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_uxb_2_evo(dt, FEM_prm, SGS_param,            &
!!     &          filter_param, nod_comm, node, ele, conduct, cd_prop,  &
!!     &          iphys_base, iphys_fil, iphys_SGS_wk,                  &
!!     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
!!     &          filtering, icomp_sgs_term, iphys_elediff_vec,         &
!!     &          sgs_coefs, wk_filter, mhd_fem_wk, fem_wk,             &
!!     &          f_nl, nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_fluxes
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_material_property
      use t_SGS_model_coefs
      use t_vector_for_solver
      use t_solver_SR
!
      use cal_sgs_fluxes_simi
!
      implicit none
!
      private :: cal_sgs_m_flux_diffuse, const_viscosity_tensor
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_heat_flux(iflag_supg, num_int, dt,             &
     &          iflag_SGS_flux, itype_Csym_flux,                        &
     &          ifleld, ifield_f, ivelo, ivelo_f, i_sgs,                &
     &          icomp_sgs_flux, ie_dvx, SGS_param, filter_param,        &
     &          nod_comm, node, ele, fluid, iphys_ele_base, ele_fld,    &
     &          jacs, rhs_tbl, FEM_elens, filtering,                    &
     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,          &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
!
      use cal_sgs_heat_fluxes_grad
      use cal_gradient
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: itype_Csym_flux
      integer(kind = kint), intent(in) :: ifleld, ifield_f
      integer(kind = kint), intent(in) :: i_sgs, ivelo, ivelo_f
      integer(kind = kint), intent(in) :: icomp_sgs_flux, ie_dvx
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_SGS_flux .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_s_flux_grad_w_coef', &
     &                       SGS_param%ifilter_final
      call cal_sgs_s_flux_grad_w_coef                                   &
     &   (iflag_supg, num_int, dt, itype_Csym_flux,                     &
     &    SGS_param%icoord_Csim, SGS_param%ifilter_final,               &
     &    icomp_sgs_flux, i_sgs, ifleld, ie_dvx,                        &
     &    nod_comm, node, ele, fluid, iphys_ele_base, ele_fld, jacs,    &
     &    rhs_tbl, FEM_elens, sgs_coefs, mlump_fl, mhd_fem_wk, fem_wk,  &
     &    f_l, nod_fld, v_sol, SR_sig, SR_r)
!
      else if(iflag_SGS_flux .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_sf_simi'
        call cal_sgs_sf_simi                                            &
     &     (i_sgs, ifleld, ifield_f, ivelo, ivelo_f, icomp_sgs_flux,    &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
      else if(iflag_SGS_flux .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_h_flux_diffuse'
        call choose_cal_gradient_w_const                                &
     &     (iflag_supg, num_int, dt, ifleld, i_sgs, dminus,             &
     &      fluid%istack_ele_fld_smp, mlump_fl,                         &
     &      nod_comm, node, ele, iphys_ele_base, ele_fld,               &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, fem_wk,                   &
     &      f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_sgs_heat_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_momentum_flux(dt, FEM_prm, SGS_param,          &
     &          filter_param, nod_comm, node, ele, fluid,               &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,      &
     &          filtering, icomp_sgs_term, iphys_elediff_vec,           &
     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,          &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
!
      use cal_sgs_mom_fluxes_grad
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(SGS_param%SGS_momentum%iflag_SGS_flux                          &
     &     .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_grad', SGS_param%ifilter_final
        call cal_sgs_m_flux_grad_w_coef                                 &
     &     (SGS_param%ifilter_final, icomp_sgs_term%i_SGS_m_flux,       &
     &      iphys_SGS%i_SGS_m_flux, iphys_base%i_velo,                  &
     &      iphys_elediff_vec%i_velo, dt, FEM_prm, SGS_param,           &
     &      nod_comm, node, ele, fluid, iphys_ele_base, ele_fld,        &
     &      jacs, FEM_elens, sgs_coefs, rhs_tbl, mlump_fl,              &
     &      fem_wk, mhd_fem_wk, nod_fld, v_sol, SR_sig, SR_r)
!
      else if(SGS_param%SGS_momentum%iflag_SGS_flux                     &
     &      .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_mf_simi', iphys_SGS%i_SGS_m_flux
        call cal_sgs_mf_simi                                            &
     &    (iphys_SGS%i_SGS_m_flux, iphys_base%i_velo, iphys_fil%i_velo, &
     &     icomp_sgs_term%i_SGS_m_flux, filter_param, nod_comm, node,   &
     &     filtering, sgs_coefs_nod, wk_filter, nod_fld,                &
     &     v_sol, SR_sig, SR_r)
!
      else if(SGS_param%SGS_momentum%iflag_SGS_flux                     &
     &      .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_m_flux_diffuse',      &
     &                                  iphys_SGS%i_SGS_m_flux
        call cal_sgs_m_flux_diffuse(iphys_base%i_velo,                  &
     &      iphys_SGS_wk%i_wk_diffuse, iphys_SGS%i_SGS_m_flux, dt,      &
     &      FEM_prm, nod_comm, node, ele, fluid,                        &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      mlump_fl, fem_wk, f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_sgs_momentum_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell(dt, FEM_prm, SGS_param,                &
     &          filter_param, nod_comm, node, ele, fluid,               &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,      &
     &          filtering, icomp_sgs_term, iphys_elediff_vec,           &
     &          sgs_coefs, sgs_coefs_nod, mlump_fl, wk_filter,          &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,                 &
     &          v_sol, SR_sig, SR_r)
!
      use cal_sgs_mom_fluxes_grad
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (    SGS_param%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_maxwell_grad', SGS_param%ifilter_final
        call cal_sgs_m_flux_grad_w_coef                                 &
     &     (SGS_param%ifilter_final, icomp_sgs_term%i_SGS_Lorentz,      &
     &      iphys_SGS%i_SGS_maxwell, iphys_base%i_magne,                &
     &      iphys_elediff_vec%i_magne, dt, FEM_prm, SGS_param,          &
     &      nod_comm, node, ele, fluid, iphys_ele_base, ele_fld,        &
     &      jacs, FEM_elens, sgs_coefs, rhs_tbl, mlump_fl,              &
     &      fem_wk, mhd_fem_wk, nod_fld, v_sol, SR_sig, SR_r)
!
!
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_mf_simi',  iphys_SGS%i_SGS_maxwell
        call cal_sgs_mf_simi                                            &
     &     (iphys_SGS%i_SGS_maxwell, iphys_base%i_magne,                &
     &      iphys_fil%i_magne, icomp_sgs_term%i_SGS_Lorentz,            &
     &      filter_param, nod_comm, node, filtering, sgs_coefs_nod,     &
     &      wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_m_flux_diffuse',      &
     &                                  iphys_SGS%i_SGS_maxwell
        call cal_sgs_m_flux_diffuse(iphys_base%i_magne,                 &
     &     iphys_SGS_wk%i_wk_diffuse, iphys_SGS%i_SGS_maxwell, dt,      &
     &     FEM_prm, nod_comm, node, ele, fluid,                         &
     &     iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d,            &
     &     rhs_tbl, mlump_fl, fem_wk, f_l, f_nl, nod_fld,               &
     &     v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_magne_induction(dt, FEM_prm, SGS_param,        &
     &          filter_param, nod_comm, node, ele, conduct, cd_prop,    &
     &          iphys_base, iphys_fil, iphys_SGS,                       &
     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,      &
     &          filtering, icomp_sgs_term, iphys_elediff_vec,           &
     &          sgs_coefs, sgs_coefs_nod, mlump_cd, wk_filter,          &
     &          mhd_fem_wk, fem_wk, f_l, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_sgs_inductions_grad
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if     (SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_induct_t_grad'
        call cal_sgs_induct_t_grad_w_coef(SGS_param%ifilter_final,      &
     &      icomp_sgs_term%i_SGS_induction, iphys_SGS%i_SGS_induct_t,   &
     &      iphys_base%i_velo, iphys_base%i_magne,                      &
     &      iphys_elediff_vec%i_velo, iphys_elediff_vec%i_magne, dt,    &
     &      FEM_prm, SGS_param, nod_comm, node, ele, conduct, cd_prop,  &
     &      iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,          &
     &      sgs_coefs, mlump_cd, fem_wk, mhd_fem_wk, f_l,               &
     &      nod_fld, v_sol, SR_sig, SR_r)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_induct_t_simi'
        call cal_sgs_induct_t_simi(iphys_SGS%i_SGS_induct_t,            &
     &      iphys_base%i_velo, iphys_base%i_magne,                      &
     &      iphys_fil%i_velo, iphys_fil%i_magne,                        &
     &      icomp_sgs_term%i_SGS_induction, filter_param,               &
     &      nod_comm, node, filtering, sgs_coefs_nod, wk_filter,        &
     &      nod_fld, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine cal_sgs_magne_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_evo(dt, FEM_prm, SGS_param,              &
     &          filter_param, nod_comm, node, ele, conduct, cd_prop,    &
     &          iphys_base, iphys_fil, iphys_SGS_wk,                    &
     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,      &
     &          filtering, icomp_sgs_term, iphys_elediff_vec,           &
     &          sgs_coefs, wk_filter, mhd_fem_wk, fem_wk,               &
     &          f_nl, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_rotation
      use cal_sgs_uxb_grad
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if     (SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_uxb_2_ff_grad', SGS_param%ifilter_final
        call cal_sgs_uxb_2_ff_grad(SGS_param%itype_Csym_uxb,            &
     &      SGS_param%icoord_Csim, SGS_param%ifilter_final,             &
     &      icomp_sgs_term%i_SGS_induction, iphys_elediff_vec%i_velo,   &
     &      dt, FEM_prm, node, ele, conduct, cd_prop,                   &
     &      iphys_base, nod_fld, iphys_ele_base, ele_fld, jacs,         &
     &      rhs_tbl, FEM_elens, sgs_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &   write(*,*) 'cal_sgs_uxb_2_ff_simi', SGS_param%ifilter_final
        call cal_sgs_uxb_2_ff_simi(icomp_sgs_term%i_SGS_induction, dt,  &
     &      FEM_prm, filter_param, nod_comm, node, ele, conduct,        &
     &      iphys_base, iphys_fil, iphys_SGS_wk,                        &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      filtering, sgs_coefs, wk_filter, fem_wk, f_nl,              &
     &      nod_fld, v_sol, SR_sig, SR_r)
!
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_diffusion) then
         if (iflag_debug.eq.1)                                          &
     &      write(*,*) 'choose_int_vol_rotations'
         call choose_int_vol_rotations                                  &
     &      (FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &       conduct%istack_ele_fld_smp, iphys_base%i_magne,            &
     &       node, ele, nod_fld, iphys_ele_base, ele_fld,               &
     &       jacs%g_FEM, jacs%jac_3d, rhs_tbl, fem_wk, f_nl)
      end if
!
      end subroutine cal_sgs_uxb_2_evo
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_m_flux_diffuse                                 &
     &         (i_vect, i_sgs_diffuse, i_sgs, dt, FEM_prm,              &
     &          nod_comm, node, ele, fluid, iphys_ele_base, ele_fld,    &
     &          g_FEM, jac_3d, rhs_tbl, mlump_fl, fem_wk,               &
     &          f_l, f_nl, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_gradient
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_sgs_diffuse
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    i_vect, i_sgs, dminus, fluid%istack_ele_fld_smp,              &
     &    mlump_fl, nod_comm, node, ele, iphys_ele_base, ele_fld,       &
     &    g_FEM, jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld,           &
     &    v_sol, SR_sig, SR_r)
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    (i_vect+1),  i_sgs_diffuse, dminus, fluid%istack_ele_fld_smp, &
     &    mlump_fl, nod_comm, node, ele, iphys_ele_base, ele_fld,       &
     &    g_FEM, jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld,           &
     &    v_sol, SR_sig, SR_r)
      call choose_cal_gradient_w_const                                  &
     &   (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,        &
     &    (i_vect+2), (i_sgs_diffuse+3),                                &
     &    dminus, fluid%istack_ele_fld_smp,                             &
     &    mlump_fl, nod_comm, node, ele, iphys_ele_base, ele_fld,       &
     &    g_FEM, jac_3d, rhs_tbl, fem_wk, f_l, f_nl, nod_fld,           &
     &    v_sol, SR_sig, SR_r)
!
!
      call const_viscosity_tensor(nod_fld%n_point, nod_fld%ntot_phys,   &
     &    i_sgs_diffuse, i_sgs, nod_fld%d_fld)
!
      end subroutine cal_sgs_m_flux_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine const_viscosity_tensor                                 &
     &         (nnod, ncomp_nod, i_sgs_diffuse, i_sgs, d_nod)
!
      use cal_gradient
!
      integer (kind = kint), intent(in) :: nnod, ncomp_nod
      integer (kind=kint), intent(in) :: i_sgs, i_sgs_diffuse
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
!
      integer (kind=kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod
        d_nod(inod,i_sgs  ) = two * d_nod(inod,i_sgs  )
        d_nod(inod,i_sgs+1) =       d_nod(inod,i_sgs+1)                 &
     &                            + d_nod(inod,i_sgs_diffuse  )
        d_nod(inod,i_sgs+2) =       d_nod(inod,i_sgs+2)                 &
     &                            + d_nod(inod,i_sgs_diffuse+3)
        d_nod(inod,i_sgs+3) = two * d_nod(inod,i_sgs_diffuse+1)
        d_nod(inod,i_sgs+4) =       d_nod(inod,i_sgs_diffuse+2)         &
     &                            + d_nod(inod,i_sgs_diffuse+4)
        d_nod(inod,i_sgs+5) = two * d_nod(inod,i_sgs_diffuse+5)
      end do
!$omp end parallel do
!
      end subroutine const_viscosity_tensor
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes
