!>@file   cal_div_sgs_mom_flux_true.f90
!!@brief  module cal_div_sgs_mom_flux_true
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for true SGS terms
!!
!!@verbatim
!!      subroutine cal_div_sgs_m_flux_true_pre(dt, FEM_prm, SGS_par,    &
!!     &          nod_comm, node, ele, surf, sf_grp, fluid,             &
!!     &          fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,                   &
!!     &          iphys_base, iphys_frc, iphys_div_frc, iphys_dif,      &
!!     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,   &
!!     &          iphys_tr_div_SGS, iphys_ele_base, ak_MHD, fem_int,    &
!!     &          FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat, &
!!     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_div_sgs_maxwell_true_pre(dt, FEM_prm, SGS_par,   &
!!     &          nod_comm, node, ele, surf, sf_grp,                    &
!!     &          fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,            &
!!     &          iphys_base, iphys_frc, iphys_div_frc, iphys_dif,      &
!!     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,   &
!!     &          iphys_trSGS, iphys_ele_base, ak_MHD, fem_int,         &
!!     &          FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat, &
!!     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!!        real(kind = kreal), intent(in) :: dt
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_force_address), intent(in) :: iphys_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(diffusion_address), intent(in) :: iphys_dif
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(SGS_term_address), intent(in) :: iphys_div_SGS
!!        type(SGS_term_address), intent(in) :: iphys_tr_div_SGS
!!        type(SGS_term_address), intent(in) :: iphys_trSGS
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module cal_div_sgs_mom_flux_true
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_surface_bc_data_MHD
      use t_material_property
      use t_scalar_property
      use t_FEM_SGS_model_coefs
      use t_SGS_commutation_coefs
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_m_flux_true_pre(dt, FEM_prm, SGS_par,      &
     &          nod_comm, node, ele, surf, sf_grp, fluid,               &
     &          fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,                     &
     &          iphys_base, iphys_frc, iphys_div_frc, iphys_dif,        &
     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,     &
     &          iphys_tr_div_SGS, iphys_ele_base, ak_MHD, fem_int,      &
     &          FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,   &
     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use copy_nodal_fields
      use cal_fluxes
      use cal_momentum_terms
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(SGS_term_address), intent(in) :: iphys_tr_div_SGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_flux_tensor(iphys_fil%i_velo, iphys_fil%i_velo,          &
     &    iphys_frc%i_m_flux, nod_fld)
      call cal_terms_4_momentum(iphys_div_frc%i_m_flux, dt,             &
     &    FEM_prm, SGS_par%model_p, SGS_par%commute_p,                  &
     &    nod_comm, node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,   &
     &    Vsf_bcs, Bsf_bcs, iphys_base, iphys_frc, iphys_div_frc,       &
     &    iphys_dif, iphys_fil, iphys_fil_frc,                          &
     &    iphys_SGS, iphys_div_SGS, iphys_ele_base, ak_MHD,             &
     &    fem_int, FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk,         &
     &    rhs_mat, nod_fld, ele_fld, v_sol, SR_sig, SR_r)
      call copy_vector_component(nod_fld,                               &
     &    iphys_div_frc%i_m_flux, iphys_tr_div_SGS%i_SGS_m_flux)
!
      end subroutine cal_div_sgs_m_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_maxwell_true_pre(dt, FEM_prm, SGS_par,     &
     &          nod_comm, node, ele, surf, sf_grp,                      &
     &          fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,              &
     &          iphys_base, iphys_frc, iphys_div_frc, iphys_dif,        &
     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,     &
     &          iphys_trSGS, iphys_ele_base, ak_MHD, fem_int,           &
     &          FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,   &
     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use copy_nodal_fields
      use cal_fluxes
      use cal_momentum_terms
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(SGS_term_address), intent(in) :: iphys_trSGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_maxwell_tensor(cd_prop%ex_magne,                         &
     &    iphys_fil%i_magne, iphys_frc%i_maxwell, nod_fld)
      call cal_terms_4_momentum(iphys_div_frc%i_maxwell, dt,            &
     &    FEM_prm, SGS_par%model_p, SGS_par%commute_p,                  &
     &    nod_comm, node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,   &
     &    Vsf_bcs, Bsf_bcs, iphys_base, iphys_frc, iphys_div_frc,       &
     &    iphys_dif, iphys_fil, iphys_fil_frc, iphys_SGS,               &
     &    iphys_div_SGS, iphys_ele_base, ak_MHD, fem_int, FEM_elens,    &
     &    diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,  &
     &    v_sol, SR_sig, SR_r)
      call copy_vector_component(nod_fld,                               &
     &   iphys_div_frc%i_maxwell, iphys_trSGS%i_SGS_Lorentz)
!
      end subroutine cal_div_sgs_maxwell_true_pre
!
!-----------------------------------------------------------------------
!
      end module cal_div_sgs_mom_flux_true
