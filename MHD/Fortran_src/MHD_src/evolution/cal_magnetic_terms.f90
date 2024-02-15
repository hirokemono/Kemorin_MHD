!
!     module cal_magnetic_terms
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_terms_4_magnetic                                 &
!!     &        (i_field, ak_d_magne, dt, FEM_prm,                      &
!!     &         SGS_param, cmt_param, nod_comm, node, ele, surf,       &
!!     &         conduct, sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs,  &
!!     &         iphys_base, iphys_frc, iphys_div_frc, iphys_dif,       &
!!     &         iphys_SGS, iphys_ele_base, ele_fld, fem_int, FEM_elens,&
!!     &         iak_diff_SGS_induction, diff_coefs, mlump_cd,          &
!!     &         mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_magnetic_diffusion(ak_d_magne,                   &
!!     &          FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,   &
!!     &          surf, conduct, sf_grp, Bnod_bcs, Asf_bcs, Bsf_bcs,    &
!!     &          iphys_base, iphys_dif, iphys_SGS, fem_int, FEM_elens, &
!!     &          diff_coefs, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_force_address), intent(in) :: iphys_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(diffusion_address), intent(in) :: iphys_dif
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_magnetic_terms
!
      use m_precision
!
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_SGS_term_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_bc_data_magne
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_material_property
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use cal_multi_pass
      use nod_phys_send_recv
      use int_surf_magne_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_magnetic                                   &
     &        (i_field, ak_d_magne, dt, FEM_prm,                        &
     &         SGS_param, cmt_param, nod_comm, node, ele, surf,         &
     &         conduct, sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs,    &
     &         iphys_base, iphys_frc, iphys_div_frc, iphys_dif,         &
     &         iphys_SGS, iphys_ele_base, ele_fld, fem_int, FEM_elens,  &
     &         ak_diff, mlump_cd, mhd_fem_wk, rhs_mat, nod_fld,         &
     &         v_sol, SR_sig, SR_r)
!
      use int_vol_magne_monitor
      use set_boundary_scalars
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: mlump_cd
!
      integer (kind=kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff_smps(node, rhs_mat%f_l, rhs_mat%f_nl)
!
      if (FEM_prm%iflag_magne_supg .gt. id_turn_OFF) then
        call int_vol_magne_monitor_upm                                  &
     &     (i_field, FEM_prm%npoint_t_evo_int, dt,                      &
     &      SGS_param, cmt_param, node, ele, conduct, cd_prop,          &
     &      iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,            &
     &      nod_fld, iphys_ele_base, ele_fld,                           &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, ak_diff, mhd_fem_wk,                             &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      else
        call int_vol_magne_monitor_pg                                   &
     &     (i_field, FEM_prm%npoint_t_evo_int,                          &
     &      SGS_param, cmt_param, node, ele, conduct, cd_prop,          &
     &      iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,            &
     &      nod_fld, iphys_ele_base, ele_fld,                           &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      FEM_elens, ak_diff, mhd_fem_wk,                             &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
      call int_surf_magne_monitor(SGS_param, cmt_param,                 &
     &    FEM_prm%npoint_t_evo_int, i_field, ak_d_magne,                &
     &    node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs,                    &
     &    iphys_base, iphys_dif, iphys_SGS, nod_fld,                    &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,   &
     &    FEM_elens, ak_diff, rhs_mat%fem_wk, rhs_mat%surf_wk,          &
     &    rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_t_evo_4_vector_cd                                        &
     &   (FEM_prm%iflag_magne_supg, conduct%istack_ele_fld_smp, dt,     &
     &    FEM_prm, mlump_cd, nod_comm, node, ele,                       &
     &    iphys_ele_base, ele_fld,                                      &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    mhd_fem_wk%ff_m_smp, rhs_mat%fem_wk,                          &
     &    rhs_mat%f_l, rhs_mat%f_nl, v_sol, SR_sig, SR_r)
      call delete_vector_ffs_on_bc                                      &
     &   (node, Bnod_bcs%nod_bc_b, rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_nl%ff, mlump_cd%ml, nod_fld%ntot_phys,              &
     &    i_field, nod_fld%d_fld)
      call vector_send_recv(i_field, nod_comm, nod_fld,                 &
     &                      v_sol, SR_sig, SR_r)
!
      end subroutine cal_terms_4_magnetic
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_diffusion(ak_d_magne,                     &
     &          FEM_prm, SGS_param, cmt_param, nod_comm, node, ele,     &
     &          surf, conduct, sf_grp, Bnod_bcs, Asf_bcs, Bsf_bcs,      &
     &          iphys_base, iphys_dif, iphys_SGS, fem_int, FEM_elens,   &
     &          diff_coefs, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use int_vol_diffusion_ele
      use set_boundary_scalars
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(diffusion_address), intent(in) :: iphys_dif
      type(SGS_term_address), intent(in) :: iphys_SGS
!
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call reset_ff_smps(node, rhs_mat%f_l, rhs_mat%f_nl)
!
      call int_vol_vector_diffuse_ele                                   &
     &   (SGS_param%ifilter_final, conduct%istack_ele_fld_smp,          &
     &    FEM_prm%npoint_t_evo_int, node, ele, nod_fld,                 &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    FEM_elens, diff_coefs%Cdiff_magne, one, ak_d_magne,           &
     &    iphys_base%i_magne, rhs_mat%fem_wk, rhs_mat%f_l)
!
      call int_surf_magne_monitor(SGS_param, cmt_param,                 &
     &   FEM_prm%npoint_t_evo_int, iphys_dif%i_b_diffuse, ak_d_magne,   &
     &   node, ele, surf, sf_grp, Asf_bcs, Bsf_bcs,                     &
     &   iphys_base, iphys_dif, iphys_SGS, nod_fld,                     &
     &   fem_int%jcs%g_FEM, fem_int%jcs%jac_sf_grp, fem_int%rhs_tbl,    &
     &   FEM_elens, diff_coefs%Cdiff_SGS_uxb%coef(1,1),                 &
     &   rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node,                           &
     &   fem_int%rhs_tbl, rhs_mat%f_l, rhs_mat%f_nl)
!
      call delete_vector_ffs_on_bc                                      &
     &   (node, Bnod_bcs%nod_bc_b, rhs_mat%f_l, rhs_mat%f_nl)
!
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    rhs_mat%f_l%ff, fem_int%m_lump%ml, nod_fld%ntot_phys,         &
     &    iphys_dif%i_b_diffuse, nod_fld%d_fld)
      call vector_send_recv                                             &
     &   (iphys_dif%i_b_diffuse, nod_comm, nod_fld,                     &
     &    v_sol, SR_sig, SR_r)
!
      end subroutine cal_magnetic_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_terms
