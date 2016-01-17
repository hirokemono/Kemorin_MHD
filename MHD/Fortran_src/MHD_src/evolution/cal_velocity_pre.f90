!
!      module cal_velocity_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine s_cal_velocity_pre(layer_tbl)
!
      module cal_velocity_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_address
!
      use t_layering_ele_list
!
      implicit none
!
      private :: cal_velo_pre_euler, cal_velo_pre_adams
      private :: cal_velo_pre_crank, cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_velocity_pre(layer_tbl)
!
      use m_group_data
      use m_geometry_data_MHD
      use m_surf_data_torque
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use int_vol_diffusion_ele
      use int_vol_velo_pre
      use int_surf_velo_pre
      use int_vol_coriolis_term
      use cal_sgs_m_flux_sgs_buo
      use modify_Csim_by_SGS_buo_ele
      use set_normal_field
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!   ----  set SGS fluxes
!
!
      if (iflag_SGS_gravity .ne. id_SGS_none) then
        call cal_sgs_mom_flux_with_sgs_buo                              &
     &     (i_dvx, nod_comm, node1, ele1, surf1, sf_grp1,               &
     &      fluid1, layer_tbl, iphys, iphys_ele, fld_ele1,              &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
        call mod_Csim_by_SGS_buoyancy_ele(layer_tbl%e_grp, ele1)
      end if
!
      if ( iflag_SGS_inertia .ne. id_SGS_none) then
        call cal_sgs_momentum_flux(i_dvx, nod_comm, node1, ele1,        &
     &      fluid1, iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,    &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      if ( iflag_SGS_lorentz .ne. id_SGS_none) then
        call cal_sgs_maxwell(i_dbx, nod_comm, node1, ele1, fluid1,      &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, &
     &      mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
!   --- reset work array for time evolution
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
! --------   loop for direction of velocity ---------------
!
      if (coef_velo.gt.zero .and. coef_exp_v.gt.zero) then
        call int_vol_vector_diffuse_ele(fluid1%istack_ele_fld_smp,      &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_v, coef_exp_v, ak_d_velo, iphys%i_velo,            &
     &      fem1_wk, f1_l)
      end if
!
      if ( iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
         if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele                                 &
     &     (node1, ele1, fluid1, jac1_3d_q, rhs_tbl1,                   &
     &      iphys%i_velo, nod_fld1, fem1_wk, f1_l)
      end if
!
! -------     advection and forces
!
      if (iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_pre_ele_upwind                                &
     &     (node1, ele1, fluid1, iphys, nod_fld1, fld_ele1%ntot_phys,   &
     &      iphys_ele%i_velo, fld_ele1%d_fld, iphys_ele,                &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk,                &
     &      fem1_wk, f1_nl)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_pre_ele_upwind                                &
     &     (node1, ele1, fluid1, iphys, nod_fld1, fld_ele1%ntot_phys,   &
     &      iphys_ele%i_magne, fld_ele1%d_fld, iphys_ele,               &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk,                &
     &      fem1_wk, f1_nl)
      else
        call int_vol_velo_pre_ele                                       &
     &     (node1, ele1, fluid1, iphys, nod_fld1, fld_ele1%ntot_phys,   &
     &      fld_ele1%d_fld, iphys_ele, jac1_3d_q, rhs_tbl1, FEM1_elen,  &
     &      mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
!    ---  lead surface boundaries
!
      call int_surf_velo_pre_ele(node1, ele1, surf1, sf_grp1,           &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
!
      if (iflag_t_evo_4_velo .eq. id_explicit_euler) then
       call cal_velo_pre_euler
!
      else if (iflag_t_evo_4_velo .eq. id_explicit_adams2) then
       call cal_velo_pre_adams
!
      else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
       call cal_velo_pre_crank
!
      else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then 
       call cal_velo_pre_consist_crank
      end if
!
      call set_boundary_velo(node1, iphys%i_velo, nod_fld1)
      call set_normal_velocity                                          &
     &   (sf_grp1, sf_grp_nod1, sf_bc1_norm_v, iphys%i_velo, nod_fld1)
!
      call vector_send_recv(iphys%i_velo, node1, nod_comm, nod_fld1)
!
      end subroutine s_cal_velocity_pre
!
! ----------------------------------------------------------------------
!  --------  subroutine cal_velo_pre_euler  -------
!
      subroutine cal_velo_pre_euler
!
      use cal_multi_pass
      use cal_sol_vector_explicit
      use int_vol_coriolis_term
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &    (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call cal_sol_velo_pre_euler(node1, iphys, nod_fld1)
!
      end subroutine cal_velo_pre_euler
!
! ----------------------------------------------------------------------
!  --------  subroutine cal_velo_pre_adams  -------
!
      subroutine cal_velo_pre_adams
!
      use cal_multi_pass
      use cal_sol_vector_explicit
      use int_vol_coriolis_term
!
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call cal_sol_velo_pre_adams(node1, iphys, nod_fld1)
!
      end subroutine cal_velo_pre_adams
!
! ----------------------------------------------------------------------
!  --------  subroutine cal_velo_pre_crank  -------
!
      subroutine cal_velo_pre_crank
!
      use m_t_step_parameter
!
      use m_geometry_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, node1, ele1, nod_fld1,   &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_sol_velo_pre_linear(node1, iphys, nod_fld1)
!
      call cal_sol_velo_pre_crank                                       &
     &    (node1, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,                  &
     &     num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,            &
     &     MG_mat_velo, MG_vector, iphys%i_velo, f1_l, b_vec,           &
     &     x_vec, nod_fld1)
!
      end subroutine cal_velo_pre_crank
!
! ----------------------------------------------------------------------
!  --------  subroutine cal_velo_pre_consist_crank  -------
!
      subroutine cal_velo_pre_consist_crank
!
      use m_t_step_parameter
      use m_physical_property
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
      use m_geometry_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, node1, ele1, nod_fld1,   &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call int_vol_initial_velo
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_velo,                     &
     &    f1_nl%ff, n_vector, iphys%i_pre_mom, nod_fld1, f1_l%ff)
!
      call cal_sol_velo_pre_crank                                       &
     &    (node1, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,                  &
     &     num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,            &
     &     MG_mat_velo, MG_vector, iphys%i_velo, f1_l, b_vec,           &
     &     x_vec, nod_fld1)
!
      end subroutine cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_velocity_pre
