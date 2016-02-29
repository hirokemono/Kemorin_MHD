!
!      module cal_velocity_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine s_cal_velocity_pre(nod_comm, node, ele, surf, fluid, &
!!     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,        &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens, &
!!     &          layer_tbl, num_MG_level, MG_interpolate,              &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!      subroutine cal_velocity_co(nod_comm, node, ele, surf, fluid,    &
!!     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,        &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, num_MG_level, MG_interpolate,     &
!!     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,&
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!       &           :: MG_comm_fluid(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!       &           :: MG_DJDS_fluid(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!       &           :: MG_vector(0:num_MG_level)
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_velocity_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_layering_ele_list
      use t_solver_djds
      use t_interpolate_table
      use t_vector_for_solver
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_velocity_pre(nod_comm, node, ele, surf, fluid,   &
     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,          &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,   &
     &          layer_tbl, num_MG_level, MG_interpolate,                &
     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_surf_data_torque
      use m_SGS_address
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
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
!   ----  set SGS fluxes
!
!
      if (iflag_SGS_gravity .ne. id_SGS_none) then
        call cal_sgs_mom_flux_with_sgs_buo(nod_comm, node, ele, surf,   &
     &      sf_grp, fluid, layer_tbl, iphys, iphys_ele, ele_fld,        &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl, FEM_elens,       &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
        call mod_Csim_by_SGS_buoyancy_ele(layer_tbl%e_grp, ele)
      end if
!
      if ( iflag_SGS_inertia .ne. id_SGS_none) then
        call cal_sgs_momentum_flux(icomp_sgs_mf, ie_dvx,                &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,           &
     &      f_l, f_nl, nod_fld)
      end if
!
      if ( iflag_SGS_lorentz .ne. id_SGS_none) then
        call cal_sgs_maxwell(icomp_sgs_lor, ie_dbx,                     &
     &      nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,      &
     &      jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk,           &
     &      f_l, f_nl, nod_fld)
      end if
!
!   --- reset work array for time evolution
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
! --------   loop for direction of velocity ---------------
!
      if (coef_velo.gt.zero .and. coef_exp_v.gt.zero) then
        call int_vol_vector_diffuse_ele(fluid%istack_ele_fld_smp,       &
     &      node, ele, nod_fld, jac_3d_q, rhs_tbl, FEM_elens,           &
     &      iak_diff_v, coef_exp_v, ak_d_velo, iphys%i_velo,            &
     &      fem_wk, f_l)
      end if
!
      if ( iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
         if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele(node, ele, fluid, jac_3d_q,     &
     &      rhs_tbl, iphys%i_velo, nod_fld, fem_wk, f_l)
      end if
!
! -------     advection and forces
!
      if (iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_pre_ele_upwind(node, ele, fluid,              &
     &      iphys, nod_fld, ele_fld%ntot_phys, iphys_ele%i_velo,        &
     &      ele_fld%d_fld, iphys_ele, iak_diff_mf, iak_diff_lor,        &
     &      jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_pre_ele_upwind(node, ele, fluid,              &
     &      iphys, nod_fld, ele_fld%ntot_phys, iphys_ele%i_magne,       &
     &      ele_fld%d_fld, iphys_ele, iak_diff_mf, iak_diff_lor,        &
     &      jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
      else
        call int_vol_velo_pre_ele                                       &
     &     (node, ele, fluid, iphys, nod_fld, ele_fld%ntot_phys,        &
     &      ele_fld%d_fld, iphys_ele, iak_diff_mf, iak_diff_lor,        &
     &      jac_3d_q, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, f_nl)
      end if
!
!    ---  lead surface boundaries
!
      call int_surf_velo_pre_ele(iak_diff_mf, iak_diff_lor,             &
     &    node, ele, surf, sf_grp, iphys, nod_fld,                      &
     &    jac_sf_grp_q, rhs_tbl, FEM_elens, fem_wk, f_l, f_nl)
!
!
      if (iflag_t_evo_4_velo .eq. id_explicit_euler) then
        call cal_velo_pre_euler(iflag_velo_supg,                        &
     &     nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,       &
     &     jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if (iflag_t_evo_4_velo .eq. id_explicit_adams2) then
        call cal_velo_pre_adams(iflag_velo_supg,                        &
     &     nod_comm, node, ele, fluid, iphys, iphys_ele, ele_fld,       &
     &     jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
        call cal_velo_pre_lumped_crank                                  &
     &     (iak_diff_v, nod_comm, node, ele, fluid,                     &
     &      iphys, iphys_ele, ele_fld, jac_3d_q, rhs_tbl, FEM_elens,    &
     &      num_MG_level, MG_interpolate, MG_comm_fluid,                &
     &      MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,                     &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then 
        call cal_velo_pre_consist_crank                                 &
     &     (iphys%i_velo, iphys%i_pre_mom, iak_diff_v,                  &
     &      node, ele, fluid, jac_3d_q, rhs_tbl, FEM_elens,             &
     &      num_MG_level, MG_interpolate, MG_comm_fluid,                &
     &      MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,                     &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      call set_boundary_velo(node, iphys%i_velo, nod_fld)
      call set_normal_velocity                                          &
     &   (sf_grp, sf_grp_nod, sf_bc1_norm_v, iphys%i_velo, nod_fld)
!
      call vector_send_recv(iphys%i_velo, node, nod_comm, nod_fld)
!
      end subroutine s_cal_velocity_pre
!
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co(nod_comm, node, ele, surf, fluid,      &
     &          sf_grp, sf_grp_nod, iphys, iphys_ele, ele_fld,          &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, num_MG_level, MG_interpolate,       &
     &          MG_comm_fluid, MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,  &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_SGS_address
      use m_SGS_model_coefs
      use m_surf_data_torque
      use m_surf_data_press
!
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use set_normal_field
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
      use implicit_vector_correct
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_interpolate(num_MG_level)
      type(communication_table), intent(in)                             &
     &           :: MG_comm_fluid(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &           :: MG_DJDS_fluid(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: Vmat_MG_DJDS(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &           :: MG_vector(0:num_MG_level)
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call reset_ff_smps(node%max_nod_smp, f_l, f_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_velo_co'
      call int_vol_solenoid_co                                          &
     &   (fluid%istack_ele_fld_smp, iphys%i_p_phi, iak_diff_v,          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l,                       &
     &    rhs_tbl, FEM_elens, fem_wk, f_nl)
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON                     &
     &     .and. sf_sgs1_grad_p%ngrp_sf_dat.gt.0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_p_phi
        call int_surf_sgs_velo_co_ele(node, ele, surf, sf_grp,          &
     &      nod_fld, jac_sf_grp_q, jac_sf_grp_l,                        &
     &      rhs_tbl, FEM_elens, intg_point_poisson,                     &
     &      sf_sgs1_grad_p%ngrp_sf_dat, sf_sgs1_grad_p%id_grp_sf_dat,   &
     &      ifilter_final, ak_diff(1,iak_diff_v), iphys%i_p_phi,        &
     &      fem_wk, f_nl)
      end if
!
!
      if (   iflag_implicit_correct.eq.3                                &
     &  .or. iflag_implicit_correct.eq.4) then
        call cal_velocity_co_imp(iphys%i_velo,                          &
     &      nod_comm, node, ele, fluid, iphys_ele, ele_fld,             &
     &      jac_3d_q, rhs_tbl, FEM_elens,                               &
     &      num_MG_level, MG_interpolate, MG_comm_fluid,                &
     &      MG_DJDS_fluid, Vmat_MG_DJDS, MG_vector,                     &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      else
        call cal_velocity_co_exp(iphys%i_velo, iphys%i_p_phi,           &
     &      nod_comm, node, ele, fluid, jac_3d_q, rhs_tbl,              &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo'
      call set_boundary_velo(node, iphys%i_velo, nod_fld)
      if (iflag_debug.eq.1) write(*,*) 'set_normal_velocity'
      call set_normal_velocity                                          &
     &   (sf_grp, sf_grp_nod, sf_bc1_norm_v, iphys%i_velo, nod_fld)
!
      if(iflag_debug.eq.1) write(*,*) 'vector_send_recv(iphys%i_velo)'
      call vector_send_recv(iphys%i_velo, node, nod_comm, nod_fld)
      if(iflag_debug.eq.1) write(*,*) 'scalar_send_recv(iphys%i_press)'
      call scalar_send_recv(iphys%i_press, node, nod_comm, nod_fld)
!
      end subroutine cal_velocity_co
!
! ----------------------------------------------------------------------
!
      end module cal_velocity_pre
