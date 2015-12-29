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
      use m_geometry_data
      use m_nod_comm_table
      use m_finite_element_matrix
      use m_node_phys_data
      use m_element_phys_data
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
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!   ----  set SGS fluxes
!
!
      if (iflag_SGS_gravity .ne. id_SGS_none) then
        call cal_sgs_mom_flux_with_sgs_buo(layer_tbl)
        call mod_Csim_by_SGS_buoyancy_ele(layer_tbl%e_grp)
      end if
!
      if ( iflag_SGS_inertia .ne. id_SGS_none) then
       call cal_sgs_momentum_flux
      end if
!
      if ( iflag_SGS_lorentz .ne. id_SGS_none) then
       call cal_sgs_maxwell
      end if
!
!   --- reset work array for time evolution
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
! --------   loop for direction of velocity ---------------
!
      call int_vol_viscosity_ele
!
      if ( iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
         if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele
      end if
!
! -------     advection and forces
!
      if (iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_pre_ele_upwind(fld_ele1%ntot_phys,            &
     &      iphys_ele%i_velo, fld_ele1%d_fld, iphys_ele)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_pre_ele_upwind(fld_ele1%ntot_phys,            &
     &      iphys_ele%i_magne, fld_ele1%d_fld, iphys_ele)
      else
        call int_vol_velo_pre_ele                                       &
     &     (fld_ele1%ntot_phys, fld_ele1%d_fld, iphys_ele)
      end if
!
!    ---  lead surface boundaries
!
      call int_surf_velo_pre_ele
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
      call set_normal_velo
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
      use m_geometry_data
      use m_node_phys_data
      use cal_multi_pass
      use cal_sol_vector_explicit
      use int_vol_coriolis_term
!
      call cal_t_evo_4_vector_fl(iflag_velo_supg)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp
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
      use m_geometry_data
      use m_node_phys_data
      use cal_multi_pass
      use cal_sol_vector_explicit
      use int_vol_coriolis_term
!
!
      call cal_t_evo_4_vector_fl(iflag_velo_supg)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp
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
      use m_finite_element_matrix
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
!
!
       if (coef_imp_v.gt.0.0d0) then
         call int_sk_4_fixed_velo
!         if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
       end if
!
       call cal_t_evo_4_vector_fl(iflag_velo_supg)
!
       if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
       call int_coriolis_nod_exp
!
       if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
       call int_buoyancy_nod_exp
!
       call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
       call cal_sol_velo_pre_linear(node1, iphys, nod_fld1)
!
       call cal_sol_velo_pre_crank
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
      use m_finite_element_matrix
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo
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
      call cal_sol_velo_pre_crank
!
      end subroutine cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_velocity_pre
