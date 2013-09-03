!cal_part_temperature.f90
!      module cal_part_temp
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine cal_parturbation_temp
!
      module cal_part_temperature
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_control_parameter
      use m_t_int_parameter
!
      implicit none
!
      private :: cal_per_temp_euler, cal_per_temp_adams
      private :: cal_per_temp_crank, cal_per_temp_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_parturbation_temp
!
      use m_node_phys_address
      use m_finite_element_matrix
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_surf_temp
      use int_vol_thermal_ele
      use cal_stratification_by_temp
      use add_nodal_fields
!
!      use check_surface_groups
!      use check_finite_element_mat
!      use check_jacobians
!
!
      if (iflag_SGS_heat .ne. id_SGS_none) call cal_sgs_heat_flux
!
!      call check_nodal_data(my_rank, 3, iphys%i_SGS_h_flux)
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps
!
!  ----------  lead diffusion term
!
      call int_vol_p_termal_diffuse_ele
!
!  ----------  lead advection term
!
      if ( iflag_4_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw
      else
        call int_vol_temp_ele
      end if
!
!      call check_ff_smp(n_scalar)
!      call check_ff_nl_smp(n_scalar)
!
      call int_surf_temp_ele
!
!      call check_nodal_data(my_rank, n_scalar, iphys%i_temp)
!      call check_elemental_data(my_rank, 3 ,iphys_ele%i_velo)
!      call check_ff_smp(n_scalar)
!      call check_ff_nl_smp(n_scalar)
!
      if (iflag_t_strat .gt. id_turn_OFF) then
        if ( iflag_4_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw
        else
          call cal_stratified_layer
        end if
      end if
!
!
      if (iflag_t_evo_4_temp .eq. id_explicit_euler) then
        call cal_per_temp_euler
      else if (iflag_t_evo_4_temp .eq. id_explicit_adams2) then
        call cal_per_temp_adams
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson) then
        call cal_per_temp_crank
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass) then 
        call cal_per_temp_consist_crank
      end if
!
      call set_boundary_part_temp
!
      call scalar_send_recv(iphys%i_par_temp)
!
      call add_2_nod_scalars(iphys%i_temp, iphys%i_par_temp,            &
     &    iphys%i_ref_t)
!
      end subroutine cal_parturbation_temp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine cal_per_temp_euler
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
!
       call cal_t_evo_4_scalar_fl
       call cal_sol_part_temp_euler
!
       end subroutine cal_per_temp_euler
!
! ----------------------------------------------------------------------
!
       subroutine cal_per_temp_adams
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
!
       call cal_t_evo_4_scalar_fl
       call cal_sol_part_temp_adams
!
       end subroutine cal_per_temp_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_crank
!
      use m_t_step_parameter
      use m_node_phys_address
      use cal_sol_vector_pre_crank
      use cal_multi_pass
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
       if (coef_imp_t .gt. 0.0d0) then
         call int_sk_4_fixed_part_temp
         if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
       end if
!
       if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
       call cal_t_evo_4_scalar_fl
!
       call set_boundary_ene_4_rhs
!
       call cal_sol_par_temp_linear
!
       call cal_sol_energy_crank(iphys%i_par_temp)
!
       end subroutine cal_per_temp_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_consist_crank
!
      use m_t_step_parameter
      use m_node_phys_address
      use cal_sol_vect_crank_consist
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
       if (coef_imp_t .gt. 0.0d0) then
         call int_sk_4_fixed_part_temp
         if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
       end if
!
       call int_vol_initial_part_temp
       call set_ff_nl_smp_2_ff(n_scalar)
!
       call set_boundary_ene_4_rhs
!
       call cal_sol_temp_consist
!
       call cal_sol_energy_crank(iphys%i_par_temp)
!
       end subroutine cal_per_temp_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_part_temperature
