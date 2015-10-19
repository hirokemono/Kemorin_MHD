!
!      module cal_temperature
!
!      subroutine cal_temperature_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
      module cal_temperature
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
      private :: cal_temp_pre_euler, cal_temp_pre_adams
      private :: cal_temp_pre_crank, cal_temp_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_temperature_field
!
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_boundary_scalars
      use int_vol_diffusion_ele
      use int_surf_temp
      use int_vol_thermal_ele
      use cal_stratification_by_temp
      use copy_nodal_fields
!
!
!      call check_surface_param_smp('cal_velocity_pre start',           &
!     &    my_rank, sf_grp1, sf_grp_nod1)
!      call check_jacobians_triquad(ele1, jac1_3d_q)
!
      if (iflag_SGS_heat .ne. id_SGS_none) call cal_sgs_heat_flux
!
!      call check_nodal_data(my_rank, nod_fld1, 3, iphys%i_SGS_h_flux)
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
!  ----------  lead diffusion term
!
      call int_vol_termal_diffuse_ele
!
!  ----------  lead advection term
!
      if (iflag_temp_supg .gt. id_turn_OFF) then
        call int_vol_temp_ele_upw                                       &
     &     (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      else
        call int_vol_temp_ele                                           &
     &     (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
      end if
!
!      call check_ff_smp(my_rank, n_scalar, node1%max_nod_smp, f1_l)
!      call check_ff_smp(my_rank, n_scalar, node1%max_nod_smp, f1_nl)
!
      call int_surf_temp_ele
!
!      call check_nodal_data(my_rank, nod_fld1, n_scalar, iphys%i_temp)
!      call check_nodal_data(my_rank, fld_ele1,                         &
!     &    n_vector, iphys_ele%i_velo)
!      call check_ff_smp(my_rank, n_scalar, node1%max_nod_smp, f1_l)
!      call check_ff_smp(my_rank, n_scalar, node1%max_nod_smp, f1_nl)
!
      if (iflag_t_strat .gt. id_turn_OFF) then
        if (iflag_temp_supg .gt. id_turn_OFF) then
          call cal_stratified_layer_upw                                 &
     &       (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
        else
          call cal_stratified_layer                                     &
     &       (fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld)
        end if
      end if
!
!
      if (iflag_t_evo_4_temp .eq. id_explicit_euler) then
        call cal_temp_pre_euler
      else if (iflag_t_evo_4_temp .eq. id_explicit_adams2) then
        call cal_temp_pre_adams
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson) then
        call cal_temp_pre_crank
      else if (iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass) then 
        call cal_temp_pre_consist_crank
      end if
!
      call set_boundary_ene
!
      call scalar_send_recv(iphys%i_temp, node1, nod_comm, nod_fld1)
!
      if (iphys%i_par_temp .gt. 0) then
        call subtract_2_nod_scalars(node1, nod_fld1,                    &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
!
      end subroutine cal_temperature_field
!
! ----------------------------------------------------------------------
!
       subroutine cal_temp_pre_euler
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
       call cal_t_evo_4_scalar_fl(iflag_temp_supg)
       call cal_sol_temp_euler
!
       end subroutine cal_temp_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_adams
!
      use m_finite_element_matrix
      use cal_multi_pass
      use cal_sol_vector_explicit
!
      call cal_t_evo_4_scalar_fl(iflag_temp_supg)
!      call check_ff(my_rank, n_scalar, node1%numnod, f1_l)
!      call check_ff(my_rank, n_scalar, node1%numnod, f1_nl)
      call cal_sol_temp_adams
!
      end subroutine cal_temp_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_crank
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
         call int_sk_4_fixed_temp
!         if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
       end if
!
       if (iflag_debug.eq.1) write(*,*) 'multi_pass temp'
       call cal_t_evo_4_scalar_fl(iflag_temp_supg)
!
       call set_boundary_ene_4_rhs
!
       call cal_sol_temp_linear
!
       call cal_sol_energy_crank(iphys%i_temp)
!
       end subroutine cal_temp_pre_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_consist_crank
!
      use m_t_step_parameter
      use m_node_phys_address
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
       if (coef_imp_t .gt. 0.0d0) then
         call int_sk_4_fixed_temp
!         if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
       end if
!
       call int_vol_initial_temp
       call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_scalar)
!
       call set_boundary_ene_4_rhs
!
       call cal_sol_temp_consist
!
       call cal_sol_energy_crank(iphys%i_temp)
!
       end subroutine cal_temp_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_temperature
