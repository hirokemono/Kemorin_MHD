!cal_vector_potential_pre.f90
!      module cal_vector_potential_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
      module cal_vector_potential_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
!
      implicit none
!
      private :: cal_vect_p_pre_euler, cal_vect_p_pre_adams
      private :: cal_vect_p_pre_crank, cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vector_p_pre
!
      use calypso_mpi
      use m_finite_element_matrix
      use m_node_phys_address
      use m_group_data
      use m_element_phys_data
!
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use set_vecp_boundary
      use int_vol_diffusion_ele
      use int_vol_vect_p_pre
      use int_surf_fixed_gradients
!
      use check_finite_element_mat
!
!
      call reset_ff_smps
!
!   lead diffusion term
!
      call int_vol_vecp_diffuse_ele
!
!  lead induction terms
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_sgs_uxb_2_evo
      end if
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_vect_p_pre_ele_upm(fld_ele1%ntot_phys, d_ele)
      else
        call int_vol_vect_p_pre_ele(fld_ele1%ntot_phys, d_ele)
      end if
!
      call int_sf_grad_vecp(sf_grp1, intg_point_t_evo)
!
!      call check_nodal_data(my_rank, n_vector, iphys%i_velo)
!      call check_elemental_data(my_rank, n_vector, iphys_ele%i_magne)
!      call check_ff_smp(n_vector)
!      call check_ff_nl_smp(n_vector)
!
      if (iflag_debug.eq.1) write(*,*) 'coefs_4_time_evolution_end'
!
!  -----for explicit euler
!
      if (iflag_t_evo_4_vect_p .eq. id_explicit_euler) then
        call cal_vect_p_pre_euler
!
!  -----for Adams_Bashforth
!
      else if (iflag_t_evo_4_vect_p .eq. id_explicit_adams2) then
        call cal_vect_p_pre_adams
!
!  -----for Ceank-nicolson
!
      else if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson) then
        call cal_vect_p_pre_crank
      else if (iflag_t_evo_4_vect_p.eq.id_Crank_nicolson_cmass) then
        call cal_vect_p_pre_consist_crank
      end if
!
      call set_boundary_vect_p
!
      call vector_send_recv(iphys%i_vecp)
!
!      call check_nodal_data(my_rank, n_vector, iphys%i_vecp)
!
      end subroutine cal_vector_p_pre
!
! ----------------------------------------------------------------------
!
       subroutine cal_vect_p_pre_euler
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
       call cal_t_evo_4_vector_cd(iflag_mag_supg)
       call cal_sol_vect_p_pre_euler
!
       end subroutine cal_vect_p_pre_euler
!
! ----------------------------------------------------------------------
!
       subroutine cal_vect_p_pre_adams
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
       call cal_t_evo_4_vector_cd(iflag_mag_supg)
       call cal_sol_vect_p_pre_adams
!
       end subroutine cal_vect_p_pre_adams
!
! ----------------------------------------------------------------------
!
       subroutine cal_vect_p_pre_crank
!
       use m_t_step_parameter
       use cal_multi_pass
       use cal_sol_vector_pre_crank
       use set_vecp_boundary
       use int_sk_4_fixed_boundary
       use cal_solver_MHD
!
!
       if (coef_imp_b.gt.0.0d0) then
         call int_sk_4_fixed_vector_p
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
       end if
!
       call cal_t_evo_4_vector_cd(iflag_mag_supg)
!
       call set_boundary_vect_p_4_rhs
!
       call cal_sol_vect_p_pre_linear
!
       call cal_sol_vect_p_pre_crank
!
       end subroutine cal_vect_p_pre_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_vect_p_pre_consist_crank
!
       use m_phys_constants
       use m_t_step_parameter
       use set_vecp_boundary
       use cal_sol_vect_crank_consist
       use int_sk_4_fixed_boundary
       use cal_ff_smp_to_ffs
       use int_vol_initial_MHD
       use cal_solver_MHD
!
!
       if (coef_imp_b.gt.0.0d0) then
         call int_sk_4_fixed_vector_p
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
       end if
!
       call int_vol_initial_vect_p
       call set_ff_nl_smp_2_ff(n_vector)
!
       call set_boundary_vect_p_4_rhs
!
       call cal_sol_vect_p_pre_consist
!
       if (iflag_debug.eq.1) write(*,*) 'time_evolution'
       call cal_sol_vect_p_pre_crank
!
       end subroutine cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_vector_potential_pre
