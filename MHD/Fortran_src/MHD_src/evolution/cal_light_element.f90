!
!      module cal_light_element
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine cal_light_element_variation
!
      module cal_light_element
!
      use m_precision
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_phys_constants
      use m_t_int_parameter
!
      implicit none
!
      private :: cal_composit_pre_euler, cal_composit_pre_adams
      private :: cal_composit_pre_crank, cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_light_element_variation
!
      use m_finite_element_matrix
      use m_node_phys_address
!
      use nod_phys_send_recv
      use set_boundary_scalars
      use set_bc_grad_light_comp
      use int_vol_diffusion_ele
      use int_vol_light_comp_ele
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smps
!
      call int_vol_composition_diffuse_ele
!
      if ( iflag_4_supg .gt. id_turn_OFF) then
        call int_vol_composition_ele_upw
      else
        call int_vol_composition_ele
      end if
!
!
      call set_bc_grad_composition
!
!
      if     (iflag_t_evo_4_composit .eq. id_explicit_euler) then
       call cal_composit_pre_euler
!
      else if(iflag_t_evo_4_composit .eq. id_explicit_adams2) then
       call cal_composit_pre_adams
!
      else if(iflag_t_evo_4_composit .eq. id_Crank_nicolson) then
       call cal_composit_pre_crank
!
      else if(iflag_t_evo_4_composit .eq. id_Crank_nicolson_cmass) then
       call cal_composit_pre_consist_crank
      end if
!
      call set_boundary_composition
!
      call scalar_send_recv(iphys%i_light)
!
      end subroutine cal_light_element_variation
!
! ----------------------------------------------------------------------
!
       subroutine cal_composit_pre_euler
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
       call cal_t_evo_4_scalar_fl
       call cal_sol_d_scalar_euler
!
       end subroutine cal_composit_pre_euler
!
! ----------------------------------------------------------------------
!
       subroutine cal_composit_pre_adams
!
       use cal_multi_pass
       use cal_sol_vector_explicit
!
       call cal_t_evo_4_scalar_fl
       call cal_sol_d_scalar_adams
!
       end subroutine cal_composit_pre_adams
!
! ----------------------------------------------------------------------
!
       subroutine cal_composit_pre_crank
!
       use m_t_step_parameter
       use m_node_phys_address
       use cal_multi_pass
       use set_boundary_scalars
       use cal_sol_vector_pre_crank
       use int_sk_4_fixed_boundary
       use cal_solver_MHD
!
!
       if (coef_imp_c.gt.0.0d0) then
         call int_sk_4_fixed_composition
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
       end if
!
       call cal_t_evo_4_scalar_fl
!
       call set_boundary_composition_4_rhs
       call cal_sol_d_scalar_linear
!
       call cal_sol_d_scalar_crank(iphys%i_light)
!
       end subroutine cal_composit_pre_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_composit_pre_consist_crank
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
       if (coef_imp_c.gt.0.0d0) then
         call int_sk_4_fixed_composition
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
       end if
!
       call int_vol_initial_d_scalar
       call set_ff_nl_smp_2_ff(n_scalar)
!
       call set_boundary_composition_4_rhs
!
       call cal_sol_d_scalar_consist
!
       call cal_sol_d_scalar_crank(iphys%i_light)
!
       end subroutine cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_light_element
