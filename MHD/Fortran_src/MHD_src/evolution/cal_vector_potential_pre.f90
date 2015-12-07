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
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_finite_element_matrix
      use m_jacobian_sf_grp
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_data
      use m_control_parameter
      use m_bc_data_magne
      use m_surf_data_vector_p
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use int_vol_diffusion_ele
      use int_vol_vect_p_pre
      use int_surf_fixed_gradients
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
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
        call int_vol_vect_p_pre_ele_upm                                 &
     &     (fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      else
        call int_vol_vect_p_pre_ele                                     &
     &     (fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld)
      end if
!
      call int_sf_grad_velocity(node1, ele1, surf1, sf_grp1,            &
     &    jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_grad_a,                    &
     &    intg_point_t_evo, ak_d_magne, fem1_wk, f1_l)
!
!      call check_nodal_data(my_rank, nod_fld1, n_vector, iphys%i_velo)
!      call check_nodal_data(my_rank, fld_ele1,                         &
!     &    n_vector, iphys_ele%i_magne)
!      call check_ff_smp(my_rank, n_vector, node1%max_nod_smp, f1_l)
!      call check_ff_smp(my_rank, n_vector, node1%max_nod_smp, f1_nl)
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
      call set_boundary_vect(nod_bc1_a, iphys%i_vecp, nod_fld1)
!
      call vector_send_recv(iphys%i_vecp, node1, nod_comm, nod_fld1)
!
!      call check_nodal_data(my_rank, nod_fld1, n_vector, iphys%i_vecp)
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
      use m_phys_constants
      use m_t_step_parameter
      use m_finite_element_matrix
      use m_bc_data_magne
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
!
!
       if (coef_imp_b.gt.0.0d0) then
         call int_sk_4_fixed_vector_p
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
       end if
!
       call cal_t_evo_4_vector_cd(iflag_mag_supg)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
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
      use m_finite_element_matrix
      use m_bc_data_magne
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
!
       if (coef_imp_b.gt.0.0d0) then
         call int_sk_4_fixed_vector_p
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
       end if
!
       call int_vol_initial_vect_p
       call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
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
