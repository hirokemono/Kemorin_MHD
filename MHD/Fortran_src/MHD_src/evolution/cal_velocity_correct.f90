!cal_velocity_correct.f90
!      module cal_velocity_correct
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on March, 2006
!
!      subroutine cal_velocity_co
!
      module cal_velocity_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
!
      use cal_multi_pass
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
      implicit none
!
      private :: cal_velocity_co_exp, cal_velocity_co_imp
      private :: cal_velocity_co_crank
      private :: cal_velo_co_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_SGS_address
      use m_SGS_model_coefs
      use m_filter_elength
      use m_surf_data_press
!
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
      use set_nodal_bc_id_data
      use set_surface_id_MHD
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_velo_co'
      call int_vol_velo_co
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON                     &
     &     .and. sf_sgs1_grad_p%ngrp_sf_dat.gt.0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_p_phi
        call int_surf_sgs_velo_co_ele                                   &
     &     (node1, ele1, surf1, sf_grp1, nod_fld1,                      &
     &      jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, rhs_tbl1, FEM1_elen,    &
     &      intg_point_poisson,                                         &
     &      sf_sgs1_grad_p%ngrp_sf_dat, sf_sgs1_grad_p%id_grp_sf_dat,   &
     &      ifilter_final, ak_diff(1,iak_diff_v), iphys%i_p_phi,        &
     &      fem1_wk, f1_nl)
      end if
!
!
      if (   iflag_implicit_correct.eq.3                                &
     &  .or. iflag_implicit_correct.eq.4) then
        call cal_velocity_co_imp
      else
        call cal_velocity_co_exp
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo'
      call set_boundary_velo
      if (iflag_debug.eq.1) write(*,*) 'set_normal_velo'
      call set_normal_velo
!
      if (iflag_debug.eq.1) write(*,*) 'vector_send_recv(iphys%i_velo)'
      call vector_send_recv(iphys%i_velo, node1, nod_comm, nod_fld1)
      call scalar_send_recv(iphys%i_press, node1, nod_comm, nod_fld1)
!
!
      end subroutine cal_velocity_co
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co_exp
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_multi_pass_4_vector_fl'
      call cal_multi_pass_4_vector_fl
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co'
      call cal_sol_velo_co(node1%istack_internal_smp)
!
      end subroutine cal_velocity_co_exp
!
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co_imp
!
      use m_t_step_parameter
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use int_vol_coriolis_term
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_viscosity_co'
      call int_vol_viscosity_co
!
!
      if (coef_imp_v.gt.0.0d0) then
        if (iflag_debug.eq.1) write(*,*) 'int_sk_4_fixed_velo'
        call int_sk_4_fixed_velo
      end if
!
      if ( iflag_4_coriolis .eq. id_Coriolis_ele_imp) then
        if (iflag_debug.eq.1) write(*,*) 'int_vol_coriolis_crank_ele'
        call int_vol_coriolis_crank_ele
      end if
!
!
      if (     iflag_implicit_correct.eq.3) then
        call cal_velocity_co_crank
      else if (iflag_implicit_correct.eq.4) then
        call cal_velo_co_consist_crank
      end if
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_pre_crank'
      call cal_sol_velo_pre_crank
!
      end subroutine cal_velocity_co_imp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velocity_co_crank
!
      use int_vol_coriolis_term
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_t_evo_4_vector_fl'
      call cal_t_evo_4_vector_fl(iflag_velo_supg)
!
      if (iflag_debug.eq.1) write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank'
      call cal_sol_velo_co_crank(node1%istack_internal_smp)
!
      end subroutine cal_velocity_co_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_consist_crank
!
      use m_phys_constants
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
!
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_initial_velo'
      call int_vol_initial_velo
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank_consist'
      call cal_sol_velo_co_crank_consist(node1%istack_internal_smp)
!
      end subroutine cal_velo_co_consist_crank
!
! -----------------------------------------------------------------------
!
      end module cal_velocity_correct
