!
!      module cal_magnetic_correct
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on March, 2006
!
!      subroutine cal_magnetic_co
!      subroutine cal_magnetic_co_outside
!
      module cal_magnetic_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_group_data
!
      use cal_multi_pass
      use cal_sol_vector_co_crank
!
      implicit none
!
      private :: cal_magnetic_co_exp, cal_magnetic_co_imp
      private :: cal_magnetic_co_crank
      private :: cal_magne_co_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co
!
      use m_nod_comm_table
      use m_node_phys_address
      use m_node_phys_data
      use m_phys_constants
      use m_jacobian_sf_grp
      use m_sorted_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_address
      use m_SGS_model_coefs
      use m_surf_data_magne_p
      use m_bc_data_magne
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_co'
      call int_vol_magne_co
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. ngrp_sf_sgs_magp.gt.0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_m_phi
         call int_surf_sgs_velo_co_ele                                  &
     &      (node1, ele1, surf1, sf_grp1, nod_fld1,                     &
     &       jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, rhs_tbl1, FEM1_elen,   &
     &       intg_point_poisson, ngrp_sf_sgs_magp, id_grp_sf_sgs_magp,  &
     &       ifilter_final, ak_diff(1,iak_diff_b), iphys%i_m_phi,       &
     &       fem1_wk, f1_nl)
      end if
!
!
!
      if (   iflag_implicit_correct.eq.3                                &
     &  .or. iflag_implicit_correct.eq.4) then
        call cal_magnetic_co_imp
      else
        call cal_magnetic_co_exp
      end if
!
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_vect magne'
      call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
      call vector_send_recv(iphys%i_magne, node1, nod_comm, nod_fld1)
      call scalar_send_recv(iphys%i_mag_p, node1, nod_comm, nod_fld1)
!
      end subroutine cal_magnetic_co
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_exp
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_multi_pass_4_vector_ff'
      call cal_multi_pass_4_vector_ff
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co'
      call cal_sol_magne_co(node1%istack_internal_smp)
!
      end subroutine cal_magnetic_co_exp
!
! -----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_imp
!
      use m_t_step_parameter
      use int_vol_diffusion_ele
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
!
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_magne_diffuse_co'
      call int_vol_magne_diffuse_co
!
!
      if (coef_imp_b.gt.0.0d0) then
        if (iflag_debug.eq.1)  write(*,*) 'int_sk_4_fixed_magne'
        call int_sk_4_fixed_magne
      end if
!
!
      if (     iflag_implicit_correct.eq.3) then
        call cal_magnetic_co_crank
      else if (iflag_implicit_correct.eq.4) then
        call cal_magne_co_consist_crank
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_pre_crank'
      call cal_sol_magne_pre_crank
!
      end subroutine cal_magnetic_co_imp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_crank
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_bc_data_magne
      use set_boundary_scalars
!
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_t_evo_4_vector'
      call cal_t_evo_4_vector(iflag_mag_supg)
!
      if (iflag_debug.eq.1)   write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)   write(*,*) 'cal_sol_magne_co_crank'
      call cal_sol_magne_co_crank(node1%istack_internal_smp)
!
      end subroutine cal_magnetic_co_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_magne_co_consist_crank
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_bc_data_magne
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_boundary_scalars
!
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_initial_magne'
      call int_vol_initial_magne
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co_crank_consist'
      call cal_sol_magne_co_crank_consist(node1%istack_internal_smp)
!
      end subroutine cal_magne_co_consist_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_magnetic_co_outside
!
      use m_nod_comm_table
      use m_node_phys_address
      use m_node_phys_data
      use m_phys_constants
      use m_jacobian_sf_grp
      use m_sorted_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_SGS_address
      use m_SGS_model_coefs
      use m_surf_data_magne_p
      use m_bc_data_magne
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use int_vol_solenoid_correct
      use int_surf_grad_sgs
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_magne_ins_co
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. ngrp_sf_sgs_magp.gt.0) then
        if (iflag_debug.eq.1) write(*,*)                                &
                             'int_surf_sgs_velo_co_ele', iphys%i_m_phi
         call int_surf_sgs_velo_co_ele                                  &
     &      (node1, ele1, surf1, sf_grp1, nod_fld1,                     &
     &       jac1_sf_grp_2d_q, jac1_sf_grp_2d_l, rhs_tbl1, FEM1_elen,   &
     &       intg_point_poisson, ngrp_sf_sgs_magp, id_grp_sf_sgs_magp,  &
     &       ifilter_final, ak_diff(1,iak_diff_b), iphys%i_m_phi,       &
     &       fem1_wk, f1_nl)
      end if
!
!
      call cal_multi_pass_4_vector_ins
!
      call cal_sol_magne_insulator
!
      call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
      call vector_send_recv(iphys%i_magne, node1, nod_comm, nod_fld1)
!
      end subroutine cal_magnetic_co_outside
!
! -----------------------------------------------------------------------
!
      end module cal_magnetic_correct
