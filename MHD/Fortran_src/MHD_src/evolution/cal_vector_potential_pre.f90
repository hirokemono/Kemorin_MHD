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
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
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
      use m_group_data
      use m_jacobian_sf_grp
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
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(ele1%istack_ele_smp,            &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_b, coef_exp_b, ak_d_magne, iphys%i_vecp,           &
     &      fem1_wk, f1_l)
      end if
!
!  lead induction terms
!
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_sgs_uxb_2_evo(icomp_sgs_uxb, ie_dvx,                   &
     &     nod_comm, node1, ele1, conduct1, iphys, iphys_ele, fld_ele1, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk,        &
     &     f1_nl, nod_fld1)
      end if
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_vect_p_pre_ele_upm                                 &
     &     (node1, ele1, conduct1, iphys, nod_fld1,                     &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      jac1_3d_q, rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_nl)
      else
        call int_vol_vect_p_pre_ele                                     &
     &     (node1, ele1, conduct1, iphys, nod_fld1,                     &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      jac1_3d_q, rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_nl)
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
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_p_pre_euler(node1, iphys, nod_fld1)
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
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_p_pre_adams(node1, iphys, nod_fld1)
!
      end subroutine cal_vect_p_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_crank
!
      use m_phys_constants
      use m_t_step_parameter
      use m_bc_data_magne
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_SGS_address
!
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector_p(iphys%i_vecp, iak_diff_b,          &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
!
      call cal_sol_vect_p_pre_linear                                    &
     &   (node1, iphys, mhd_fem1_wk, f1_nl, f1_l, nod_fld1)
!
      call cal_sol_vect_p_pre_crank                                     &
     &   (node1, iphys, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,          &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_vect_p_pre_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_consist_crank
!
      use m_phys_constants
      use m_t_step_parameter
      use m_bc_data_magne
      use m_physical_property
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_SGS_address
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_vector_p(iphys%i_vecp, iak_diff_b,          &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      call int_vol_initial_vector                                       &
     &   (conduct1%istack_ele_fld_smp, iphys%i_vecp, coef_magne,        &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_magne,                    &
     &    n_vector, iphys%i_pre_uxb, nod_fld1, rhs_tbl1,                &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      if (iflag_debug.eq.1) write(*,*) 'time_evolution'
      call cal_sol_vect_p_pre_crank                                     &
     &   (node1, iphys, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,          &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, f1_l, b_vec, x_vec, nod_fld1)
!
      end subroutine cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      end module cal_vector_potential_pre
