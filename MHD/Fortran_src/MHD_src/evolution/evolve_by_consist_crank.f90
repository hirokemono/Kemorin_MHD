!
!      module evolve_by_consist_crank
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_consist_crank
!!      subroutine cal_vect_p_pre_consist_crank
!!      subroutine cal_magne_pre_consist_crank(iak_diff_b)
!!
!!      subroutine cal_temp_pre_consist_crank
!!      subroutine cal_per_temp_consist_crank
!!      subroutine cal_composit_pre_consist_crank
!!
!!      subroutine cal_velo_co_consist_crank
!!      subroutine cal_vector_p_co_consist_crank
!!      subroutine cal_magne_co_consist_crank
!
      module evolve_by_consist_crank
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use m_t_int_parameter
      use m_t_step_parameter
!
      use m_phys_constants
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_consist_crank
!
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use set_nodal_bc_id_data
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
      if (coef_imp_v.gt.0.0d0) then
        call int_sk_4_fixed_velo(iphys%i_velo, iak_diff_v, node1, ele1, &
     &      nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_v = 1.0d0 / coef_imp_v
      end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      call int_vol_initial_vector                                       &
     &   (fluid1%istack_ele_fld_smp, iphys%i_velo, coef_velo,           &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_velo,                     &
     &    n_vector, iphys%i_pre_mom, nod_fld1, rhs_tbl1,                &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      call cal_sol_velo_pre_crank                                       &
     &    (node1, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,                  &
     &     num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,            &
     &     MG_mat_velo, MG_vector, iphys%i_velo, f1_l, b_vec,           &
     &     x_vec, nod_fld1)
!
      end subroutine cal_velo_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_consist_crank
!
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
      use copy_nodal_fields
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
      call clear_nodal_data(node1, nod_fld1, n_scalar, iphys%i_m_phi)
!
      end subroutine cal_vect_p_pre_consist_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_magne_pre_consist_crank(iak_diff_b)
!
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: iak_diff_b
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_magne(iphys%i_magne, iak_diff_b,            &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
      call int_vol_initial_vector                                       &
     &   (conduct1%istack_ele_fld_smp, iphys%i_magne, coef_magne,       &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_magne,                    &
     &    n_vector, iphys%i_pre_uxb, nod_fld1, rhs_tbl1,                &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      if (iflag_debug.eq.1)                                             &
     &        write(*,*) 'time_evolution for magnetic field'
      call cal_sol_magne_pre_crank                                      &
     &   (node1, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,                 &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, iphys%i_magne, f1_l, b_vec,          &
     &    x_vec, nod_fld1)
!
       end subroutine cal_magne_pre_consist_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_consist_crank
!
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_temp(iphys%i_temp, iak_diff_t, node1, ele1, &
     &      nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid1%istack_ele_fld_smp, iphys%i_temp, coef_temp,           &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_temp,                     &
     &    n_scalar, iphys%i_pre_heat, nod_fld1, rhs_tbl1,               &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      call cal_sol_energy_crank                                         &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,                   &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,             &
     &    MG_mat_temp, MG_vector, iphys%i_temp, f1_l, b_vec,            &
     &    x_vec, nod_fld1)
!
      end subroutine cal_temp_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_consist_crank
!
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
      if (coef_imp_t .gt. 0.0d0) then
        call int_sk_4_fixed_part_temp(iphys%i_par_temp, iak_diff_t,     &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      fem1_wk, f1_l)
        if (iflag_initial_step.eq.1) coef_imp_t = 1.0d0 / coef_imp_t
      end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid1%istack_ele_fld_smp, iphys%i_par_temp, coef_temp,       &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_temp,                     &
     &    n_scalar, iphys%i_pre_heat, nod_fld1, rhs_tbl1,               &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      call cal_sol_energy_crank                                         &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,                   &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,             &
     &    MG_mat_temp, MG_vector, iphys%i_par_temp, f1_l, b_vec,        &
     &    x_vec, nod_fld1)
!
      end subroutine cal_per_temp_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_consist_crank
!
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      use cal_sol_vector_pre_crank
      use set_boundary_scalars
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
!
!
       if (coef_imp_c.gt.0.0d0) then
         call int_sk_4_fixed_composition(iphys%i_light, iak_diff_c,     &
     &       node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1,                &
     &       FEM1_elen, fem1_wk, f1_l)
!         if (iflag_initial_step.eq.1) coef_imp_c = 1.0d0 / coef_imp_c
       end if
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      call int_vol_initial_scalar                                       &
     &   (fluid1%istack_ele_fld_smp, iphys%i_light, coef_light,         &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_c, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_light,                    &
     &    n_scalar, iphys%i_pre_composit, nod_fld1, rhs_tbl1,           &
     &    mhd_fem1_wk, f1_nl, f1_l)
!
      call cal_sol_d_scalar_crank                                       &
     &   (node1, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,                   &
     &    num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,             &
     &    MG_mat_d_scalar, MG_vector, iphys%i_light, f1_l, b_vec,       &
     &    x_vec, nod_fld1)
!
       end subroutine cal_composit_pre_consist_crank
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_velo_co_consist_crank
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use set_nodal_bc_id_data
      use cal_sol_vector_co_crank
!
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_initial_velo'
      call int_vol_initial_vector                                       &
     &   (fluid1%istack_ele_fld_smp, iphys%i_velo, coef_velo,           &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_velo_4_rhs'
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_velo_co_crank_consist'
      call cal_vector_co_crank_consist                                  &
     &   (node1, node1%istack_internal_smp, coef_velo)
!
      end subroutine cal_velo_co_consist_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_vector_p_co_consist_crank
!
      use m_bc_data_magne
!
      use int_vol_initial_MHD
      use cal_ff_smp_to_ffs
      use cal_sol_vector_co_crank
      use set_boundary_scalars
!
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'int_vol_initial_vect_p'
      call int_vol_initial_vector                                       &
     &   (conduct1%istack_ele_fld_smp, iphys%i_vecp, coef_magne,        &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect_p_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_a, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_vect_p_co_crank_consist'
      call cal_vector_co_crank_consist                                  &
     &   (node1, node1%istack_internal_smp, coef_magne)
!
      end subroutine cal_vector_p_co_consist_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_co_consist_crank
!
      use m_bc_data_magne
!
      use int_vol_initial_MHD
      use cal_sol_vector_co_crank
      use cal_ff_smp_to_ffs
      use set_boundary_scalars
!
!
      call reset_ff_t_smp(node1%max_nod_smp, mhd_fem1_wk)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_initial_magne'
      call int_vol_initial_vector                                       &
     &   (conduct1%istack_ele_fld_smp, iphys%i_magne, coef_magne,       &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, fem1_wk,          &
     &    mhd_fem1_wk)
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_boundary_magne_4_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_sol_magne_co_crank_consist'
      call cal_vector_co_crank_consist                                  &
     &   (node1, node1%istack_internal_smp, coef_magne)
!
      end subroutine cal_magne_co_consist_crank
!
! -----------------------------------------------------------------------
!
      end module evolve_by_consist_crank
