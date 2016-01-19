!
!      module cal_magnetic_pre
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine cal_magnetic_field_pre
!
      module cal_magnetic_pre
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_t_int_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      implicit none
!
      private :: cal_magne_pre_euler, cal_magne_pre_adams
      private :: cal_magne_pre_crank, cal_magne_pre_consist_crank
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_magnetic_field_pre
!
      use m_group_data
      use m_bc_data_magne
!
      use set_boundary_scalars
      use nod_phys_send_recv
      use cal_sgs_fluxes
      use int_vol_diffusion_ele
      use int_vol_magne_pre
      use int_surf_magne_pre
!
!      use check_surface_groups
!
!
!      call check_surface_param_smp('cal_magnetic_field_pre start',     &
!     &    my_rank, sf_grp1, sf_grp_nod1)
      if ( iflag_SGS_induction .ne. id_SGS_none) then
        call cal_sgs_magne_induction(i_dvx, i_dbx,                      &
     &     nod_comm, node1, ele1, conduct1, iphys, iphys_ele, fld_ele1, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk,        &
     &     f1_l, nod_fld1)
      end if
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
! lead diffusion term
!
      if (coef_magne.gt.zero .and. coef_exp_b.gt.zero) then
        call int_vol_vector_diffuse_ele(conduct1%istack_ele_fld_smp,    &
     &      node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,      &
     &      iak_diff_b, coef_exp_b, ak_d_magne, iphys%i_magne,          &
     &      fem1_wk, f1_l)
      end if
!
! lead induction terms
!
      if (iflag_debug .eq. 0 ) write(*,*) 'coefs_4_time_evolution'
      if (iflag_mag_supg .gt. id_turn_OFF) then
       call int_vol_magne_pre_ele_upm                                   &
     &    (node1, ele1, conduct1, iphys, nod_fld1,                      &
     &     fld_ele1%ntot_phys, fld_ele1%d_fld, iphys_ele,               &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else
       call int_vol_magne_pre_ele                                       &
     &    (node1, ele1, conduct1, iphys, nod_fld1,                      &
     &     fld_ele1%ntot_phys, fld_ele1%d_fld, iphys_ele,               &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
!
      call int_surf_magne_pre_ele(node1, ele1, surf1, sf_grp1,          &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
      if (iflag_t_evo_4_magne .eq. id_explicit_euler) then
       call cal_magne_pre_euler
!
      else if (iflag_t_evo_4_magne .eq. id_explicit_adams2) then
       call cal_magne_pre_adams
!
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson) then
       call cal_magne_pre_crank
!
      else if (iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass) then 
       call cal_magne_pre_consist_crank
      end if
!
      call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
      call vector_send_recv(iphys%i_magne, node1, nod_comm, nod_fld1)
!
      end subroutine cal_magnetic_field_pre
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_euler
!
      use cal_sol_vector_explicit
      use cal_multi_pass
!
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_magne_pre_euler(node1, iphys, nod_fld1)
!
      end subroutine cal_magne_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_adams
!
      use cal_sol_vector_explicit
      use cal_multi_pass
!
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_magne_pre_adams(node1, iphys, nod_fld1)
!
      end subroutine cal_magne_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_crank
!
      use m_phys_constants
      use m_t_step_parameter
      use m_bc_data_magne
      use m_geometry_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use cal_multi_pass
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_solver_MHD
      use set_boundary_scalars
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_magne(iphys%i_magne, node1, ele1, nod_fld1, &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!        if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_sol_magne_pre_linear(node1, iphys, nod_fld1)
!
      if (iflag_debug .eq. 0 ) write(*,*) 'time_evolution'
      call cal_sol_magne_pre_crank                                      &
     &   (node1, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,                 &
     &    num_MG_level, MG_itp, MG_comm, MG_djds_tbl,                   &
     &    MG_mat_magne, MG_vector, iphys%i_magne, f1_l, b_vec,          &
     &    x_vec, nod_fld1)
!
       end subroutine cal_magne_pre_crank
!
! ----------------------------------------------------------------------
!
       subroutine cal_magne_pre_consist_crank
!
      use m_t_step_parameter
      use m_phys_constants
      use m_bc_data_magne
      use m_physical_property
      use m_geometry_data
      use m_node_phys_data
      use m_finite_element_matrix
      use m_solver_djds_MHD
      use m_array_for_send_recv
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use cal_sol_vector_pre_crank
      use int_sk_4_fixed_boundary
      use cal_ff_smp_to_ffs
      use int_vol_initial_MHD
      use cal_solver_MHD
      use set_boundary_scalars
!
!
      if (coef_imp_b.gt.0.0d0) then
        call int_sk_4_fixed_magne(iphys%i_magne, node1, ele1, nod_fld1, &
     &      jac1_3d_q, rhs_tbl1, FEM1_elen, fem1_wk, f1_l)
!         if (iflag_initial_step.eq.1) coef_imp_b = 1.0d0 / coef_imp_b
      end if
!
      call int_vol_initial_magne
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      if (iflag_debug.eq.1) write(*,*) 'bc_4_magne_rhs'
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_vector_pre_consist(node1, coef_magne,                    &
     &    f1_nl%ff, n_vector, iphys%i_pre_uxb, nod_fld1, f1_l%ff)
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
!
      end module cal_magnetic_pre
