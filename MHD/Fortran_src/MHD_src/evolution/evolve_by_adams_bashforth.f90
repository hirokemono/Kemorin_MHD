!evolve_by_adams_bashforth.f90
!      module evolve_by_adams_bashforth
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!      subroutine s_cal_velocity_pre(layer_tbl)
!
      module evolve_by_adams_bashforth
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_t_int_parameter
      use m_phys_constants
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
      subroutine cal_velo_pre_adams
!
      use cal_multi_pass
      use cal_sol_field_explicit
      use int_vol_coriolis_term
!
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &   (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo,                   &
     &    iphys%i_pre_mom, nod_fld1%d_fld)
!
      end subroutine cal_velo_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vect_p_pre_adams
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_conduct_adams                               &
     &  (node1%numnod, conduct1%istack_inter_fld_smp,                   &
     &   conduct1%numnod_fld, conduct1%inod_fld,                        &
     &   mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                    &
     &   nod_fld1%ntot_phys, n_vector, iphys%i_vecp, iphys%i_pre_uxb,   &
     &   nod_fld1%d_fld)
!
      end subroutine cal_vect_p_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_adams
!
      use cal_sol_field_explicit
      use cal_multi_pass
!
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_conduct_adams                               &
     &   (node1%numnod, conduct1%istack_inter_fld_smp,                  &
     &    conduct1%numnod_fld, conduct1%inod_fld,                       &
     &    mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_magne, iphys%i_pre_uxb, &
     &    nod_fld1%d_fld)
!
      end subroutine cal_magne_pre_adams
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_temp_pre_adams
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
!
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!      call check_ff(my_rank, n_scalar, node1%numnod, f1_l)
!      call check_ff(my_rank, n_scalar, node1%numnod, f1_nl)
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_temp, iphys%i_pre_heat, &
     &    nod_fld1%d_fld)
!
      end subroutine cal_temp_pre_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_per_temp_adams
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
!
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_par_temp,               &
     &    iphys%i_pre_heat, nod_fld1%d_fld)
!
      end subroutine cal_per_temp_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_composit_pre_adams
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
!
      call cal_t_evo_4_scalar(iflag_comp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_light,                  &
     &    iphys%i_pre_composit, nod_fld1%d_fld)
!
      end subroutine cal_composit_pre_adams
!
! ----------------------------------------------------------------------
!
      end module evolve_by_adams_bashforth
