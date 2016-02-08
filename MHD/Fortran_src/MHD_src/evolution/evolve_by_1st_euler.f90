!evolve_by_1st_euler.f90
!      module evolve_by_1st_euler
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!!      subroutine cal_velo_pre_euler(iflag_supg)
!!      subroutine cal_magne_pre_euler(iflag_supg, i_field)
!!      subroutine cal_scalar_pre_euler(iflag_supg, i_field)
!
      module evolve_by_1st_euler
!
      use m_precision
!
      use m_machine_parameter
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
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_euler(iflag_supg)
!
      use cal_multi_pass
      use cal_sol_field_explicit
      use int_vol_coriolis_term
!
      integer(kind = kint), intent(in) :: iflag_supg
!
!
      call cal_t_evo_4_vector(iflag_supg,                               &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node1, mhd_fem1_wk,                     &
     &    iphys%i_velo, nod_fld1, f1_l, f1_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &    (node1, mhd_fem1_wk, iphys, nod_fld1, f1_nl)
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo, nod_fld1%d_fld)
!
      end subroutine cal_velo_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_euler(iflag_supg, i_field)
!
      use cal_sol_field_explicit
      use cal_multi_pass
!
      integer(kind = kint), intent(in) :: iflag_supg, i_field
!
!
      call cal_t_evo_4_vector_cd(iflag_supg,                            &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_conduct_euler                               &
     &   (node1%numnod, conduct1%istack_inter_fld_smp,                  &
     &    conduct1%numnod_fld, conduct1%inod_fld,                       &
     &    mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, i_field, nod_fld1%d_fld)
!
      end subroutine cal_magne_pre_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_pre_euler(iflag_supg, i_field)
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: iflag_supg, i_field
!
!
      call cal_t_evo_4_scalar(iflag_supg, &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, i_field, nod_fld1%d_fld)
!
      end subroutine cal_scalar_pre_euler
!
! ----------------------------------------------------------------------
!
      end module evolve_by_1st_euler
