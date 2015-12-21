!
!      module cal_sol_vector_explicit
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_pre_euler
!      subroutine cal_sol_temp_euler
!      subroutine cal_sol_part_temp_euler
!      subroutine cal_sol_vect_p_pre_euler
!      subroutine cal_sol_magne_pre_euler
!      subroutine cal_sol_d_scalar_euler
!
!      subroutine cal_sol_velo_pre_adams
!      subroutine cal_sol_temp_adams
!      subroutine cal_sol_part_temp_adams
!      subroutine cal_sol_vect_p_pre_adams
!      subroutine cal_sol_magne_pre_adams
!      subroutine cal_sol_d_scalar_adams
!
      module cal_sol_vector_explicit
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
!
      use cal_sol_field_explicit
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_euler
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo, nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_euler
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_temp, nod_fld1%d_fld)
!
      end subroutine cal_sol_temp_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_part_temp_euler
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_par_temp,               &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_part_temp_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_euler
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vect_pre_conduct_euler(node1%numnod,                 &
     &    inter_cd_smp_stack, numnod_conduct, inod_conduct,             &
     &    mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_vecp, nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_euler
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vect_pre_conduct_euler(node1%numnod,                 &
     &   inter_cd_smp_stack, numnod_conduct, inod_conduct,              &
     &   mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                    &
     &   nod_fld1%ntot_phys, n_vector, iphys%i_magne, nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_pre_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_euler
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_light, nod_fld1%d_fld)
!
      end subroutine cal_sol_d_scalar_euler
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_adams
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo,                   &
     &    iphys%i_pre_mom, nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_adams
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_temp, iphys%i_pre_heat, &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_temp_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_part_temp_adams
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_par_temp,               &
     &    iphys%i_pre_heat, nod_fld1%d_fld)
!
      end subroutine cal_sol_part_temp_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_adams
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vect_pre_conduct_adams(node1%numnod,                 &
     &   inter_cd_smp_stack, numnod_conduct, inod_conduct,              &
     &   mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                    &
     &   nod_fld1%ntot_phys, n_vector, iphys%i_vecp, iphys%i_pre_uxb,   &
     &   nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_adams
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vect_pre_conduct_adams(node1%numnod,                 &
     &    inter_cd_smp_stack, numnod_conduct, inod_conduct,             &
     &    mhd_fem1_wk%mlump_cd%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_magne, iphys%i_pre_uxb, &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_pre_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_adams
!
      use m_int_vol_data
!
      call cal_sol_vect_pre_fluid_adams                                 &
     &   (node1%numnod, node1%istack_internal_smp,                      &
     &    mhd_fem1_wk%mlump_fl%ml, f1_l%ff, f1_nl%ff,                   &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_light,                  &
     &    iphys%i_pre_composit, nod_fld1%d_fld)
!
      end subroutine cal_sol_d_scalar_adams
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_explicit
