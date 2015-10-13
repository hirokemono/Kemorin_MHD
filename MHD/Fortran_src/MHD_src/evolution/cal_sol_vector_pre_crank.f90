!
!      module cal_sol_vector_pre_crank
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_sol_velo_pre_linear
!!      subroutine cal_sol_temp_linear
!!      subroutine cal_sol_par_temp_linear
!!      subroutine cal_sol_vect_p_pre_linear
!!      subroutine cal_sol_magne_pre_linear
!!      subroutine cal_sol_d_scalar_linear
!!
!!      subroutine cal_sol_velo_pre_consist
!!      subroutine cal_sol_temp_consist
!!      subroutine cal_sol_vect_p_pre_consist
!!      subroutine cal_sol_d_scalar_consist
!
      module cal_sol_vector_pre_crank
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
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
      subroutine cal_sol_velo_pre_linear
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_velo,                   &
     &    iphys%i_pre_mom, nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_linear
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_temp,                   &
     &    iphys%i_pre_heat, nod_fld1%d_fld)
!
      end subroutine cal_sol_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_par_temp_linear
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_par_temp,               &
     &    iphys%i_pre_heat, nod_fld1%d_fld)
!
      end subroutine cal_sol_par_temp_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_linear
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vec_conduct_linear                                   &
     &   (node1%numnod, node1%istack_internal_smp, inter_cd_smp_stack,  &
     &    numnod_conduct, inod_conduct, mhd_fem1_wk%ml_o_cd,            &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_vecp, iphys%i_pre_uxb,  &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_pre_linear
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_magne_pre_linear
!
      use m_geometry_data_MHD
      use m_int_vol_data
!
!
      call cal_sol_vec_conduct_linear                                   &
     &   (node1%numnod, node1%istack_internal_smp, inter_cd_smp_stack,  &
     &    numnod_conduct, inod_conduct, mhd_fem1_wk%ml_o_cd,            &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_magne, iphys%i_pre_uxb, &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_magne_pre_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_linear
!
!
      call cal_sol_vec_fluid_linear(node1%numnod, node1%istack_nod_smp, &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_light,                  &
     &    iphys%i_pre_composit, nod_fld1%d_fld)
!
      end subroutine cal_sol_d_scalar_linear
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_consist
!
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_velo,           &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_pre_mom,                &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_velo_pre_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temp_consist
!
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_temp,           &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_pre_heat,               &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_temp_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_consist
!
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_magne,          &
     &    nod_fld1%ntot_phys, n_vector, iphys%i_pre_uxb,                &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_vect_p_pre_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_consist
!
      use m_physical_property
!
!
      call cal_sol_vec_pre_consist                                      &
     &   (node1%numnod, node1%istack_internal_smp, coef_light,          &
     &    nod_fld1%ntot_phys, n_scalar, iphys%i_pre_composit,           &
     &    nod_fld1%d_fld)
!
      end subroutine cal_sol_d_scalar_consist
!
! -----------------------------------------------------------------------
!
      end module cal_sol_vector_pre_crank
