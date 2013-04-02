!
!     module precond_djds_scalars
!
!     Written by H. Matsui on June, 2005
!
!      subroutine precond_djds_press
!      subroutine precond_djds_temp
!      subroutine precond_djds_d_scalar
!      subroutine precond_djds_mag_potential
!      subroutine precond_djds_mag_p_ins
!      subroutine precond_djds_mag_p_cd
!
      module precond_djds_scalars
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_geometry_parameter
      use m_iccg_parameter
!
      use preconditioning_DJDS11
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_press
!
      use m_solver_djds_linear_fl
      use m_press_matrix
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', precond_4_solver, sigma_diag
      call precond_DJDS11                                               &
     &         (internal_node, numnod, NLmax1, itotal1_fl_l, NHYP1,     &
     &          np_smp, inter_smp_stack, STACKmc1, NLmaxHYP1, IVECT1,   &
     &          OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, LtoU1,              &
     &          aiccg_press(im_press_d), indexDJDS1_L, itemDJDS1_L,     &
     &          aiccg_press(im_press_l), alug_press_l, alug_press_u,    &
     &          precond_4_solver, sigma_diag)
!
!
      end subroutine precond_djds_press
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_temp
!
      use m_solver_djds_fluid
      use m_temp_matrix
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', precond_4_solver, sigma_diag
      call precond_DJDS11                                               &
     &         (internal_node, numnod, NLmax, itotal_fl_l, NHYP,        &
     &          np_smp, inter_smp_stack, STACKmc, NLmaxHYP, IVECT,      &
     &          OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU,                 &
     &          aiccg_temp(im_temp_d), indexDJDS_l, itemDJDS_L,         &
     &          aiccg_temp(im_temp_l), alug_temp_l, alug_temp_u,        &
     &          precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_temp
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_d_scalar
!
      use m_solver_djds_fluid
      use m_light_element_matrix
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', precond_4_solver, sigma_diag
      call precond_DJDS11                                               &
     &         (internal_node, numnod, NLmax, itotal_fl_l, NHYP,        &
     &          np_smp, inter_smp_stack, STACKmc, NLmaxHYP, IVECT,      &
     &          OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU,                 &
     &          aiccg_composit(im_cps_d), indexDJDS_l, itemDJDS_L,      &
     &          aiccg_composit(im_cps_l), ALUG_composit_l,              &
     &          ALUG_composit_u, precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_d_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_djds_mag_potential
!
      use m_solver_djds_linear
      use m_mag_potential_matrix
!
!C
!C== PRECONDITIONING
!
!
      call precond_DJDS11                                               &
     &         (internal_node, numnod, NLmax1, itotal1_l, NHYP1,        &
     &          np_smp, inter_smp_stack, STACKmc1, NLmaxHYP1, IVECT1,   &
     &          OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, LtoU1,              &
     &          aiccg_mag_p(im_mp_d), indexDJDS1_L, itemDJDS1_L,        &
     &          aiccg_mag_p(im_mp_l), alug_mag_p_l, alug_mag_p_u,       &
     &          precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_mag_potential
!
! ----------------------------------------------------------------------
!
!      subroutine precond_djds_mag_p_ins
!
!      use m_solver_djds_linear_ins
!      use m_mag_potential_matrix
!
!C
!C== PRECONDITIONING
!
!      call precond_DJDS11                                              &
!     &         (internal_node, numnod, NLmax1, itotal1_ins_l, NHYP1,   &
!     &          np_smp, inter_smp_stack, STACKmc1, NLmaxHYP1, IVECT1,  &
!     &          OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, LtoU1,             &
!     &          aiccg_mag_pi(im_mpi_d), indexDJDS1_L, itemDJDS1_L,     &
!     &          aiccg_mag_pi(im_mpi_l), alug_mag_pi_l, alug_mag_pi_u,  &
!     &          precond_4_solver, sigma_diag)
!
!      end subroutine precond_djds_mag_p_ins
!
! ----------------------------------------------------------------------
!
!      subroutine precond_djds_mag_p_cd
!
!      use m_solver_djds_linear_cd
!      use m_mag_potential_matrix
!
!!C
!C== PRECONDITIONING
!
!      call precond_DJDS11                                              &
!     &         (internal_node, numnod, NLmax1, itotal1_cd_l, NHYP1,    &
!     &          np_smp, inter_smp_stack, STACKmc1, NLmaxHYP1, IVECT1,  &
!     &          OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, LtoU1,             &
!     &          aiccg_mag_pc(im_mpc_d), indexDJDS1_L, itemDJDS1_L,     &
!     &          aiccg_mag_pc(im_mpc_l), alug_mag_pc_l, alug_mag_pc_u,  &
!     &          precond_4_solver, sigma_diag)
!
!      end subroutine precond_djds_mag_p_cd
!
! ----------------------------------------------------------------------
!
      end module precond_djds_scalars
