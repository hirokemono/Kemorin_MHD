!
!     module precond_djds_vectors
!
!     Written by H. Matsui on June, 2005
!
!      subroutine precond_djds_velo
!      subroutine precond_djds_magne
!
      module precond_djds_vectors
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_parameter
      use m_iccg_parameter
!
      use preconditioning_DJDS33
      use preconditioning_DJDSNN
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_velo
!
      use m_solver_djds_fluid
      use m_velo_matrix
!C
!C== PRECONDITIONING
!
      call precond_DJDS33                                               &
     &         (internal_node, numnod, np_smp, inter_smp_stack,         &
     &          OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                       &
     &          aiccg_velo(im_velo_d), alug_velo_l, alug_velo_u,        &
     &          precond_4_crank, sigma_diag)
!
!      call precond_DJDSnn                                              &
!     &         (internal_node, numnod, n_vector, np_smp,               &
!     &          inter_smp_stack, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,     &
!     &          aiccg_velo(im_velo_d), alug_velo_l, alug_velo_u,       &
!     &          precond_4_crank, sigma_diag)
!
      end subroutine precond_djds_velo
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_magne
!
      use m_solver_djds
      use m_magne_matrix
!
!C
!C== PRECONDITIONING
!
      call precond_DJDS33                                               &
     &         (internal_node, numnod, np_smp, inter_smp_stack,         &
     &          OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                       &
     &          aiccg_magne(im_mag_d), alug_magne_l, alug_magne_u,      &
     &          precond_4_crank, sigma_diag)
!
!      call precond_DJDSnn                                              &
!     &         (internal_node, numnod, n_vector, np_smp,               &
!     &          inter_smp_stack, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,     &
!     &          aiccg_magne(im_mag_d), alug_magne_l, alug_magne_u,     &
!     &          precond_4_crank, sigma_diag)
!
      end subroutine precond_djds_magne
!
! ----------------------------------------------------------------------
!
      end module precond_djds_vectors
