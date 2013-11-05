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
      use calypso_mpi
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
     &          Vmat_DJDS%D, Vmat_DJDS%ALUG_L, Vmat_DJDS%ALUG_U,        &
     &          precond_4_crank, sigma_diag)
!
!      call precond_DJDSnn                                              &
!     &         (internal_node, numnod, n_vector, np_smp,               &
!     &          inter_smp_stack, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,     &
!     &          Vmat_DJDS%D, Vmat_DJDS%ALUG_L, Vmat_DJDS%ALUG_U,       &
!     &          precond_4_crank, sigma_diag)
!
      end subroutine precond_djds_velo
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_magne
!
      use m_solver_djds_MHD
      use m_magne_matrix
      use solver_DJDS33_struct
      use solver_DJDSnn_struct
!
!C
!C== PRECONDITIONING
!
      call precond_DJDS33_struct(np_smp, DJDS_entire, Bmat_DJDS,        &
     &    precond_4_crank, sigma_diag)
!
!      call precond_DJDSnn_struct(n_vector, np_smp,                     &
!     &    DJDS_entire, Bmat_DJDS, precond_4_crank, sigma_diag)
!
      end subroutine precond_djds_magne
!
! ----------------------------------------------------------------------
!
      end module precond_djds_vectors
