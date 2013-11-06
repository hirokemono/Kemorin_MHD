!>@file   precond_djds_field.f90
!!@brief  module precond_djds_field
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>     Preconditiong of DJDS solver for MHD dynamo
!!
!!@verbatim
!!      subroutine precond_djds_velo
!!      subroutine precond_djds_magne
!!
!!      subroutine precond_djds_press
!!      subroutine precond_djds_temp
!!      subroutine precond_djds_composition
!!      subroutine precond_djds_mag_potential
!!@endverbatim
!
      module precond_djds_field
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
      use m_iccg_parameter
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
      use m_solver_djds_MHD
      use m_velo_matrix
      use solver_DJDS33_struct
      use solver_DJDSnn_struct
!C
!C== PRECONDITIONING
!
      call precond_DJDS33_struct(np_smp, DJDS_fluid, Vmat_DJDS,         &
     &    precond_4_crank, sigma_diag)
!
!      call precond_DJDSnn_struct(n_vector, np_smp,                     &
!     &    DJDS_fluid, Vmat_DJDS, precond_4_crank, sigma_diag)
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
! ----------------------------------------------------------------------
!
      subroutine precond_djds_press
!
      use m_solver_djds_MHD
      use m_velo_matrix
!
      use solver_DJDS11_struct
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', trim(precond_4_solver),' ', sigma_diag
      call precond_DJDS11_struct(np_smp, DJDS_fl_l, Pmat_DJDS,          &
     &    precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_press
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_temp
!
      use m_solver_djds_MHD
      use m_temp_matrix
!
      use solver_DJDS11_struct
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', trim(precond_4_solver),' ', sigma_diag
      call precond_DJDS11_struct(np_smp, DJDS_fluid, Tmat_DJDS,         &
     &    precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_temp
!
! ----------------------------------------------------------------------
!
      subroutine precond_djds_composition
!
      use m_solver_djds_MHD
      use m_light_element_matrix
!
      use solver_DJDS11_struct
!
!C
!C== PRECONDITIONING
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'precond: ', trim(precond_4_solver),' ', sigma_diag
      call precond_DJDS11_struct(np_smp, DJDS_fluid, Cmat_DJDS,         &
     &    precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_composition
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_djds_mag_potential
!
      use m_solver_djds_MHD
      use m_magne_matrix
      use solver_DJDS11_struct
!
      use preconditioning_DJDS11
!
!C
!C== PRECONDITIONING
!
      call precond_DJDS11_struct(np_smp, DJDS_linear, Fmat_DJDS,        &
     &    precond_4_solver, sigma_diag)
!
      end subroutine precond_djds_mag_potential
!
! ----------------------------------------------------------------------
!
      end module precond_djds_field
