!>@file   precond_djds_MHD.f90
!!@brief  module precond_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>     Preconditiong of DJDS solver for MHD dynamo
!!
!!@verbatim
!!      subroutine matrix_precondition
!!
!!      subroutine precond_djds_velo
!!      subroutine precond_djds_magne
!!
!!      subroutine precond_djds_press
!!      subroutine precond_djds_temp
!!      subroutine precond_djds_composition
!!      subroutine precond_djds_mag_potential
!!@endverbatim
!
      module precond_djds_MHD
!
      use m_precision
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine matrix_precondition
!
      use m_machine_parameter
      use m_phys_constants
!
      use m_iccg_parameter
      use m_control_parameter
      use m_solver_djds_MHD
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_DJDSnn_struct
!
      use preconditioning_DJDS11
!
!C
!C +-----------------+
!C | preconditioning |
!C +-----------------+
!C===
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1)   write(*,*) 'precond: ',                 &
     &                trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp, DJDS_fl_l, Pmat_DJDS,        &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call precond_DJDS33_struct(np_smp, DJDS_fluid, Vmat_DJDS,       &
     &      precond_4_crank, sigma_diag)
!        call precond_DJDSnn_struct(n_vector, np_smp,                   &
!     &      DJDS_fluid, Vmat_DJDS, precond_4_crank, sigma_diag)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        if (iflag_debug.eq.1)  write(*,*) 'precond: ',                  &
     &          trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp, DJDS_fluid, Tmat_DJDS,       &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        if (iflag_debug.eq.1)  write(*,*) 'precond: ',                  &
     &         trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp, DJDS_fluid, Cmat_DJDS,       &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, DJDS_linear, Fmat_DJDS,      &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_DJDS33_struct(np_smp, DJDS_entire, Bmat_DJDS,      &
     &      precond_4_crank, sigma_diag)
!        call precond_DJDSnn_struct(n_vector, np_smp,                   &
!       &    DJDS_entire, Bmat_DJDS, precond_4_crank, sigma_diag)
      end if
!
      end subroutine matrix_precondition
!
!-----------------------------------------------------------------------
!
      end module precond_djds_MHD
