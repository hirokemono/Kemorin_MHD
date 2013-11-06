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
      use m_control_parameter
      use m_solver_djds_MHD
!
      use precond_djds_field
!
!C
!C +-----------------+
!C | preconditioning |
!C +-----------------+
!C===
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call precond_djds_press
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call precond_djds_velo
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call precond_djds_temp
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call precond_djds_composition
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_djds_mag_potential
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_djds_magne
      end if
!
      end subroutine matrix_precondition
!
!-----------------------------------------------------------------------
!
      end module precond_djds_MHD
