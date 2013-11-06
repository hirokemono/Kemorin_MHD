!
!     module precond_djds_scalars
!
!     Written by H. Matsui on June, 2005
!
!      subroutine precond_djds_press
!      subroutine precond_djds_temp
!      subroutine precond_djds_composition
!      subroutine precond_djds_mag_potential
!
      module precond_djds_scalars
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_parameter
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
      end module precond_djds_scalars
