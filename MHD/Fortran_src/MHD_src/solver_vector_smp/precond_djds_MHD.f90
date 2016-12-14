!>@file   precond_djds_MHD.f90
!!@brief  module precond_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>     Preconditiong of DJDS solver for MHD dynamo
!!
!!@verbatim
!!      subroutine matrix_precondition                                  &
!!     &         (Vmatrix, Pmatrix, Bmatrix, Fmatrix, Tmatrix, Cmatrix)
!!        type(MHD_MG_matrix), intent(inout) :: Vmatrix, Bmatrix
!!        type(MHD_MG_matrix), intent(inout) :: Pmatrix, Fmatrix
!!        type(MHD_MG_matrix), intent(inout) :: Tmatrix, Cmatrix
!!@endverbatim
!
      module precond_djds_MHD
!
      use m_precision
      use calypso_mpi
!
      use t_solver_djds_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine matrix_precondition                                    &
     &         (Vmatrix, Pmatrix, Bmatrix, Fmatrix, Tmatrix, Cmatrix)
!
      use m_machine_parameter
      use m_phys_constants
!
      use m_iccg_parameter
      use m_control_parameter
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_DJDSnn_struct
!
      use preconditioning_DJDS11
!
      type(MHD_MG_matrix), intent(inout) :: Vmatrix, Bmatrix
      type(MHD_MG_matrix), intent(inout) :: Pmatrix, Fmatrix
      type(MHD_MG_matrix), intent(inout) :: Tmatrix, Cmatrix
!C
!C +-----------------+
!C | preconditioning |
!C +-----------------+
!C===
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1)   write(*,*) 'precond: ',                 &
     &                trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp,                              &
     &      Pmatrix%MG_DJDS_table(0), Pmatrix%mat_MG_DJDS(0),           &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call precond_DJDS33_struct(np_smp,                              &
     &      Vmatrix%MG_DJDS_table(0), Vmatrix%mat_MG_DJDS(0),           &
     &      precond_4_crank, sigma_diag)
!        call precond_DJDSnn_struct(n_vector, np_smp,                   &
!     &      Vmatrix%MG_DJDS_table(0), Vmatrix%mat_MG_DJDS(0),          &
!     &      precond_4_crank, sigma_diag)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        if (iflag_debug.eq.1)  write(*,*) 'precond: ',                  &
     &          trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp,                              &
     &      Tmatrix%MG_DJDS_table(0), Tmatrix%mat_MG_DJDS(0),           &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
        if (iflag_debug.eq.1)  write(*,*) 'precond: ',                  &
     &         trim(precond_4_solver),' ', sigma_diag
        call precond_DJDS11_struct(np_smp,                              &
     &      Cmatrix%MG_DJDS_table(0), Cmatrix%mat_MG_DJDS(0),           &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution                  &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_DJDS11_struct                                      &
     &     (np_smp, Fmatrix%MG_DJDS_table(0), Fmatrix%mat_MG_DJDS(0),   &
     &      precond_4_solver, sigma_diag)
      end if
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution                  &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call precond_DJDS33_struct                                      &
     &     (np_smp, Bmatrix%MG_DJDS_table(0), Bmatrix%mat_MG_DJDS(0),   &
     &      precond_4_crank, sigma_diag)
!        call precond_DJDSnn_struct(n_vector, np_smp,                   &
!       &    Bmatrix%MG_DJDS_table(0), Bmatrix%mat_MG_DJDS(0),          &
!       &    precond_4_crank, sigma_diag)
      end if
!
      end subroutine matrix_precondition
!
!-----------------------------------------------------------------------
!
      end module precond_djds_MHD
