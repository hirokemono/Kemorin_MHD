!precond_djds_MHD.f90
!     module precond_djds_MHD
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2007
!
!      subroutine matrix_precondition
!
      module precond_djds_MHD
!
      use m_precision
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
      use calypso_mpi
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_finite_element_matrix
      use m_sorted_node
      use m_solver_djds
!
      use precond_djds_vectors
      use precond_djds_scalars
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
        call precond_djds_d_scalar
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
