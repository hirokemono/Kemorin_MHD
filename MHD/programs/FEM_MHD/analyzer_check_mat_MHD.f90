!analyzer_check_mat_MHD.f90
!      module analyzer_check_mat_MHD
!..................................................
!
      module analyzer_check_mat_MHD
!
!      Written by H. Matsui and H. Okuda
!      modified by H. Matsui on June, 2005 
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use FEM_check_MHD_matrices
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
      call input_control_4_MHD
!
      call FEM_check_MHD_mat
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
