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
      use m_parallel_var_dof
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
      use m_ctl_data_fem_MHD
      use set_control_MHD
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call read_control_4_fem_MHD
      call set_control
!
      call time_prog_barrier
!
      call FEM_check_MHD_mat
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
