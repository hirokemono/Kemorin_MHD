!analyzer_test_djds.f90
!
!      module analyzer_test_djds
!..................................................
!
!      modified by H. Matsui on June, 2007
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_test_djds
!
      use m_precision
!
      implicit none
!
      real(kind = kreal) :: RTIME, STARTTIME, ENDTIME
      private :: RTIME, STARTTIME, ENDTIME
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_parallel_var_dof
      use m_ctl_data_solver_test
      use set_control_solver_test
!
      use crs_matrix_io
!
!
!C-- CNTL DATA

      call read_control_4_solver_test
      call set_ctl_params_4_solver_test
!
!C 
!C +-------------+
!C | MATRIX file |
!C +-------------+
!C===
      call read_matrix_file
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_crs_matrix
      use crs_matrix_io
      use DJDS_precond_solve11
      use DJDS_precond_solve33
      use DJDS_precond_solveNN
!
!
!      call check_crs_matrix_components(my_rank)
!C
!C-- ICCG computation
      if (SOLVER_crs .eq. 'scalar' .or. SOLVER_crs.eq.'SCALAR') then
        call solve_by_djds_solver11
      else if (SOLVER_crs.eq.'block33'                                  &
     &    .or. SOLVER_crs.eq.'BLOCK33') then
        call solve_by_djds_solver33
      else if (SOLVER_crs.eq.'blockNN'                                  &
     &    .or. SOLVER_crs.eq.'BLOCKNN') then
        call solve_by_djds_solverNN
      end if

      call output_solution

      if (my_rank.eq.0) write (*,*) itr_res, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER  (CALYPSO_COMM,ierr)

      if (my_rank.eq.0) then
        RTIME= ENDTIME-STARTTIME
        write (*, '("*** ELAPCE TIME", 1pe16.6, " sec.")') RTIME
      endif
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_djds
