!analyzer_test_crs.f90
!
!      module analyzer_test_crs
!..................................................
!
!      modified by H. Matsui on June, 2007
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_test_crs
!
      use m_precision
!
      use t_geometry_data
      use t_comm_table
      use t_crs_matrix
      use t_iccg_parameter
      use t_ctl_data_solver_test
      use t_solver_SR
!
      implicit none
!
      type(ctl_data_solver_test), save :: solvertest_c1
      type(communication_table), save :: nod_comm
      type(node_data), save :: node
      type(CRS_matrix_connect), save :: tbl_crs
      type(CRS_matrix), save :: mat_crs
!
      type(CG_poarameter), save :: CG_param_t
      type(DJDS_poarameter), save :: DJDS_param_t
!
!>      Structure of communication flags
      type(send_recv_status) :: SR_sig1
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer) :: SR_r1
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
      use calypso_mpi
      use set_control_solver_test
!
      use crs_matrix_io
!
!     --------------------- 
!
!C-- CNTL DATA
!
      call read_control_4_solver_test(solvertest_c1)
      call set_ctl_params_4_solver_test                                 &
     &   (solvertest_c1, mat_crs, CG_param_t, DJDS_param_t)
!
!C 
!C +-------------+
!C | MATRIX file |
!C +-------------+
!C===
      call read_matrix_file(nod_comm, node, tbl_crs, mat_crs)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use crs_matrix_io
      use solve_by_crs_solver
!
!C
!C-- ICCG computation

      if (mat_crs%SOLVER_crs .eq. 'scalar'                              &
     &    .or. mat_crs%SOLVER_crs.eq.'SCALAR') then
        call solve_by_crs_solver11(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1)
      else if (mat_crs%SOLVER_crs.eq.'block33'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCK33') then
        call solve_by_crs_solver33(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1)
      else if (mat_crs%SOLVER_crs.eq.'blockNN'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCKNN') then
        call solve_by_crs_solverNN(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1)
      end if

      call output_solution(node, mat_crs)

      if (my_rank.eq.0) write (*,*) mat_crs%ITERactual, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)

      if (my_rank.eq.0) then
        RTIME= ENDTIME-STARTTIME
        write (*, '("*** ELAPCE TIME", 1pe16.6, " sec.")') RTIME
      endif
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_crs
