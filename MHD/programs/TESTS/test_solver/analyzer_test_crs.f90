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
      use m_work_time
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_stest_ctl = "ctl_solver_test"
!
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
      logical :: flag_TEST_CRS_time = .FALSE.
      integer(kind = kint) :: ist_elapsed_TEST_CRS = 0
      integer(kind = kint) :: ied_elapsed_TEST_CRS = 0
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
      use input_control_solver_test
!
      use crs_matrix_io
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call init_elapse_time_by_TOTAL
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_TEST_CRS, ied_elapsed_TEST_CRS)
!
      elps1%labels(ist_elapsed_TEST_CRS+ 1) = 'Solver Preconditioning   '
      elps1%labels(ist_elapsed_TEST_CRS+ 2) = 'Solver iteration time    '
      elps1%labels(ist_elapsed_TEST_CRS+ 3) = 'Solver communicatio time '
!
      flag_TEST_CRS_time = .TRUE.
!
!     --------------------- 
!C-- CNTL DATA
      call s_input_control_solver_test                                  &
     &   (fname_stest_ctl, mat_crs, CG_param_t, DJDS_param_t)
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
      real(kind = kreal) :: PRECtime, COMPtime, COMMtime
!C
!C-- ICCG computation

      if (mat_crs%SOLVER_crs .eq. 'scalar'                              &
     &    .or. mat_crs%SOLVER_crs.eq.'SCALAR') then
        call solve_by_crs_solver11(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1,             &
     &                             PRECtime, COMPtime, COMMtime)
      else if (mat_crs%SOLVER_crs.eq.'block33'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCK33') then
        call solve_by_crs_solver33(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1,             &
     &                             PRECtime, COMPtime, COMMtime)
      else if (mat_crs%SOLVER_crs.eq.'blockNN'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCKNN') then
        call solve_by_crs_solverNN(nod_comm, node, tbl_crs,             &
     &                             mat_crs, SR_sig1, SR_r1,             &
     &                             PRECtime, COMPtime, COMMtime)
      end if

      if(flag_TEST_CRS_time) then
        elps1%elapsed(ist_elapsed_TEST_CRS+1) = PRECtime
        elps1%elapsed(ist_elapsed_TEST_CRS+2) = COMPtime
        elps1%elapsed(ist_elapsed_TEST_CRS+3) = COMMtime
      end if
!
      call output_solution(node, mat_crs)

      if (my_rank.eq.0) write (*,*) mat_crs%ITERactual, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER(CALYPSO_COMM,ierr_MPI)

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
