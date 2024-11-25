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
      use m_work_time
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
      use crs_matrix_io
!
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
      use solve_precond_DJDS
      use copy_matrix_2_djds_array
!
      use t_solver_djds
!
      type(DJDS_ordering_table) :: djds_tbl
      type(DJDS_MATRIX) :: djds_mat
!
      real(kind = kreal) :: INITtime, PRECtime
      real(kind = kreal) :: COMPtime, COMMtime
      integer(kind = kint) :: itr_res, ierr
!
      logical :: flag_DJDS_TEST = .FALSE.
      integer(kind = kint) :: ist_elapsed_DJDS_TEST =  0
      integer(kind = kint) :: ied_elapsed_DJDS_TEST =  0
      integer(kind = kint), parameter :: num_append = 4
!
      call init_elapse_time_by_TOTAL
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_DJDS_TEST, ied_elapsed_DJDS_TEST)
!
      elps1%labels(ist_elapsed_DJDS_TEST+1)= 'initialization time '
      elps1%labels(ist_elapsed_DJDS_TEST+2)= 'precondition time   '
      elps1%labels(ist_elapsed_DJDS_TEST+3)= 'main time           '
      elps1%labels(ist_elapsed_DJDS_TEST+4)= 'communication time  '
      flag_DJDS_TEST = .TRUE.
!
!      call check_crs_matrix_comps(my_rank, tbl_crs, mat_crs)
!C
!C-- ICCG computation
      call transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs, mat_crs, &
     &    CG_param_t, DJDS_param_t, djds_tbl, djds_mat)
!
      if (mat_crs%SOLVER_crs .eq. 'scalar'                              &
     &   .or. mat_crs%SOLVER_crs.eq.'SCALAR') then
        call solve_by_djds_solver11(node, nod_comm, CG_param_t,         &
     &      mat_crs, djds_tbl, djds_mat, SR_sig1, SR_r1, itr_res, ierr, &
     &      INITtime, PRECtime, COMPtime, COMMtime)
      else if (mat_crs%SOLVER_crs.eq.'block33'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCK33') then
        call solve_by_djds_solver33(node, nod_comm, CG_param_t,         &
     &      mat_crs, djds_tbl, djds_mat, SR_sig1, SR_r1, itr_res, ierr, &
     &      INITtime, PRECtime, COMPtime, COMMtime)
      else if (mat_crs%SOLVER_crs.eq.'blockNN'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCKNN') then
        call solve_by_djds_solverNN(node, nod_comm, CG_param_t,         &
     &      mat_crs, djds_tbl, djds_mat, SR_sig1, SR_r1, itr_res, ierr, &
     &      INITtime, PRECtime, COMPtime, COMMtime)
      end if

      if(flag_DJDS_TEST) then
        elps1%elapsed(ist_elapsed_DJDS_TEST+1)                          &
     &        = elps1%elapsed(ist_elapsed_DJDS_TEST+1) + INITtime
        elps1%elapsed(ist_elapsed_DJDS_TEST+2)                          &
     &        = elps1%elapsed(ist_elapsed_DJDS_TEST+2) + INITtime
        elps1%elapsed(ist_elapsed_DJDS_TEST+3)                          &
     &        = elps1%elapsed(ist_elapsed_DJDS_TEST+3) + COMPtime
        elps1%elapsed(ist_elapsed_DJDS_TEST+4)                          &
     &        = elps1%elapsed(ist_elapsed_DJDS_TEST+4) + COMMtime
      end if
!
      call output_solution(node, mat_crs)

      if (my_rank.eq.0) write (*,*) itr_res, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)

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
