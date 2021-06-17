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
      use t_geometry_data
      use t_comm_table
      use t_crs_matrix
      use t_iccg_parameter
      use t_ctl_data_solver_test
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
!
!C-- CNTL DATA

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
      use solve_precond_DJDS
      use copy_matrix_2_djds_array
!
      use t_mesh_SR
      use m_solver_SR
      use t_solver_djds
!
      type(DJDS_ordering_table) :: djds_tbl
      type(DJDS_MATRIX) :: djds_mat
      integer(kind = kint) :: itr_res, ierr
!
!      call check_crs_matrix_comps(my_rank, tbl_crs, mat_crs)
!C
!C-- ICCG computation
      call transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs, mat_crs, &
     &    CG_param_t, DJDS_param_t, djds_tbl, djds_mat)
!
      if (mat_crs%SOLVER_crs .eq. 'scalar'                              &
     &   .or. mat_crs%SOLVER_crs.eq.'SCALAR') then
        call solve_by_djds_solver11                                     &
     &     (node, nod_comm, CG_param_t, mat_crs, djds_tbl, djds_mat,    &
     &      SR_sig1, SR_r1, itr_res, ierr)
      else if (mat_crs%SOLVER_crs.eq.'block33'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCK33') then
        call solve_by_djds_solver33                                     &
     &     (node, nod_comm, CG_param_t, mat_crs, djds_tbl, djds_mat,    &
     &      SR_sig1, SR_r1, itr_res, ierr)
      else if (mat_crs%SOLVER_crs.eq.'blockNN'                          &
     &    .or. mat_crs%SOLVER_crs.eq.'BLOCKNN') then
        call solve_by_djds_solverNN                                     &
     &     (node, nod_comm, CG_param_t, mat_crs, djds_tbl, djds_mat,    &
     &      SR_sig1, SR_r1, itr_res, ierr)
      end if

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
