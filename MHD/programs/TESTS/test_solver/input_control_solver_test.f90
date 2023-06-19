!>@file   input_control_solver_test.f90
!!@brief  module input_control_solver_test
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine s_input_control_solver_test                          &
!!     &         (ctl_file_name, mat_crs, CG_param, DJDS_param)
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!!@endverbatim
!
      module input_control_solver_test
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_solver_test
      use t_crs_matrix
      use t_iccg_parameter
      use t_ctl_data_solver_test
      use calypso_mpi
!
      implicit  none
!
!
      private :: bcast_ctl_data_test, set_ctl_params_4_solver_test
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_control_solver_test                            &
     &         (ctl_file_name, mat_crs, CG_param, DJDS_param)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
      type(ctl_data_solver_test) :: solvertest_c
!
!
      if(my_rank .eq. 0) then
        call read_control_4_solver_test(ctl_file_name, solvertest_c)
      end if
      call bcast_ctl_data_test(solvertest_c)
!
      if(solvertest_c%i_solver_test_ctl .ne. 1) then
        call calypso_MPI_abort(solvertest_c%i_solver_test_ctl,          &
     &                             'control file is broken')
      end if
!
      call set_ctl_params_4_solver_test                                 &
     &   (solvertest_c, mat_crs, CG_param, DJDS_param)
!
      end subroutine s_input_control_solver_test
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_data_test(solvertest_c)
!
      use calypso_mpi_int
      use bcast_4_solver_ctl
      use bcast_control_arrays
!
      type(ctl_data_solver_test), intent(inout) :: solvertest_c
!
!
      call bcast_CG_solver_param_ctl(solvertest_c%CG_test_ctl)
!
      call bcast_ctl_type_c1(solvertest_c%matrix_head_ctl)
      call bcast_ctl_type_c1(solvertest_c%solution_head_ctl)
      call bcast_ctl_type_i1(solvertest_c%ip_smp_p_ctl)
      call bcast_ctl_type_c1(solvertest_c%solver_type_ctl)
!
      call calypso_mpi_bcast_one_int(solvertest_c%i_solver_test_ctl, 0)
!
      end subroutine bcast_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_params_4_solver_test                           &
     &         (solvertest_c, mat_crs, CG_param, DJDS_param)
!
      use calypso_mpi
      use m_machine_parameter
      use crs_matrix_io
      use skip_comment_f
!
      use set_parallel_file_name
!
      type(ctl_data_solver_test), intent(in) :: solvertest_c
!
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
!
      if(solvertest_c%matrix_head_ctl%iflag .ne. 0) then
        matrix_file_head = solvertest_c%matrix_head_ctl%charavalue
      else
        matrix_file_head = "matIN"
      end if
      matrix_file_name = add_process_id(my_rank, matrix_file_head)
      write(*,*) 'matrix data file: ', matrix_file_name
!
      if(solvertest_c%solution_head_ctl%iflag .ne. 0) then
        solution_file_head = solvertest_c%solution_head_ctl%charavalue
      else
        solution_file_head = "matIN"
      end if
      solution_file_name = add_process_id(my_rank, solution_file_head)
      write(*,*) 'solution data file: ', solution_file_name
!
      if(solvertest_c%ip_smp_p_ctl%iflag .ne. 0) then
        np_smp = solvertest_c%ip_smp_p_ctl%intvalue
      else
        np_smp = 1
      end if
!
!     set solver information
!
      if(solvertest_c%solver_type_ctl%iflag .gt. 0) then
        mat_crs%SOLVER_crs =  solvertest_c%solver_type_ctl%charavalue
      else
        mat_crs%SOLVER_crs = 'block33'
      end if
!
      call set_control_4_CG_solver(solvertest_c%CG_test_ctl, CG_param)
      call set_control_4_DJDS_solver                                    &
     &   (solvertest_c%CG_test_ctl%DJDS_ctl, DJDS_param)
      call copy_from_iccg_parameter(CG_param, mat_crs)
!
      if (iflag_debug .eq. 1) then
        write(*,*) 'np_smp       ', np_smp
        write(*,*) 'SOLVER_crs   ', mat_crs%SOLVER_crs
        write(*,*) 'METHOD_crs   ', mat_crs%METHOD_crs
        write(*,*) 'PRECOND_crs  ', mat_crs%PRECOND_crs
        write(*,*) 'INTARRAY_crs (iteration)', mat_crs%INTARRAY_crs(1)
        write(*,*) 'REALARRAY_crs (eps) ',                              &
     &            mat_crs%REALARRAY_crs(1)
        write(*,*) 'REALARRAY_crs (sigma_diag) ',                       &
     &            mat_crs%REALARRAY_crs(2)
        write(*,*) 'REALARRAY_crs (sigma) ',                            &
     &            mat_crs%REALARRAY_crs(3)
      end if
!
      end subroutine set_ctl_params_4_solver_test
!
!   --------------------------------------------------------------------
!
      end module input_control_solver_test
