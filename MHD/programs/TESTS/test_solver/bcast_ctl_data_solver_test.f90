!>@file   bcast_ctl_data_solver_test.f90
!!@brief  module bcast_ctl_data_solver_test
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine load_control_4_solver_test(solvertest_c)
!!        type(ctl_data_solver_test), intent(inout) :: solvertest_c
!!@endverbatim
!
      module bcast_ctl_data_solver_test
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_solver_test
      use calypso_mpi
!
      implicit  none
!
!
      character(len = kchara), parameter                                &
     &                        :: fname_stest_ctl = "ctl_solver_test"
!
      private :: bcast_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_4_solver_test(solvertest_c)
!
      use skip_comment_f
!
      type(ctl_data_solver_test), intent(inout) :: solvertest_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        call read_control_4_solver_test(fname_stest_ctl, solvertest_c)
      end if
!
      call bcast_ctl_data_test(solvertest_c)
!
      end subroutine load_control_4_solver_test
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
      end module bcast_ctl_data_solver_test
