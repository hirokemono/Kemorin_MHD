!>@file   t_ctl_data_solver_test.f90
!!@brief  module t_ctl_data_solver_test
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine read_control_4_solver_test
!!@endverbatim
!
      module t_ctl_data_solver_test
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_solvers
      use t_control_array_integer
      use t_control_array_character
!
      implicit  none
!
!
      integer(kind = kint), parameter :: stest_ctl_file_code = 11
!
      type ctl_data_solver_test
!>        File prefix for matrix data
        type(read_character_item) :: matrix_head_ctl
!>        File prefix for solution data
        type(read_character_item) :: solution_head_ctl
!>        Number of SMP
        type(read_integer_item) :: ip_smp_p_ctl
!>        Solver type
        type(read_character_item) :: solver_type_ctl
!>        Structure for CG solver control
        type(solver_control) :: CG_test_ctl
!
        integer (kind=kint) :: i_solver_test_ctl = 0
      end type ctl_data_solver_test
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_solver_test_ctl = 'solver_test_ctl'
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_matrix_head_ctl =   'matrix_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_solution_head_ctl = 'solution_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ip_smp_p_ctl =     'num_smp_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_solver_type =      'solver_type'
      character(len=kchara), parameter                                  &
     &         :: hd_solver_ctl =     'solver_ctl'
!
      private :: hd_solver_test_ctl
      private :: hd_matrix_head_ctl, hd_solution_head_ctl
      private :: hd_ip_smp_p_ctl, hd_solver_type, hd_solver_ctl
!
      private :: read_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_solver_test(file_name, solvertest_c)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_solver_test), intent(inout) :: solvertest_c
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open(stest_ctl_file_code, file=file_name, status='old')
!
      do
        call load_one_line_from_control(stest_ctl_file_code, c_buf1)
        call read_ctl_data_test(stest_ctl_file_code,                    &
     &      hd_solver_test_ctl, solvertest_c, c_buf1)
        if(solvertest_c%i_solver_test_ctl .gt. 0) exit
      end do
      close(stest_ctl_file_code)
!
      end subroutine read_control_4_solver_test
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_test                                     &
     &         (id_control, hd_block, solvertest_c, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_solver_test), intent(inout) :: solvertest_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(solvertest_c%i_solver_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_CG_solver_param_ctl                                   &
     &     (id_control, hd_solver_ctl, solvertest_c%CG_test_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_matrix_head_ctl,             &
     &      solvertest_c%matrix_head_ctl)
        call read_chara_ctl_type(c_buf, hd_solution_head_ctl,           &
     &      solvertest_c%solution_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_solver_type, solvertest_c%solver_type_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ip_smp_p_ctl, solvertest_c%ip_smp_p_ctl)
      end do
      solvertest_c%i_solver_test_ctl = 1
!
      end subroutine read_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      subroutine reset_ctl_data_test(solvertest_c)
!
      type(ctl_data_solver_test), intent(inout) :: solvertest_c
!
!
      solvertest_c%i_solver_test_ctl = 0
!
      end subroutine reset_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_solver_test
