!>@file   m_ctl_data_solver_test.f90
!!@brief  module m_ctl_data_solver_test
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
      module m_ctl_data_solver_test
!
      use m_precision
      use t_ctl_data_4_solvers
      use t_control_elements
!
      implicit  none
!
!
      integer(kind = kint), parameter :: stest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_stest_ctl = "ctl_solver_test"
!
!>      File prefix for matrix data
      type(read_character_item), save :: matrix_head_ctl
!>      File prefix for solution data
      type(read_character_item), save :: solution_head_ctl
!
!>      Number of SMP
      type(read_integer_item), save :: ip_smp_p_ctl
!
!>      Solver type
      type(read_character_item), save :: solver_type_ctl
!
!>      Structure for CG solver control
      type(solver_control), save :: CG_test_ctl
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_solver_test_ctl = 'solver_test_ctl'
      integer (kind=kint) :: i_solver_test_ctl = 0
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
      integer (kind=kint) :: i_solver_ctl =     0
!
      private :: hd_solver_test_ctl, i_solver_test_ctl
      private :: hd_matrix_head_ctl, hd_solution_head_ctl
      private :: hd_ip_smp_p_ctl,    hd_solver_type
      private :: hd_solver_ctl, i_solver_ctl
!
      private :: read_ctl_data_test
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_solver_test
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = stest_ctl_file_code
!
      open(ctl_file_code, file=fname_stest_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_data_test
!
      close(ctl_file_code)
!
      end subroutine read_control_4_solver_test
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_test
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_solver_test_ctl) .eq. 0) return
      if (i_solver_test_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_solver_test_ctl,                  &
     &      i_solver_test_ctl)
!
!
        call read_CG_solver_param_ctl                                   &
     &   (hd_solver_ctl, i_solver_ctl, CG_test_ctl)
!
        call read_chara_ctl_type(hd_matrix_head_ctl, matrix_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_solution_head_ctl,solution_head_ctl)
        call read_chara_ctl_type(hd_solver_type, solver_type_ctl)
!
        call read_integer_ctl_type(hd_ip_smp_p_ctl, ip_smp_p_ctl)
      end do
!
      end subroutine read_ctl_data_test
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_solver_test
