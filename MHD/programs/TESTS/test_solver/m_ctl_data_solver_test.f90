!
!      module m_ctl_data_solver_test
!
      module m_ctl_data_solver_test
!
!      Written by H. Matsui on June, 2007
!
      use m_precision
!
      use m_ctl_data_4_solvers
!
      implicit  none
!
!
      integer(kind = kint), parameter :: stest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_stest_ctl = "ctl_solver_test"
!
      character(len = kchara) :: matrix_head_ctl =   "matIN"
      character(len = kchara) :: solution_head_ctl = "solution"
!
      integer(kind = kint) :: ip_smp_p_ctl = 1
!
      character(len=kchara) :: solver_type_ctl = 'block33'
!
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
!
      integer (kind=kint) :: i_matrix_head_ctl =    0
      integer (kind=kint) :: i_solution_head_ctl =  0
      integer (kind=kint) :: i_ip_smp_p_ctl =       0
      integer (kind=kint) :: i_plane_ctl =          0
      integer (kind=kint) :: i_solver_type =        0
!
      private :: hd_solver_test_ctl, i_solver_test_ctl
      private :: hd_matrix_head_ctl, hd_solution_head_ctl
      private :: hd_ip_smp_p_ctl,    hd_solver_type
!
      private :: read_ctl_data_test
!
!      subroutine read_control_4_solver_test
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
      use m_parallel_var_dof
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
        call read_crs_solver_param_ctl(ierr)
        if(ierr .gt. 0) call parallel_abort(ierr, e_message)
!
        call read_DJDS_solver_param_ctl
!
!
        call read_character_ctl_item(hd_matrix_head_ctl,                &
     &        i_matrix_head_ctl, matrix_head_ctl)
        call read_character_ctl_item(hd_solution_head_ctl,              &
     &        i_solution_head_ctl, solution_head_ctl)
        call read_character_ctl_item(hd_solver_type,                    &
     &        i_solver_type, solver_type_ctl)
!
        call read_integer_ctl_item(hd_ip_smp_p_ctl,                     &
     &        i_ip_smp_p_ctl, ip_smp_p_ctl)
      end do
!
      end subroutine read_ctl_data_test
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_solver_test
