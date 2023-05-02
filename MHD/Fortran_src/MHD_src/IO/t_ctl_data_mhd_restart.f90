!>@file   t_ctl_data_mhd_restart.f90
!!@brief  module t_ctl_data_mhd_restart
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief data structure for restart data control block
!!
!!@verbatim
!!      subroutine read_restart_ctl(id_control, hd_block, mr_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_restart_control), intent(inout) :: mr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_restart_ctl(id_control, hd_block,              &
!!     &                             mr_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_restart_control), intent(in) :: mr_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine bcast_restart_ctl(mr_ctl)
!!      subroutine reset_restart_ctl(mr_ctl)
!!        type(mhd_restart_control), intent(inout) :: mr_ctl
!! !!!!  control for initial and restart data  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!   no_data:             No initial values
!!   start_from_rst_file: Read restart data as initial values
!!
!!   dynamo_benchmark_0: Initial values for dynamo benchmark Case 0
!!   dynamo_benchmark_1: Initial values for dynamo benchmark Case 1
!!   dynamo_benchmark_2: Initial values for dynamo benchmark Case 1
!!
!!   pseudo_vacuum_benchmark: Initial values for pseudo vacuum benchmark
!!
!!   rotate_x: rotate around x-axis
!!   rotate_y: rotate around y-axis
!!   rotate_z: rotate around z-axis
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin restart_file_ctl
!!     rst_ctl                start_from_rst_file
!!    end restart_file_ctl
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_restart
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!
!>   control flage for restart data
      type mhd_restart_control
        type(read_character_item) :: restart_flag_ctl
!
        integer (kind=kint) :: i_restart_file =   0
      end type mhd_restart_control
!
!    4th level for restart
!
      character(len=kchara), parameter, private                         &
     &                     :: hd_rst_flag = 'rst_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_restart_ctl(id_control, hd_block, mr_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mr_ctl%i_restart_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_rst_flag,                    &
     &      mr_ctl%restart_flag_ctl)
      end do
      mr_ctl%i_restart_file = 1
!
      end subroutine read_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_restart_ctl(id_control, hd_block,                &
     &                             mr_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_restart_control), intent(in) :: mr_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(mr_ctl%i_restart_file .le. 0) return
      maxlen = len_trim(hd_rst_flag)
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &      hd_rst_flag, mr_ctl%restart_flag_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_restart_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_restart_ctl(mr_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
!
!
      call bcast_ctl_type_c1(mr_ctl%restart_flag_ctl)
      call calypso_mpi_bcast_one_int(mr_ctl%i_restart_file, 0)
!
      end subroutine bcast_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_restart_ctl(mr_ctl)
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
!
      mr_ctl%restart_flag_ctl%iflag = 0
      mr_ctl%i_restart_file = 0
!
      end subroutine reset_restart_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_restart
