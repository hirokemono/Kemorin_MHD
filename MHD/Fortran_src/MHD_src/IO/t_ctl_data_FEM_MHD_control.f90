!>@file   t_ctl_data_FEM_MHD_control.f90
!!@brief  module t_ctl_data_FEM_MHD_control
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_fem_mhd_control                                 &
!!     &         (id_control, hd_block, fmctl_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_fem_mhd_control                                &
!!     &         (id_control, hd_block, fmctl_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fem_mhd_control_control), intent(in) :: fmctl_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_fem_mhd_control(fmctl_ctl)
!!        type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!!@endverbatim
!
      module t_ctl_data_FEM_MHD_control
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_time_steps
      use t_ctl_data_mhd_evo_scheme
      use t_ctl_data_mhd_restart
      use t_ctl_data_4_solvers
      use t_ctl_data_4_fem_int_pts
!
      use skip_comment_f
!
      implicit none
!
      type fem_mhd_control_control
!>        Structure for time stepping control
        type(time_data_control) :: tctl
!>        Structure for restart flag
        type(mhd_restart_control) :: mrst_ctl
!>        Structures for time integration controls
        type(mhd_evo_scheme_control) :: mevo_ctl
!>        Structure for CG solver control
        type(solver_control) :: CG_ctl
!>        integeration points
        type(fem_intergration_control)  :: fint_ctl
!
        integer (kind=kint) :: i_control = 0
      end type fem_mhd_control_control
!
!    label for entry of group
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_restart_file =   'restart_file_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_int_points = 'intg_point_num_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_loop =      'time_loop_ctl'
!
      private :: hd_time_step, hd_restart_file, hd_time_loop
      private :: hd_solver_ctl, hd_int_points
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control                                   &
     &         (id_control, hd_block, fmctl_ctl, c_buf)
!
      use ctl_data_4_time_steps_IO
      use ctl_data_mhd_evo_scheme_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(fmctl_ctl%i_control .gt. 0) return
      call init_ctl_time_step_label(hd_time_step, fmctl_ctl%tctl)
      call init_restart_ctl_label(hd_restart_file, fmctl_ctl%mrst_ctl)
      call init_time_loop_ctl_label(hd_time_loop, fmctl_ctl%mevo_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, fmctl_ctl%tctl, c_buf)
        call read_restart_ctl                                           &
     &     (id_control, hd_restart_file, fmctl_ctl%mrst_ctl, c_buf)
        call read_control_fem_int_points                                &
     &     (id_control, hd_int_points, fmctl_ctl%fint_ctl, c_buf)
!
        call read_CG_solver_param_ctl                                   &
     &     (id_control, hd_solver_ctl, fmctl_ctl%CG_ctl, c_buf)
        call read_time_loop_ctl                                         &
     &     (id_control, hd_time_loop, fmctl_ctl%mevo_ctl, c_buf)
      end do
      fmctl_ctl%i_control = 0
!
      end subroutine read_fem_mhd_control
!
!   --------------------------------------------------------------------
!
      subroutine write_fem_mhd_control                                  &
     &         (id_control, hd_block, fmctl_ctl, level)
!
      use ctl_data_4_time_steps_IO
      use ctl_data_mhd_evo_scheme_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fem_mhd_control_control), intent(in) :: fmctl_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(fmctl_ctl%i_control .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_time_step_data                                 &
     &   (id_control, fmctl_ctl%tctl, level)
      call write_restart_ctl(id_control, fmctl_ctl%mrst_ctl, level)
      call write_control_fem_int_points                                 &
     &   (id_control, hd_int_points, fmctl_ctl%fint_ctl, level)
!
      call write_CG_solver_param_ctl                                    &
     &   (id_control, hd_solver_ctl, fmctl_ctl%CG_ctl, level)
      call write_time_loop_ctl(id_control, fmctl_ctl%mevo_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_fem_mhd_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_fem_mhd_control(fmctl_ctl)
!
      use calypso_mpi_int
      use bcast_4_time_step_ctl
      use bcast_4_solver_ctl
      use bcast_4_fem_int_pts_ctl
!
      type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!
!
      call reset_restart_ctl(fmctl_ctl%mrst_ctl)
      call reset_time_loop_ctl(fmctl_ctl%mevo_ctl)
      call reset_ctl_data_4_time_step(fmctl_ctl%tctl)
!
      call dealloc_CG_solver_param_ctl(fmctl_ctl%CG_ctl)
      call reset_control_fem_int_points(fmctl_ctl%fint_ctl)
!
      fmctl_ctl%i_control = 0
!
      end subroutine dealloc_fem_mhd_control
!
!   --------------------------------------------------------------------
!
!
      end module t_ctl_data_FEM_MHD_control
