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
!!      subroutine read_fem_mhd_control(hd_block, iflag, fmctl_ctl)
!!      subroutine bcast_fem_mhd_control(fmctl_ctl)
!!        type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!!@endverbatim
!
      module t_ctl_data_FEM_MHD_control
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_time_steps
      use t_ctl_data_mhd_evo_scheme
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
      end type fem_mhd_control_control
!
!    label for entry of group
!
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
      integer (kind=kint) :: i_tstep =      0
      integer (kind=kint) :: i_restart_file =   0
      integer (kind=kint) :: i_solver_ctl =     0
      integer (kind=kint) :: i_int_points = 0
      integer (kind=kint) :: i_time_loop =      0
!
!
      private :: hd_time_step, i_tstep
      private :: hd_restart_file, i_restart_file
      private :: hd_time_loop, i_time_loop
      private :: hd_solver_ctl, i_solver_ctl
      private :: hd_int_points, i_int_points
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control(hd_block, iflag, fmctl_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
!
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, fmctl_ctl%tctl)
        call read_restart_ctl                                           &
     &     (hd_restart_file, i_restart_file, fmctl_ctl%mrst_ctl)
        call read_control_fem_int_points                                &
     &     (hd_int_points, i_int_points, fmctl_ctl%fint_ctl)
!
        call read_CG_solver_param_ctl                                   &
     &     (hd_solver_ctl, i_solver_ctl, fmctl_ctl%CG_ctl)
        call read_time_loop_ctl                                         &
     &     (hd_time_loop, i_time_loop, fmctl_ctl%mevo_ctl)
      end do
!
      end subroutine read_fem_mhd_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_control(fmctl_ctl)
!
      use bcast_4_time_step_ctl
      use bcast_4_solver_ctl
      use bcast_4_fem_int_pts_ctl
!
      type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!
!
      call bcast_restart_ctl(fmctl_ctl%mrst_ctl)
      call bcast_time_loop_ctl(fmctl_ctl%mevo_ctl)
      call bcast_ctl_data_4_time_step(fmctl_ctl%tctl)
!
      call bcast_CG_solver_param_ctl(fmctl_ctl%CG_ctl)
      call bcast_control_fem_int_points(fmctl_ctl%fint_ctl)
!
      end subroutine bcast_fem_mhd_control
!
!   --------------------------------------------------------------------
!
!
      end module t_ctl_data_FEM_MHD_control
