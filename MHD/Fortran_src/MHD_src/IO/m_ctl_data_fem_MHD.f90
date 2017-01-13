!m_ctl_data_fem_MHD.f90
!     module m_ctl_data_fem_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!        Modified by H. Matsui on Oct., 2007
!
!      subroutine read_control_4_fem_MHD
!      subroutine read_control_4_fem_snap
!
      module m_ctl_data_fem_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_solvers
      use t_ctl_data_4_fem_int_pts
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!>      Structure for CG solver control
      type(solver_control), save :: CG_ctl1
!>      integeration points
      type(fem_intergration_control), save  :: fint_ctl1
!
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
!
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_int_points = 'intg_point_num_ctl'
!
      integer (kind=kint) :: i_solver_ctl =     0
      integer (kind=kint) :: i_int_points = 0
!
!
      private :: MHD_ctl_name, snap_ctl_name
      private :: hd_mhd_ctl, i_mhd_ctl
!
      private :: hd_model, hd_control, i_model, i_control
      private :: hd_solver_ctl, i_solver_ctl
      private :: hd_int_points, i_int_points
!
      private :: read_fem_mhd_control_data
      private :: read_fem_mhd_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_fem_MHD
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = MHD_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_fem_mhd_control_data
!
        close(ctl_file_code)
      end if
!
      call bcast_fem_mhd_control_data
!
      end subroutine read_control_4_fem_MHD
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_fem_snap
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = snap_ctl_name, status='old' )
!
        call load_ctl_label_and_line
        call read_fem_mhd_control_data
!
        close(ctl_file_code)
      end if
!
      call bcast_fem_mhd_control_data
!
      end subroutine read_control_4_fem_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control_data
!
      use calypso_mpi
      use m_ctl_data_4_platforms
      use m_control_data_sections
      use m_ctl_data_node_monitor
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_org_data
      use read_ctl_data_sph_MHD
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if (i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mhd_ctl, i_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_sph_mhd_model
        call read_fem_mhd_control
!
        call read_monitor_data_control
        call read_pickup_sph_ctl
        call read_sections_control_data
      end do
!
      end subroutine read_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_fem_mhd_control
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_mhd_evo_scheme
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
!
        call read_time_step_ctl
        call read_restart_control
        call read_control_fem_int_points                                &
     &     (hd_int_points, i_int_points, fint_ctl1)
!
        call read_CG_solver_param_ctl                                   &
     &     (hd_solver_ctl, i_solver_ctl, CG_ctl1)
        call read_time_loop_control
      end do
!
      end subroutine read_fem_mhd_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_control_data
!
      use calypso_mpi
      use m_ctl_data_4_platforms
      use m_control_data_sections
      use m_ctl_data_node_monitor
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_org_data
      use read_ctl_data_sph_MHD
      use bcast_4_platform_ctl
      use bcast_4_sph_monitor_ctl
!
!
      call bcast_ctl_data_4_platform(plt1)
      call bcast_ctl_data_4_platform(org_plt)
!
      call bcast_sph_mhd_model
      call bcast_fem_mhd_control
!
      call bcast_monitor_data_ctl(nmtr_ctl1)
      call bcast_sph_monitoring_ctl(smonitor_ctl1)
      call bcast_files_4_psf_ctl
      call bcast_files_4_iso_ctl
!
      end subroutine bcast_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_control
!
      use read_ctl_data_sph_MHD
      use bcast_4_solver_ctl
      use bcast_4_fem_int_pts_ctl
!
!
      call bcast_sph_mhd_control
      call bcast_CG_solver_param_ctl(CG_ctl1)
      call bcast_control_fem_int_points(fint_ctl1)
!
      end subroutine bcast_fem_mhd_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_fem_MHD
