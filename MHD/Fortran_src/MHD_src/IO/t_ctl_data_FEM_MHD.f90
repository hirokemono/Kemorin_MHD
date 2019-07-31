!>@file   t_ctl_data_FEM_MHD.f90
!!@brief  module t_ctl_data_FEM_MHD
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
!!      subroutine read_control_4_fem_MHD                               &
!!     &         (file_name, FEM_MHD_ctl, viz_ctls)
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!@endverbatim
!
      module t_ctl_data_FEM_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_FEM_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_vizs
!
      implicit none
!
!
      integer(kind=kint), parameter :: ctl_file_code = 11
!
      type fem_mhd_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
!>        Control structure for MHD/model
        type(mhd_model_control) :: model_ctl
!>        Control structure for MHD/control
        type(fem_mhd_control_control) :: fmctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring plave list
        type(node_monitor_control) :: nmtr_ctl
!
        integer(kind = kint) :: i_mhd_ctl = 0
      end type fem_mhd_control
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
!
!   2nd level for MHD
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: hd_mhd_ctl
      private :: hd_viz_control
      private :: read_fem_mhd_control_data, bcast_fem_mhd_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_fem_MHD                                 &
     &         (file_name, FEM_MHD_ctl, viz_ctls)
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(ctl_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(ctl_file_code, c_buf1)
          call read_fem_mhd_control_data(ctl_file_code, hd_mhd_ctl,     &
     &        FEM_MHD_ctl, viz_ctls, c_buf1)
          if(FEM_MHD_ctl%i_mhd_ctl .gt. 0) exit
        end do
        close(ctl_file_code)
      end if
!
      call bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      end subroutine read_control_4_fem_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_fem_mhd_control_data                              &
     &         (id_control, hd_block, FEM_MHD_ctl, viz_ctls, c_buf)
!
      use calypso_mpi
      use t_ctl_data_FEM_MHD_control
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(FEM_MHD_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, FEM_MHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, FEM_MHD_ctl%org_plt, c_buf)
!
        call read_sph_sgs_mhd_model                                     &
     &     (id_control, hd_model, FEM_MHD_ctl%model_ctl, c_buf)
        call read_fem_mhd_control                                       &
     &     (id_control, hd_control, FEM_MHD_ctl%fmctl_ctl, c_buf)
!
        call read_monitor_data_ctl(id_control, hd_monitor_data,         &
     &      FEM_MHD_ctl%nmtr_ctl, c_buf)
        call read_viz_controls                                          &
     &     (id_control, hd_viz_control, viz_ctls, c_buf)
      end do
      FEM_MHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_fem_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      use t_ctl_data_FEM_MHD_control
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%plt)
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%org_plt)
!
      call bcast_sph_sgs_mhd_model(FEM_MHD_ctl%model_ctl)
      call bcast_fem_mhd_control(FEM_MHD_ctl%fmctl_ctl)
!
      call bcast_monitor_data_ctl(FEM_MHD_ctl%nmtr_ctl)
!
      call bcast_viz_controls(viz_ctls)
!
      call MPI_BCAST(FEM_MHD_ctl%i_mhd_ctl, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_fem_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_FEM_MHD
