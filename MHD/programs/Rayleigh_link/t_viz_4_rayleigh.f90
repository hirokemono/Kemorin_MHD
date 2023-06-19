!>@file   t_viz_4_rayleigh.f90
!!@brief  module t_viz_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine input_conrol_rayleigh_viz(ctl_file_name,             &
!!     &          rayleigh_vctl, FEM_Rayleigh, t_viz_param)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        type(control_data_rayleigh_vizs), intent(inout)               &
!!     &                                 :: rayleigh_vctl
!!        type(FEM_mesh_field_rayleigh_viz), intent(inout)              &
!!     &                                 :: FEM_Rayleigh
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module t_viz_4_rayleigh
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_VIZ_only_step_parameter
!
      use t_rayleigh_field_IO
      use t_rayleigh_field_address
      use t_ctl_data_rayleigh_vizs
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_mesh_field_rayleigh_viz
!>        Structure for FEM mesh data
!!         (position, connectivity, communication, and groups)
        type(mesh_data) :: geofem
!>       Structure for nodal field data
        type(phys_data) :: field
!
        type(rayleigh_field) :: rayleigh_rtp
        type(rayleigh_field_address) :: iphys_ftb
      end type FEM_mesh_field_rayleigh_viz
!
      private :: bcast_rayleigh_vizs_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_conrol_rayleigh_viz(ctl_file_name,               &
     &          rayleigh_vctl, FEM_Rayleigh, t_viz_param)
!
      use t_read_control_elements
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                                 :: rayleigh_vctl
        type(FEM_mesh_field_rayleigh_viz), intent(inout)                &
     &                                 :: FEM_Rayleigh
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      integer(kind = kint) :: ierr = 0
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_ctl_file_rayleigh_viz(ctl_file_name,                  &
     &                                  rayleigh_vctl, c_buf1)
      end if
      call bcast_rayleigh_vizs_ctl_data(rayleigh_vctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(rayleigh_vctl%i_viz_only_file,           &
     &                             'control file is broken')
      end if
!
      call set_ctl_params_rayleigh_viz(rayleigh_vctl,                   &
     &    t_viz_param, FEM_Rayleigh, ierr)
!
      end subroutine input_conrol_rayleigh_viz
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_rayleigh_vizs_ctl_data(rayleigh_vctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_control_data_vizs
      use bcast_4_field_ctl
      use bcast_4_sphere_ctl
!
      type(control_data_rayleigh_vizs), intent(inout) :: rayleigh_vctl
!
!
      call bcast_ctl_data_4_platform(rayleigh_vctl%viz_plt)
      call bcast_ctl_data_4_time_step(rayleigh_vctl%t_viz_ctl)
      call bcast_viz_controls(rayleigh_vctl%viz_ctl_v)
!
      call bcast_phys_data_ctl(rayleigh_vctl%fld_ctl)
      call bcast_ctl_ndomain_4_shell(rayleigh_vctl%sdctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (rayleigh_vctl%i_viz_only_file, 0)
!
      end subroutine bcast_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_viz(rayleigh_vctl,             &
     &          t_viz_param, FEM_Rayleigh, ierr)
!
      use m_error_IDs
      use t_file_IO_parameter
!
      use m_file_format_switch
      use set_control_platform_item
!
      type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vctl
!
      type(FEM_mesh_field_rayleigh_viz), intent(inout) :: FEM_Rayleigh
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_control_smp_def(my_rank, rayleigh_vctl%viz_plt)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (rayleigh_vctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
      if(ierr .gt. 0) return
!
      call set_ctl_rayleigh_field_address                               &
     &   (rayleigh_vctl%viz_plt, rayleigh_vctl%fld_ctl,                 &
     &    FEM_Rayleigh%iphys_ftb, e_message, ierr)
!
      call set_ctl_params_rayleigh_domains                              &
     &   (rayleigh_vctl%sdctl, FEM_Rayleigh%rayleigh_rtp,               &
     &    e_message, ierr)
!
      end subroutine set_ctl_params_rayleigh_viz
!
! ----------------------------------------------------------------------
!
      end module t_viz_4_rayleigh
