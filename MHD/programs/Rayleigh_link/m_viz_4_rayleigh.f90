!>@file   m_viz_4_rayleigh.f90
!!@brief  module m_viz_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_ctl_params_rayleigh_viz(rayleigh_vctl,           &
!!     &          t_viz_param, rayleigh_ftbl, rayleigh_rtp, VIZ_DAT,    &
!!     &          ierr)
!!        type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vctl
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!        type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!@endverbatim
!
      module m_viz_4_rayleigh
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_file_IO_parameter
      use t_time_data
      use t_VIZ_only_step_parameter
      use t_control_param_vol_grping
      use t_VIZ_mesh_field
!
      implicit none
!
!>       Structure for time stepping parameters
!!        with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ
!
!>      Structure for mesh file IO paramters
      type(field_IO_params), save :: mesh_file_VIZ
!>      Structure for field file IO paramters
      type(field_IO_params), save :: ucd_file_VIZ
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type(mesh_data), save :: femmesh_VIZ
!>       Structure for nodal field data
      type(phys_data), save :: field_VIZ
!
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: ucd_VIZ
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_viz(rayleigh_vctl,             &
     &          t_viz_param, rayleigh_ftbl, rayleigh_rtp, VIZ_DAT,      &
     &          ierr)
!
      use m_error_IDs
      use t_file_IO_parameter
      use t_ctl_data_rayleigh_vizs
      use t_rayleigh_field_address
      use t_rayleigh_field_IO
!
      use m_file_format_switch
      use set_control_platform_item
!
      type(control_data_rayleigh_vizs), intent(in) :: rayleigh_vctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
!
      call turn_off_debug_flag_by_ctl                                   &
     &   (my_rank, rayleigh_vctl%viz_plt)
      call set_control_smp_def(my_rank, rayleigh_vctl%viz_plt)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (rayleigh_vctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
      if(ierr .gt. 0) return
!
      call set_ctl_rayleigh_field_address                               &
     &   (rayleigh_vctl%viz_plt, rayleigh_vctl%fld_ctl,                 &
     &    rayleigh_ftbl, e_message, ierr)
!
      call set_ctl_params_rayleigh_domains                              &
     &   (rayleigh_vctl%sdctl, rayleigh_rtp, e_message, ierr)
!
      call set_ctl_param_vol_repart(rayleigh_vctl%repart_ctl,           &
     &                              VIZ_DAT%repart_p)
!
      end subroutine set_ctl_params_rayleigh_viz
!
! ----------------------------------------------------------------------
!
      end module m_viz_4_rayleigh
