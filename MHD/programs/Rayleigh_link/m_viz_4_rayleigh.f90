!>@file   m_viz_4_rayleigh.f90
!!@brief  module m_viz_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_ctl_params_rayleigh_viz                          &
!!     &         (tctl, plt, sdctl, field_ctl,                          &
!!     &          mesh_file, ucd_param, time_v, viz_step, ierr)
!!      subroutine mesh_setup_4_VIZ(ucd_param)
!!      subroutine element_normals_4_VIZ
!!      subroutine set_field_data_4_VIZ(iflag, istep_ucd, time_d)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: time_d
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
      use t_VIZ_step_parameter
!
      implicit none
!
!       Structure for time stepping parameters
      type(time_step_param), save :: t_VIZ
!
!>      Increment for visualizations
      type(VIZ_step_params), save :: viz_step_V
!
!>      Structure for mesh file IO paramters
      type(field_IO_params), save :: mesh_file_VIZ
!>      Structure for field file IO paramters
      type(field_IO_params), save :: ucd_file_VIZ
!
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
!>        Instance for numbers of FEM mesh for merged IO
!      type(merged_ucd_data), save :: m_ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_VIZ
!
      type(shape_finctions_at_points), save :: spfs_VIZ
!>      Stracture for Jacobians
      type(jacobians_type), save :: jacobians_VIZ
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_viz                            &
     &         (tctl, plt, sdctl, field_ctl,                            &
     &          time_v, viz_step, rayleigh_ftbl, ra_fld, ierr)
!
      use m_error_IDs
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_ctl_data_4_fields
      use t_ctl_data_4_divide_sphere
      use t_VIZ_step_parameter
      use t_rayleigh_field_address
      use t_rayleigh_field_IO
!
      use m_file_format_switch
      use set_control_platform_data
!
      type(time_data_control), intent(in) :: tctl
      type(platform_data_control), intent(in) :: plt
      type(sphere_domain_control), intent(in) :: sdctl
      type(field_control), intent(in) :: field_ctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(time_step_param), intent(inout) :: time_v
      type(VIZ_step_params), intent(inout) :: viz_step
!
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
      type(rayleigh_field), intent(inout) :: ra_fld
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
!
      call set_fixed_time_step_params                                   &
     &   (tctl, time_v, ierr, e_message)
      if(ierr .gt. 0) return
!
      call viz_fixed_time_step_params                                   &
     &   (time_v%init_d%dt, tctl, viz_step)
      call copy_delta_t(time_v%init_d, time_v%time_d)
!
      call set_ctl_rayleigh_field_address                               &
     &   (plt, field_ctl, rayleigh_ftbl, e_message, ierr)
!
      call set_ctl_params_rayleigh_domains                              &
     &   (sdctl, ra_fld, e_message, ierr)
!
      end subroutine set_ctl_params_rayleigh_viz
!
! ----------------------------------------------------------------------
!
      subroutine element_normals_4_VIZ(fem, ele_4_nod, spfs, jacobians)
!
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
      type(mesh_data), intent(inout) :: fem
      type(element_around_node), intent(inout) :: ele_4_nod
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacobians
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
      call set_element_on_node_in_mesh(fem%mesh, ele_4_nod)
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    fem%mesh, fem%group, spfs, jacobians)
!
      end subroutine element_normals_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module m_viz_4_rayleigh
