!>@file   m_viz_4_rayleigh.f90
!!@brief  module m_viz_4_rayleigh
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_control_params_4_viz                             &
!!     &         (tctl, plt, mesh_file, ucd_param, ierr)
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
      type(time_data), save :: VIZ_time_IO
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
      subroutine set_control_params_4_viz(tctl, plt,                    &
     &          mesh_file, ucd_param, time_v, viz_step, ierr)
!
      use t_ucd_data
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_VIZ_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_data
      use ucd_IO_select
!
      type(time_data_control), intent(in) :: tctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(platform_data_control), intent(inout) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
      type(time_step_param), intent(inout) :: time_v
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_ucd_file_define(plt, ucd_param)
!
      call set_fixed_time_step_params(tctl, time_v, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_v%init_d%dt, tctl, viz_step)
      call copy_delta_t(time_v%init_d, time_v%time_d)
!
      if(ierr .gt. 0) return
!
      end subroutine set_control_params_4_viz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ(mesh_file, ucd_param, time_v,         &
     &          fem, t_IO, ucd, field)
!
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(field_IO_params), intent(in) :: mesh_file
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(in) :: time_v
      type(mesh_data), intent(inout) :: fem
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: field
!
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
      call mpi_input_mesh(mesh_file, nprocs, fem)
!
       if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
       call FEM_mesh_initialization(fem%mesh, fem%group)
!
!     ---------------------
!
      ucd%nnod = fem%mesh%node%numnod
      call sel_read_udt_param(my_rank, time_v%init_d%i_time_step,       &
     &    ucd_param, t_IO, ucd)
      call alloc_phys_data_type_by_output                               &
     &   (ucd, fem%mesh%node, field)
!
      end subroutine mesh_setup_4_VIZ
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
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ(iflag, istep_ucd, ucd_param,      &
     &         fem, t_IO, ucd, time_d, field)
!
      use set_ucd_data_to_type
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iflag, istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(mesh_data), intent(in) :: fem
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(time_data), intent(inout) :: time_d
      type(phys_data), intent(inout) :: field
!
!
      if(iflag .ne. 0) return
      call set_data_by_read_ucd(my_rank, istep_ucd,                     &
     &    ucd_param, t_IO, ucd, field)
      call copy_time_step_size_data(t_IO, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, field)
!
      end subroutine set_field_data_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module m_viz_4_rayleigh
