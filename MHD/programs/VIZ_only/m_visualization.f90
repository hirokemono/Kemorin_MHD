!>@file   m_visualization.f90
!!@brief  module m_visualization
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_control_params_4_viz                             &
!!     &         (my_rank, tctl, plt, mesh_file, ucd_param, ierr)
!!      subroutine mesh_setup_4_VIZ(ucd_param)
!!      subroutine element_normals_4_VIZ
!!      subroutine set_field_data_4_VIZ(iflag, istep_ucd, time_d)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: time_d
!!@endverbatim
!
      module m_visualization
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
!
!>     Structure for element, surface, and edge mesh
!!        (position, connectivity, and communication)
      type(element_geometry), save :: elemesh_VIZ
!
!
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
      subroutine set_control_params_4_viz                               &
     &         (my_rank, tctl, plt, mesh_file, ucd_param, ierr)
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
      integer(kind = kint), intent(in) :: my_rank
      type(time_data_control), intent(in) :: tctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(platform_data_control), intent(inout) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_ucd_file_define(plt, ucd_param)
!
      call set_fixed_time_step_params(tctl, t_VIZ, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (t_VIZ%init_d%dt, tctl, viz_step_V)
      call copy_delta_t(t_VIZ%init_d, t_VIZ%time_d)
!
      if(ierr .gt. 0) return
!
      end subroutine set_control_params_4_viz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ(ucd_param)
!
      use calypso_mpi
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(field_IO_params), intent(in) :: ucd_param
!
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
      call mpi_input_mesh(mesh_file_VIZ, nprocs,                        &
     &    femmesh_VIZ%mesh, femmesh_VIZ%group,                          &
     &    elemesh_VIZ%surf%nnod_4_surf, elemesh_VIZ%edge%nnod_4_edge)
!
       if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
       call FEM_mesh_initialization                                     &
     &    (femmesh_VIZ%mesh, femmesh_VIZ%group, elemesh_VIZ)
!
!     ---------------------
!
      ucd_VIZ%nnod =      femmesh_VIZ%mesh%node%numnod
      call sel_read_udt_param(my_rank, t_VIZ%init_d%i_time_step,        &
     &    ucd_param, VIZ_time_IO, ucd_VIZ)
      call alloc_phys_data_type_by_output                               &
     &   (ucd_VIZ, femmesh_VIZ%mesh%node, field_VIZ)
!
      end subroutine mesh_setup_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine element_normals_4_VIZ
!
      use int_volume_of_domain
      use set_ele_id_4_node_type
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
      call set_ele_id_4_node                                            &
     &   (femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele, ele_4_nod_VIZ)
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians_VIZ%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (femmesh_VIZ%mesh%ele%nnod_4_ele, jacobians_VIZ%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    femmesh_VIZ%mesh, elemesh_VIZ%surf, femmesh_VIZ%group,        &
     &    spfs_VIZ, jacobians_VIZ)
!
      end subroutine element_normals_4_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ                                   &
     &         (iflag, istep_ucd, ucd_param, time_d)
!
      use set_ucd_data_to_type
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iflag, istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: time_d
!
!
      if(iflag .ne. 0) return
      call set_data_by_read_ucd(my_rank, istep_ucd,                     &
     &    ucd_param, VIZ_time_IO, ucd_VIZ, field_VIZ)
      call copy_time_step_size_data(VIZ_time_IO, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(femmesh_VIZ%mesh, field_VIZ)
!
      end subroutine set_field_data_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module m_visualization
