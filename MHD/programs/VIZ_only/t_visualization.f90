!>@file   t_visualization.f90
!!@brief  module t_visualization
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine element_normals_4_VIZ                                &
!!     &         (geofem, ele_4_nod, spfs, jacobians)
!!        type(time_data_control), intent(in) :: tctl
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: ucd_param
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module t_visualization
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
!
      implicit none
!
!
!>      Structure of mesh and field for visualization only
      type FEM_mesh_field_4_viz
!>        Structure for mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure for field file IO paramters
        type(field_IO_params) :: ucd_file_IO
!
!>       Structure for mesh data
!>        (position, connectivity, group, and communication)
        type(mesh_data) :: geofem
!>         Structure for nodal field data
        type(phys_data) :: nod_fld
!
!>          Instance of time data from data input
        type(time_data) :: ucd_time
!>          Instance of FEM field data IO
        type(ucd_data) :: ucd
!
!>        Structure of included element list for each node
        type(element_around_node) :: ele_4_nod
!>        Structure of shape function for PVR and fieldline
        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians
      end type FEM_mesh_field_4_viz
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine element_normals_4_VIZ                                  &
     &         (geofem, ele_4_nod, spfs, jacobians)
!
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
      type(mesh_data), intent(inout) :: geofem
      type(element_around_node), intent(inout) :: ele_4_nod
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacobians
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
      call set_element_on_node_in_mesh(geofem%mesh, ele_4_nod)
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    geofem%mesh, geofem%group, spfs, jacobians)
!
      end subroutine element_normals_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module t_visualization
