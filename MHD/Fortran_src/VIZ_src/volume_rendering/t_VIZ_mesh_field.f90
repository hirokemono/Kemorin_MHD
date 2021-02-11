!>@file   t_VIZ_mesh_field.f90
!!@brief  module t_VIZ_mesh_field
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief DAta structuresa for visualizers
!!
!!@verbatim
!!      subroutine link_FEM_field_4_viz(geofem, nod_fld, viz)
!!      subroutine link_jacobians_4_viz(ele_4_nod, jacobians, viz)
!!      subroutine unlink_FEM_field_4_viz(viz)
!!      subroutine unlink_jacobians_4_viz(viz)
!!        type(mesh_data), intent(inout), target :: geofem
!!        type(phys_data), intent(inout), target :: nod_fld
!!        type(element_around_node), intent(in), target :: ele_4_nod
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(VIZ_mesh_field), intent(inout) :: viz
!!
!!      subroutine sel_repartition_for_viz(geofem, nod_fld, viz)
!!        type(mesh_data), intent(inout) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(VIZ_mesh_field), intent(inout) :: viz
!!@endverbatim
!
      module t_VIZ_mesh_field
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_VIZ_step_parameter
      use t_control_param_vol_grping
      use t_calypso_comm_table
!
      implicit none
!
!
!>      Structure of data for visualization
      type VIZ_mesh_field
!>         Structure for mesh data for visualization
        type(mesh_data), pointer :: viz_fem
!>         Structure for nodal field data
        type(phys_data), pointer :: viz_fld
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: repart_p
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
!
!!>        Structure of shape function for PVR and fieldline
!        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type), pointer :: jacobians
!>        Structure of included element list for each node
        type(element_around_node), pointer :: ele_4_nod
      end type VIZ_mesh_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine link_FEM_field_4_viz(geofem, nod_fld, viz)
!
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in), target :: nod_fld
      type(VIZ_mesh_field), intent(inout) :: viz
!
      viz%viz_fem => geofem
      viz%viz_fld => nod_fld
!
      end subroutine link_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine link_jacobians_4_viz(ele_4_nod, jacobians, viz)
!
      type(element_around_node), intent(in), target :: ele_4_nod
      type(jacobians_type), intent(in), target :: jacobians
      type(VIZ_mesh_field), intent(inout) :: viz
!
      viz%ele_4_nod => ele_4_nod
      viz%jacobians => jacobians
!
      end subroutine link_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_FEM_field_4_viz(viz)
!
      type(VIZ_mesh_field), intent(inout) :: viz
!
      nullify(viz%viz_fem, viz%viz_fld)
!
      end subroutine unlink_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_jacobians_4_viz(viz)
!
      type(VIZ_mesh_field), intent(inout) :: viz
!
      nullify(viz%jacobians, viz%ele_4_nod)
!
      end subroutine unlink_jacobians_4_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_repartition_for_viz(geofem, nod_fld, viz)
!
      use field_to_new_partition
!
      type(mesh_data), intent(inout) :: geofem
      type(phys_data), intent(inout) :: nod_fld
      type(VIZ_mesh_field), intent(inout) :: viz
!
!
      if(viz%repart_p%flag_repartition) then
        allocate(viz%viz_fem)
        call const_new_partition_mesh(viz%repart_p,                     &
     &      geofem, viz%viz_fem, viz%mesh_to_viz_tbl)
!
        allocate(viz%viz_fld)
        call init_fld_to_new_partition(viz%viz_fem%mesh,                &
     &                                 nod_fld, viz%viz_fld)
      else
        call link_FEM_field_4_viz(geofem, nod_fld, viz)
      end if
!
      end subroutine sel_repartition_for_viz
!
! ----------------------------------------------------------------------
!
      subroutine normals_and_jacobians_4_VIZ                            &
     &         (viz_step, geofem, ele_4_nod, jacobians)
!
      use t_fem_gauss_int_coefs
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use parallel_FEM_mesh_init
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(element_around_node), intent(inout) :: ele_4_nod
      type(jacobians_type), intent(inout) :: jacobians
!
      integer(kind = kint) :: iflag
      type(shape_finctions_at_points) :: spfs
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!
      call deallocate_surface_geom_type(geofem%mesh%surf)
      call dealloc_edge_geometory(geofem%mesh%edge)
!
!     --------------------- init for fieldline and PVR
!
      if(viz_step%FLINE_t%increment .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
        call set_element_on_node_in_mesh(geofem%mesh, ele_4_nod)
      end if
!
      iflag = viz_step%PVR_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
        allocate(jacobians%g_FEM)
!        call sel_max_int_point_by_etype                                &
!     &     (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call set_max_integration_points(ione, jacobians%g_FEM)
        call const_jacobian_volume_normals(my_rank, nprocs,             &
     &      geofem%mesh, geofem%group, spfs, jacobians)
      end if
!
      end subroutine normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
!
      end module t_VIZ_mesh_field
