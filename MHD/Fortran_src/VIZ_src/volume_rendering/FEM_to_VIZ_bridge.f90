!>@file   FEM_to_VIZ_bridge.f90
!!@brief  module FEM_to_VIZ_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine normals_and_jacobians_4_VIZ                          &
!!     &         (viz_step, geofem, ele_4_nod, jacobians)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(element_around_node), intent(inout) :: ele_4_nod
!!        type(jacobians_type), intent(inout) :: jacobians
!!
!!      subroutine init_FEM_to_VIZ_bridge                               &
!!     &         (viz_step, geofem, nod_fld, VIZ_DAT)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!      subroutine init_FEM_MHD_to_VIZ_bridge                           &
!!     &         (viz_step, next_tbl, jacobians,                        &
!!     &          geofem, nod_fld, VIZ_DAT)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(mesh_data), intent(inout) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!
!!      subroutine s_FEM_to_VIZ_bridge(nod_fld, v_sol, VIZ_DAT)
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!@endverbatim
!
      module FEM_to_VIZ_bridge
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
      use t_VIZ_mesh_field
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
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
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_VIZ_bridge                                 &
     &         (viz_step, geofem, nod_fld, VIZ_DAT)
!
      use field_to_new_partition
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(phys_data), intent(inout) :: nod_fld
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
!
      if(VIZ_DAT%repart_p%flag_repartition) then
        call alloc_FEM_field_4_viz(VIZ_DAT)
        call load_or_const_new_partition(VIZ_DAT%repart_p,              &
     &      geofem, VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl)
!
        call init_fld_to_new_partition(VIZ_DAT%viz_fem%mesh,            &
     &                                 nod_fld, VIZ_DAT%viz_fld)
      else
        call link_FEM_field_4_viz(geofem, nod_fld, VIZ_DAT)
      end if
!
      call alloc_jacobians_4_viz(VIZ_DAT)
      call normals_and_jacobians_4_VIZ(viz_step, VIZ_DAT%viz_fem,       &
     &    VIZ_DAT%ele_4_nod, VIZ_DAT%jacobians)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (VIZ_DAT%viz_fem%mesh%node, VIZ_DAT%viz_fem%mesh%nod_comm,     &
     &    VIZ_DAT%edge_comm, VIZ_DAT%viz_fem%mesh%edge)
!
      end subroutine init_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_to_VIZ_bridge                             &
     &         (viz_step, next_tbl, jacobians,                          &
     &          geofem, nod_fld, VIZ_DAT)
!
      use field_to_new_partition
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
!
      type(mesh_data), intent(inout) :: geofem
      type(phys_data), intent(inout) :: nod_fld
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
!
      if(VIZ_DAT%repart_p%flag_repartition) then
        call alloc_FEM_field_4_viz(VIZ_DAT)
        call load_const_new_part_FEM_MHD(VIZ_DAT%repart_p, next_tbl,    &
     &      geofem, VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl)
!
        allocate(VIZ_DAT%viz_fld)
        call init_fld_to_new_partition(VIZ_DAT%viz_fem%mesh,            &
     &                                 nod_fld, VIZ_DAT%viz_fld)
!
        call alloc_jacobians_4_viz(VIZ_DAT)
        call normals_and_jacobians_4_VIZ(viz_step,                      &
     &      VIZ_DAT%viz_fem, VIZ_DAT%ele_4_nod, VIZ_DAT%jacobians)
      else
        call link_FEM_field_4_viz(geofem, nod_fld, VIZ_DAT)
        call link_jacobians_4_viz                                       &
     &     (next_tbl%neib_ele, jacobians, VIZ_DAT)
      end if
      call calypso_mpi_barrier
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (VIZ_DAT%viz_fem%mesh%node, VIZ_DAT%viz_fem%mesh%nod_comm,     &
     &    VIZ_DAT%edge_comm, VIZ_DAT%viz_fem%mesh%edge)
!
      end subroutine init_FEM_MHD_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine s_FEM_to_VIZ_bridge(nod_fld, v_sol, VIZ_DAT)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use field_to_new_partition
      use select_copy_from_recv
!
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
!
      if(VIZ_DAT%repart_p%flag_repartition) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+4)
        call nod_field_to_new_partition(iflag_import_item,              &
     &      VIZ_DAT%viz_fem%mesh, VIZ_DAT%mesh_to_viz_tbl,              &
     &      nod_fld, VIZ_DAT%viz_fld, v_sol)
        if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+4)
      end if
!
      end subroutine s_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      end module FEM_to_VIZ_bridge
