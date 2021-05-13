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
!!     &         (viz_step, geofem, edge_comm, ele_4_nod, jacobians)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(element_around_node), intent(inout) :: ele_4_nod
!!        type(jacobians_type), intent(inout) :: jacobians
!!
!!      subroutine init_FEM_to_VIZ_bridge(viz_step, geofem, VIZ_DAT)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!      subroutine init_FEM_MHD_to_VIZ_bridge                           &
!!     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(mesh_data), intent(inout) :: geofem
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!@endverbatim
!
      module FEM_to_VIZ_bridge
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
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
     &         (viz_step, geofem, edge_comm, next_tbl, jacobians)
!
      use t_fem_gauss_int_coefs
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use set_normal_vectors
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(communication_table), intent(inout) :: edge_comm
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(jacobians_type), intent(inout) :: jacobians
!
      integer(kind = kint) :: iflag
      type(shape_finctions_at_points) :: spfs
!
!
!  -----  Const Neighboring information
      iflag = viz_step%FLINE_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (geofem%mesh, next_tbl%neib_ele, next_tbl%neib_nod)
      end if
!
! ------  init for fieldline and PVR ---------------------
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm,                     &
     &      edge_comm, geofem%mesh%edge)
      end if
!
      iflag = viz_step%PVR_t%increment + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'jacobian_and_element_volume'
!        call sel_max_int_point_by_etype                                &
!     &     (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call set_max_integration_points(ione, jacobians%g_FEM)
        call jacobian_and_element_volume(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
        if (iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      geofem%mesh, geofem%group, spfs, jacobians)
      end if
!
      end subroutine normals_and_jacobians_4_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_VIZ_bridge(viz_step, geofem, VIZ_DAT)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use field_to_new_partition
      use parallel_FEM_mesh_init
!
      use set_table_4_RHS_assemble
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use set_normal_vectors
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
!
      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!
      if(iflag_debug.gt.0) write(*,*) 'normals_and_jacobians_4_VIZ'
      call link_jacobians_4_viz                                         &
     &   (VIZ_DAT%next_tbl_v, VIZ_DAT%jacobians_v, VIZ_DAT)
      call normals_and_jacobians_4_VIZ(viz_step, geofem,                &
     &    VIZ_DAT%edge_comm, VIZ_DAT%next_tbl, VIZ_DAT%jacobians)
!
      if(VIZ_DAT%repart_p%flag_repartition) then
        if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
        call link_FEM_field_4_viz(VIZ_DAT%geofem_v, VIZ_DAT)
!
!  -----  Repartition
        call load_or_const_new_partition                                &
     &     (VIZ_DAT%repart_p, geofem, VIZ_DAT%next_tbl,                 &
     &      VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl)
      else
        call link_FEM_field_4_viz(geofem, VIZ_DAT)
      end if
!
      end subroutine init_FEM_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_MHD_to_VIZ_bridge                             &
     &         (viz_step, next_tbl, jacobians, geofem, VIZ_DAT)
!
      use m_work_time
      use m_elapsed_labels_4_REPART
      use field_to_new_partition
      use const_element_comm_tables
      use parallel_FEM_mesh_init
!
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use set_normal_vectors
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(next_nod_ele_table), intent(in), target :: next_tbl
      type(jacobians_type), intent(in), target :: jacobians
!
      type(mesh_data), intent(inout) :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      integer(kind = kint) :: iflag
!
!
      if(VIZ_DAT%repart_p%flag_repartition) then
        call link_FEM_field_4_viz(VIZ_DAT%geofem_v, VIZ_DAT)
        call load_or_const_new_partition                                &
     &     (VIZ_DAT%repart_p, geofem, next_tbl,                         &
     &      VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl)
      else
        call link_FEM_field_4_viz(geofem, VIZ_DAT)
        call link_jacobians_4_viz(next_tbl, jacobians, VIZ_DAT)
!
        iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
        if(iflag .gt. 0) then
          if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
          call const_edge_comm_table                                    &
     &       (geofem%mesh%node, geofem%mesh%nod_comm,                   &
     &        VIZ_DAT%edge_comm, geofem%mesh%edge)
        end if
      end if
      call calypso_mpi_barrier
!
      end subroutine init_FEM_MHD_to_VIZ_bridge
!
! ----------------------------------------------------------------------
!
      end module FEM_to_VIZ_bridge
