!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!!      subroutine FEM_initialize_back_trans                            &
!!     &         (ucd_param, viz_step, ele_4_nod, jacobians, ucd, m_ucd)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(element_around_node), intent(inout) :: ele_4_nod
!!        type(jacobians_type), intent(inout) :: jacobians
!!        type(ucd_data), intent(inout) :: ucd
!!        type(merged_ucd_data), intent(inout)  :: m_ucd
!!      subroutine FEM_analyze_back_trans                               &
!!     &         (ucd_param, t_IO, ucd, i_step, viz_step, visval)
!!        type(VIZ_step_params), intent(inout) :: viz_step
!
      module FEM_analyzer_back_trans
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_SPH_transforms
      use t_VIZ_step_parameter
      use t_file_IO_parameter
      use t_shape_functions
!
      implicit none
!
      type(shape_finctions_at_points), save, private :: spfs_TRNS
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_back_trans                              &
     &         (ucd_param, viz_step, ele_4_nod, jacobians, ucd, m_ucd)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_ctl_params_sph_trans
      use t_jacobians
!
      use m_array_for_send_recv
!
      use nod_phys_send_recv
      use set_table_4_RHS_assemble
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use output_parallel_ucd_file
      use const_jacobians_3d
      use const_mesh_information
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(field_IO_params), intent(in) :: ucd_param
      type(element_around_node), intent(inout) :: ele_4_nod
      type(jacobians_type), intent(inout) :: jacobians
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout)  :: m_ucd
!
      integer(kind = kint) :: iflag
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment
      if(iflag .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
        call set_element_on_node_in_mesh(femmesh_STR%mesh, ele_4_nod)
!
        if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
        allocate(jacobians%g_FEM)
        call sel_max_int_point_by_etype                                 &
     &     (femmesh_STR%mesh%ele%nnod_4_ele, jacobians%g_FEM)
        call const_jacobian_volume_normals(my_rank, nprocs,             &
     &      femmesh_STR%mesh, elemesh_STR%surf, femmesh_STR%group,      &
     &      spfs_TRNS, jacobians)
      end if
!
!  -------------------------------
!  -------------------------------
!
      call dealloc_edge_geometory(elemesh_STR%edge)
!
!  connect grid data to volume output
!
      if(t_STR%ucd_step%increment .eq. 0) return
      call link_output_grd_file                                         &
     &   (femmesh_STR%mesh%node, femmesh_STR%mesh%ele,                  &
     &    femmesh_STR%mesh%nod_comm, field_STR, ucd_param, ucd, m_ucd)
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans                                 &
     &         (ucd_param, t_IO, ucd, i_step, viz_step, visval)
!
      use t_ctl_params_sph_trans
      use t_time_data
      use t_ucd_data
      use t_IO_step_parameter
      use field_IO_select
      use ucd_IO_select
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: t_IO
      type(ucd_data), intent(in) :: ucd
!
      integer (kind =kint), intent(inout) :: visval
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
!*  ----------   Count steps for visualization
!*
      visval = viz_file_step_4_fix(i_step, viz_step)
!
      if(visval .eq. 0) then
        call nod_fields_send_recv(femmesh_STR%mesh, field_STR)
      end if
!
!*  -----------  Output volume data --------------
!*
      if(output_IO_flag(i_step,t_STR%ucd_step) .eq. 0) then
        call sel_write_udt_file(my_rank, i_step, ucd_param, t_IO, ucd)
      end if
!
      end subroutine FEM_analyze_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine SPH_to_FEM_bridge_back_trans(visval)
!
!
!      end subroutine SPH_to_FEM_bridge_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine FEM_to_SPH_bridge
!
!
!      end subroutine FEM_to_SPH_bridge
!
!-----------------------------------------------------------------------
!
!      subroutine FEM_finalize_back_trans
!
!
!      end subroutine FEM_finalize_back_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_back_trans
