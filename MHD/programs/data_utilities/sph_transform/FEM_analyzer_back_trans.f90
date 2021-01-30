!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!!      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR)
!!      subroutine FEM_analyze_back_trans                               &
!!     &         (i_step, ucd_step, visval, FEM_STR)
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
      module FEM_analyzer_back_trans
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_FEM_data_4_SPH_trans
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
      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_ctl_params_sph_trans
      use t_jacobians
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
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
      integer(kind = kint) :: iflag
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS(FEM_STR)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = FEM_STR%viz_step%FLINE_t%increment                        &
     &       + FEM_STR%viz_step%PVR_t%increment                         &
     &       + FEM_STR%viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
        call set_element_on_node_in_mesh                                &
     &     (FEM_STR%geofem%mesh, FEM_STR%ele_4_nod)
!
        if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
        allocate(FEM_STR%jacobians%g_FEM)
        call sel_max_int_point_by_etype                                 &
     &     (FEM_STR%geofem%mesh%ele%nnod_4_ele,                         &
     &      FEM_STR%jacobians%g_FEM)
        call const_jacobian_volume_normals(my_rank, nprocs,             &
     &      FEM_STR%geofem%mesh, FEM_STR%geofem%group,                  &
     &      spfs_TRNS, FEM_STR%jacobians)
      end if
!
!  -------------------------------
!  -------------------------------
!
      call dealloc_edge_geometory(FEM_STR%geofem%mesh%edge)
!
!  connect grid data to volume output
!
      if(ucd_step%increment .eq. 0) return
      call link_output_grd_file                                         &
     &   (FEM_STR%geofem%mesh%node, FEM_STR%geofem%mesh%ele,            &
     &    FEM_STR%geofem%mesh%nod_comm, FEM_STR%field,                  &
     &    FEM_STR%ucd_file_IO, FEM_STR%ucd)
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans                                 &
     &         (i_step, ucd_step, visval, FEM_STR)
!
      use t_ctl_params_sph_trans
      use t_time_data
      use t_ucd_data
      use t_IO_step_parameter
      use field_IO_select
      use parallel_ucd_IO_select
      use nod_phys_send_recv
!
      logical, intent(in) :: visval
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
!
!*  ----------   Count steps for visualization
!*
      if(visval) call nod_fields_send_recv(FEM_STR%geofem%mesh,         &
     &                                 FEM_STR%field, FEM_STR%v_sol)
!
!*  -----------  Output volume data --------------
!*
      if(output_IO_flag(i_step,ucd_step)) then
        call sel_write_parallel_ucd_file                                &
     &     (i_step, FEM_STR%ucd_file_IO, FEM_STR%time_IO, FEM_STR%ucd)
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
