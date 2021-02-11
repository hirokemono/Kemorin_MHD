!>@file   FEM_analyzer_sph_SGS_MHD.f90
!!@brief  module FEM_analyzer_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2017
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_SGS_MHD                           &
!!     &        (MHD_files, MHD_step, geofem, nod_fld, iphys, iphys_LES,&
!!     &         ele_4_nod_VIZ, jacobians, MHD_IO, v_sol)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(inout) :: iphys_LES
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(element_around_node), intent(inout) :: ele_4_nod_VIZ
!!        type(jacobians_type), intent(inout) :: jacobians
!!        type(MHD_IO_data), intent(inout) :: MHD_IO
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!      subroutine SPH_to_FEM_bridge_SGS_MHD                            &
!!     &        (SGS_par, sph, WK, WK_LES, geofem, nod_fld)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(in) :: WK_LES
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_SGS_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_MHD_step_parameter
      use t_file_IO_parameter
!
      use t_shape_functions
      use t_vector_for_solver
!
      implicit none
!
      type(shape_finctions_at_points), save, private :: spfs_M
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_SGS_MHD                             &
     &        (MHD_files, MHD_step, geofem, nod_fld, iphys, iphys_LES,  &
     &         ele_4_nod_VIZ, jacobians, MHD_IO, v_sol)
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_next_node_ele_4_node
      use t_jacobians
      use t_VIZ_step_parameter
      use t_MHD_file_parameter
      use t_cal_max_indices
      use t_MHD_IO_data
      use t_ucd_file
!
      use set_table_4_RHS_assemble
      use FEM_analyzer_sph_MHD
      use int_volume_of_domain
      use set_normal_vectors
      use parallel_FEM_mesh_init
      use set_field_data_w_SGS
      use node_monitor_IO
      use nod_phys_send_recv
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(mesh_data), intent(inout) :: geofem
      type(phys_address), intent(inout) :: iphys
      type(SGS_model_addresses), intent(inout) :: iphys_LES
      type(phys_data), intent(inout) :: nod_fld
      type(element_around_node), intent(inout) :: ele_4_nod_VIZ
      type(jacobians_type), intent(inout) :: jacobians
      type(MHD_IO_data), intent(inout) :: MHD_IO
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: iflag
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
      call set_local_nod_4_monitor(geofem%mesh, geofem%group)
!
      if (iflag_debug.gt.0) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS                                        &
     &   (geofem%mesh%node%numnod, nod_fld, iphys, iphys_LES)
!
!  -------------------------------
!  connect grid data to volume output
!  -------------------------------
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call alloc_phys_range(nod_fld%ntot_phys_viz, MHD_IO%range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(MHD_files%ucd_file_IO,            &
     &    MHD_step%ucd_step, geofem%mesh, nod_fld, MHD_IO%ucd)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(geofem%mesh, v_sol)
      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!
      call deallocate_surface_geom_type(geofem%mesh%surf)
      call dealloc_edge_geometory(geofem%mesh%edge)
!
!  -------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------
!
      if(MHD_step%viz_step%FLINE_t%increment .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
        call set_element_on_node_in_mesh                                &
     &     (geofem%mesh, ele_4_nod_VIZ)
      end if
!
!  -----  If there is no volume rendering... return
!
      if (iflag_debug.eq.1) write(*,*)  'set_max_integration_points'
      iflag = MHD_step%viz_step%PVR_t%increment                         &
     &       + MHD_step%viz_step%LIC_t%increment
      if(iflag .le. 0) Return
!
      allocate(jacobians%g_FEM)
      call set_max_integration_points(ione, jacobians%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    geofem%mesh, geofem%group, spfs_M, jacobians)
!
      end subroutine FEM_initialize_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_SGS_MHD                              &
     &        (SGS_par, sph, WK, WK_LES, geofem, nod_fld)
!
      use t_spheric_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_SGS_control_parameter
!
      use set_address_sph_trans_snap
      use SGS_MHD_fields_to_FEM
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(in) :: WK_LES
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(inout) :: nod_fld
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_MHD%forward, geofem%mesh, nod_fld)
!
      if(SGS_par%model_p%iflag_SGS .gt. 0) then
        call copy_SGS_MHD_fld_from_trans                                &
     &     (sph, WK_LES, geofem%mesh, nod_fld)
      end if
!
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform base fields'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform diff_vector'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_difv%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_field_from_transform SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_force_from_transform SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK%trns_eflux%forward, geofem%mesh, nod_fld)
!
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                    'copy_force_from_transform filter_MHD'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_MHD%forward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform base fields'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                'copy_field_from_transform filtered_diff_vector'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_fil_difv%backward, geofem%mesh, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                    'copy_field_from_transform SGS_SNAP'
      call copy_field_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_SGS_snap%backward, geofem%mesh, nod_fld)
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                     'copy_force_from_transform SGS_SNAP'
      call copy_force_from_transform(sph%sph_params, sph%sph_rtp,       &
     &    WK_LES%trns_SGS_snap%forward, geofem%mesh, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_SGS_MHD
