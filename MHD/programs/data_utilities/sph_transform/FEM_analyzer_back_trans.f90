!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!      subroutine FEM_initialize_back_trans                             &
!     &         (ele_4_nod, jac_3d_l, jac_3d_q, ucd, m_ucd)
!      subroutine FEM_analyze_back_trans(ucd, i_step,                   &
!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      module FEM_analyzer_back_trans
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_SPH_transforms
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_back_trans                              &
     &         (ele_4_nod, jac_3d_l, jac_3d_q, ucd, m_ucd)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      use m_array_for_send_recv
      use m_t_step_parameter
!
      use nod_phys_send_recv
      use set_ele_id_4_node_type
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use output_parallel_ucd_file
      use const_jacobians_3d
      use const_mesh_information
      use const_element_comm_tables
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout)  :: m_ucd
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if( (i_step_output_fline+i_step_output_pvr) .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node                                          &
     &   (femmesh_STR%mesh%node, femmesh_STR%mesh%ele, ele_4_nod)
!
        if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
        call max_int_point_by_etype(femmesh_STR%mesh%ele%nnod_4_ele)
        call const_jacobian_volume_normals(femmesh_STR%mesh,            &
     &      elemesh_STR%surf, femmesh_STR%group, jac_STR_l, jac_STR_q)
      end if
!
!  -------------------------------
!  -------------------------------
!
      call deallocate_edge_geom_type(elemesh_STR%edge)
!
!  connect grid data to volume output
!
      if(i_step_output_ucd .eq. 0) return
      call link_output_grd_file                                         &
     &   (femmesh_STR%mesh%node, femmesh_STR%mesh%ele,                  &
     &    femmesh_STR%mesh%nod_comm, field_STR, ucd, m_ucd)
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans(ucd, i_step,                    &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use m_t_step_parameter
      use m_time_data_IO
      use field_IO_select
      use set_exit_flag_4_visualizer
      use ucd_IO_select
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      type(ucd_data), intent(in) :: ucd
!
      integer (kind =kint), intent(inout) :: visval
      integer(kind = kint), intent(inout) :: istep_psf, istep_iso
      integer(kind = kint), intent(inout) :: istep_pvr, istep_fline
!
!
!*  ----------   Count steps for visualization
!*
      call set_flag_to_visualization(i_step,                            &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      if(visval .eq. 0) then
        call nod_fields_send_recv                                       &
     &     (femmesh_STR%mesh%nod_comm, field_STR)
      end if
!
!*  -----------  Output volume data --------------
!*
      if(i_step_output_ucd .gt. 0) then
        if( mod(i_step,i_step_output_ucd) .eq. 0) then
          call sel_write_udt_file(my_rank, i_step, t1_IO, ucd)
        end if
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
