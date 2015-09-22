!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!      subroutine FEM_initialize_back_trans
!      subroutine FEM_analyze_back_trans(i_step,                        &
!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!      subroutine FEM_finalize_back_trans
!
      module FEM_analyzer_back_trans
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_back_trans
!
      use m_geometry_data
      use m_group_data
      use m_array_for_send_recv
      use m_node_phys_data
      use m_element_id_4_node
      use m_jacobians
      use m_jacobians_4_surface
      use m_t_step_parameter
      use m_ele_sf_eg_comm_tables
!
      use nodal_vector_send_recv
      use const_mesh_info
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use output_parallel_ucd_file
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if( (i_step_output_fline+i_step_output_pvr) .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node
!
        call set_max_int_point_by_etype
        if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
        call cal_jacobian_element
!
        call dealloc_dxi_dx_type(jac1_3d_q)
        call dealloc_dxi_dx_type(jac1_3d_l)
!
!     --------------------- Surface jacobian for fieldline
!
        if (iflag_debug.gt.0) write(*,*) 's_int_whole_volume_only'
        call s_int_whole_volume_only
!
        if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_surface'
        call cal_jacobian_surface
!
        if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector'
        call s_cal_normal_vector
!
        if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
        call pick_normal_of_surf_group(sf_grp1, sf_grp_v1)
!
        if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
        call s_sum_normal_4_surf_group(sf_grp1, sf_grp_v1)
      end if
!
!  -------------------------------
!  -------------------------------
!
      call deallocate_edge_geom_type(edge1)
!
      if (iflag_debug.gt.0) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(node1%numnod, nod_fld1)
!
!  connect grid data to volume output
!
      if(i_step_output_ucd .gt. 0) then
        call output_grd_file
      end if
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans(i_step,                         &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use m_t_step_parameter
      use m_ucd_data
      use field_IO_select
      use set_exit_flag_4_visualizer
      use ucd_IO_select
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
!
      integer (kind =kint), intent(inout) :: visval
      integer(kind = kint), intent(inout) :: istep_psf, istep_iso
      integer(kind = kint), intent(inout) :: istep_pvr, istep_fline
!
!
!*  ----------   Count steps for visualization
!*
      call set_flag_to_visualization(i_step,                          &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      if(visval .eq. 0) call phys_send_recv_all
!
!*  -----------  Output volume data --------------
!*
      if(i_step_output_ucd .gt. 0) then
        if( mod(i_step,i_step_output_ucd) .eq. 0) then
          call sel_write_udt_file(my_rank, i_step, fem_ucd)
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
      subroutine FEM_finalize_back_trans
!
      use m_t_step_parameter
      use output_parallel_ucd_file
!
!
      if(i_step_output_ucd .gt. 0) then
        call finalize_ucd_file_output
      end if
!
      end subroutine FEM_finalize_back_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_back_trans
