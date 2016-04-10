!>@file   FEM_analyzer_sph_MHD.f90
!!@brief  module FEM_analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_initialize_sph_MHD(mesh, group, ele_mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!      subroutine FEM_analyze_sph_MHD(i_step                           &
!!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!!      subroutine FEM_finalize
!!
!!      subroutine SPH_to_FEM_bridge_MHD
!!      subroutine FEM_to_SPH_bridge
!!@endverbatim
!!
!!@n @param  i_step       Current time step
!!@n @param  istep_psf    Time step increment for cross sectioning
!!@n @param  istep_iso    Time step increment for iso surfaces
!!@n @param  istep_pvr    Time step increment for volume rendering
!!@n @param  istep_fline  Time step increment for field line generation
!!@n @param  visval       Return flag to call visualization routines
!
      module FEM_analyzer_sph_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_work_time
!
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_MHD(mesh, group, ele_mesh)
!
      use m_array_for_send_recv
      use m_t_step_parameter
      use m_node_phys_data
      use m_cal_max_indices
      use t_FEM_phys_data
!
      use t_mesh_data
!
      use nod_phys_send_recv
      use range_data_IO
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(mesh%nod_comm)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(mesh, ele_mesh)
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call deallocate_edge_geom_type(ele_mesh%edge)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_field_address_type'
      call set_field_address_type(mesh%node%numnod, nod_fld1, iphys)
!
!  connect grid data to volume output
!
      if(i_step_output_ucd.gt.0) then
        call alloc_phys_range(nod_fld1%ntot_phys_viz, range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(mesh, nod_fld1)
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD(i_step,                            &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use set_exit_flag_4_visualizer
      use output_viz_file_control
!
      integer (kind =kint), intent(in) :: i_step
!
      integer (kind =kint), intent(inout) :: visval
      integer(kind = kint), intent(inout) :: istep_psf, istep_iso
      integer(kind = kint), intent(inout) :: istep_pvr, istep_fline
!
      integer (kind =kint) :: iflag
!
!
      visval = 1
      call set_lead_physical_values_flag(iflag)
!
      if(iflag .eq. 0) then
!*  ----------   Count steps for visualization
!*
        call set_flag_to_visualization(i_step,                          &
     &        istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!*
!*  -----------  Output volume data --------------
!*
        call s_output_ucd_file_control
      end if
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD(mesh)
!
      use m_node_phys_data
!
      use t_mesh_data
!
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
!
      type(mesh_geometry), intent(inout) :: mesh
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'copy_forces_to_snapshot_rtp'
      call copy_forces_to_snapshot_rtp(mesh%node, iphys, nod_fld1)
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_fld_from_trans'
      call copy_snap_vec_fld_from_trans(mesh%node, iphys, nod_fld1)
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_fld_to_trans'
      call copy_snap_vec_fld_to_trans(mesh%node, iphys, nod_fld1)
!
      if (iflag_debug.gt.0) write(*,*) 'overwrite_nodal_sph_2_xyz'
      call overwrite_nodal_sph_2_xyz(mesh%node, nod_fld1)
!
!*  ----------- transform field at pole and center --------------
!*
      if (iflag_debug.gt.0) write(*,*) 'lead_pole_fields_4_sph_mhd'
      call lead_pole_fields_4_sph_mhd(mesh%node, iphys, nod_fld1)
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh%node, mesh%nod_comm, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_MHD
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
      subroutine FEM_finalize
!
      use m_t_step_parameter
      use m_cal_max_indices
!
!
     if(i_step_output_ucd.gt.0) then
       call dealloc_phys_range(range)
       call finalize_output_ucd
     end if
!
      end subroutine FEM_finalize
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_MHD
