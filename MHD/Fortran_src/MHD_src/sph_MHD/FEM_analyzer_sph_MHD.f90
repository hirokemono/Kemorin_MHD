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
!!      subroutine FEM_initialize_sph_MHD                               &
!!     &         (mesh, group, ele_mesh, iphys, nod_fld, range)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(maximum_informations), intent(inout) :: range
!!      subroutine FEM_analyze_sph_MHD(i_step, mesh, nod_fld,           &
!!     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine FEM_finalize
!!
!!      subroutine SPH_to_FEM_bridge_MHD                                &
!!     &         (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_MHD), intent(in) :: WK
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
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
      subroutine FEM_initialize_sph_MHD                                 &
     &         (mesh, group, ele_mesh, iphys, nod_fld, range)
!
      use m_array_for_send_recv
      use m_t_step_parameter
      use t_phys_data
      use t_phys_address
      use t_FEM_phys_data
      use t_cal_max_indices
!
      use t_mesh_data
!
      use nod_phys_send_recv
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(maximum_informations), intent(inout) :: range
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
      if (iflag_debug.gt.0) write(*,*) 'init_field_address'
      call init_field_address(mesh%node%numnod, nod_fld, iphys)
!
!  connect grid data to volume output
!
      if(i_step_output_ucd.gt.0) then
        call alloc_phys_range(nod_fld%ntot_phys_viz, range)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'output_grd_file_4_snapshot'
      call output_grd_file_4_snapshot(mesh, nod_fld)
!
      end subroutine FEM_initialize_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_MHD(i_step, mesh, nod_fld,             &
     &          istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
      use output_viz_file_control
!
      integer (kind =kint), intent(in) :: i_step
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
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
      if(iflag .ne. 0) return
!
!*  ----------   Count steps for visualization
!*
      call set_flag_to_visualization(i_step,                            &
     &   istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!*
!
!*  ----------- Data communication  --------------
!
      if (iflag_debug.gt.0) write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh%nod_comm, nod_fld)
!
!*  -----------  Output volume data --------------
!*
      call s_output_ucd_file_control
!
      end subroutine FEM_analyze_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_MHD                                  &
     &         (sph_params, sph_rtp, WK, mesh, iphys, nod_fld)
!
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use m_SGS_control_parameter
!
      use output_viz_file_control
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use coordinate_convert_4_sph
      use copy_SGS_4_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(in) :: WK
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
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
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                   &
     &    mesh%node, iphys, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_fld_from_trans'
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_snap_vec_force_from_trans'
      call copy_snap_vec_force_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
!
      if(SGS_par1%model_p%iflag_SGS .eq. 0) return
!
      if (iflag_debug.gt.0) write(*,*) 'copy_filtered_field_from_trans'
      call copy_filtered_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                   &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_wide_SGS_field_from_trans'
      call copy_wide_SGS_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                   &
     &    mesh%node, iphys, nod_fld)
      if (iflag_debug.gt.0) write(*,*) 'copy_SGS_force_from_trans'
!      call copy_SGS_force_from_trans                                   &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                  &
!     &    mesh%node, iphys, nod_fld)
      call copy_SGS_snap_fld_from_trans                                 &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
      call copy_SGS_diff_field_from_trans                               &
     &   (sph_params%m_folding, sph_rtp, WK%trns_snap,                  &
     &    mesh%node, iphys, nod_fld)
!
!
!!!!!   These routines are for debugging. Be careful!
!
!  Check nonlinear terms by filtered field as SGS term list
!      call copy_filtered_forces_to_snap                                &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_MHD,                  &
!     &    mesh%node, iphys, nod_fld)
!
!  Check filtered nonlinear terms by using SGS term list
!      call copy_SGS_field_from_trans                                   &
!     &   (sph_params%m_folding, sph_rtp, WK%trns_SGS,                  &
!     &    mesh%node, iphys, nod_fld)
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
