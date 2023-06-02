!>@file   t_map_projection.f90
!!@brief  module t_map_projection
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine MAP_PROJECTION_initialize(increment_psf, geofem,     &
!!     &          edge_comm, nod_fld, map_ctls, psf, SR_sig, SR_il)
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(sectioning_module), intent(inout) :: psf
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine MAP_PROJECTION_visualize                             &
!!     &         (istep_psf, time_d, geofem, nod_fld, psf, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine MAP_PROJECTION_finalize(psf)
!!        type(sectioning_module), intent(inout) :: psf
!!@endverbatim
      module t_map_projection
!
      use calypso_mpi
      use m_precision
!
      use t_cross_section
      use t_psf_results
      use t_control_data_maps
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
!
      implicit  none
!
!      type sectioning_module
!>        Number of sections
!        integer(kind = kint) :: num_psf = 0
!
!>        Structure of case table for isosurface
!        type(psf_cases) :: psf_case_tbls
!
!>        Structure for table for sections
!        type(sectioning_list), allocatable :: psf_list(:)
!>        Structure for table for sections
!        type(grp_section_list), allocatable :: psf_grp_list(:)
!
!>        Structure for search table for sections
!        type(psf_search_lists), allocatable :: psf_search(:)
!
!>        Structure of sectioning module parameter
!        type(psf_parameters), allocatable :: psf_param(:)
!>        Structure of cross sectioning parameter
!        type(section_define), allocatable  :: psf_def(:)
!
!>        Structure for psf patch data on local domain
!        type(psf_local_data), allocatable :: psf_mesh(:)
!
!>        Structure for psf time output
!        type(time_data) :: psf_time_IO
!>        Structure for psf data output
!        type(field_IO_params), allocatable :: psf_file_IO(:)
!
!>        Structure for cross sectioning output (used by master process)
!        type(ucd_data), allocatable :: psf_out(:)
!      end type sectioning_module
!
      type(psf_results), allocatable :: map_psf_dat1(:)
      type(pvr_view_parameter), allocatable:: view_param1(:)
      type(pvr_colormap_parameter), allocatable :: color_param1(:)
      type(pvr_colorbar_parameter), allocatable :: cbar_param1(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_initialize(increment_psf, geofem,       &
     &          edge_comm, nod_fld, map_ctls, psf, SR_sig, SR_il)
!
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use m_geometry_constants
!
      use calypso_mpi
      use set_map_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use collect_psf_data
!
      integer(kind = kint), intent(in) :: increment_psf
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: edge_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(sectioning_module), intent(inout) :: psf
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: i_psf
!
!
      psf%num_psf = map_ctls%num_map_ctl
      if(increment_psf .le. 0) psf%num_psf = 0
      if(psf%num_psf .le. 0) return
!
      call init_psf_case_tables(psf%psf_case_tbls)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_psf_field_type'
      call alloc_psf_field_type(psf)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_map_control'
      allocate(view_param1(psf%num_psf))
      allocate(color_param1(psf%num_psf))
      allocate(cbar_param1(psf%num_psf))
      call s_set_map_control(psf%num_psf, geofem%group, nod_fld,        &
     &    map_ctls, psf%psf_param, psf%psf_def,                         &
     &    psf%psf_mesh, psf%psf_file_IO,                                &
     &    view_param1, color_param1, cbar_param1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (psf%num_psf, geofem%mesh, geofem%group,                       &
     &    psf%psf_param, psf%psf_search)
!
!
      allocate(map_psf_dat1(psf%num_psf))
      do i_psf = 1, psf%num_psf
        call alloc_node_param_smp(psf%psf_mesh(i_psf)%node)
        call alloc_ele_param_smp(psf%psf_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf                                      &
     &     (geofem%mesh%node, psf%psf_list(i_psf))
      end do
!
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+1)
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (psf%num_psf, psf%psf_def, geofem%mesh%node, psf%psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (psf%num_psf, geofem%mesh, geofem%group, edge_comm,            &
     &    psf%psf_case_tbls, psf%psf_def, psf%psf_search, psf%psf_list, &
     &    psf%psf_grp_list, psf%psf_mesh, SR_sig, SR_il)
!
      call alloc_psf_field_data(psf%num_psf, psf%psf_mesh)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+1)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+3)
      call output_map_mesh(psf%num_psf, psf%psf_mesh, psf%psf_file_IO,  &
     &                     map_psf_dat1, psf%psf_out, SR_sig)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+3)
!
      end subroutine MAP_PROJECTION_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_visualize                               &
     &         (istep_psf, time_d, geofem, nod_fld, psf, SR_sig)
!
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use set_fields_for_psf
      use set_ucd_data_to_type
      use collect_psf_data
!
      integer(kind = kint), intent(in) :: istep_psf
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(sectioning_module), intent(inout) :: psf
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if (psf%num_psf.le.0 .or. istep_psf.le.0) return
!
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+2)
      call set_field_4_psf(psf%num_psf, geofem%mesh%edge, nod_fld,      &
     &    psf%psf_def, psf%psf_param, psf%psf_list, psf%psf_grp_list,   &
     &    psf%psf_mesh)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+2)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_data'
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+3)
      call output_map_file(psf%num_psf, psf%psf_file_IO, istep_psf,     &
     &                     time_d, psf%psf_mesh, psf%psf_time_IO,       &
     &                     map_psf_dat1, psf%psf_out, view_param1,      &
     &                     color_param1, cbar_param1, SR_sig)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+3)
!
      end subroutine MAP_PROJECTION_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_finalize(psf)
!
      use set_map_control
      use set_psf_control
      use set_fields_for_psf
      use find_node_and_patch_psf
!
      type(sectioning_module), intent(inout) :: psf
      integer(kind = kint) :: i_psf
!
!
      if(psf%num_psf .le. 0) return
!
      do i_psf = 1, psf%num_psf
        call dealloc_node_param_smp(psf%psf_mesh(i_psf)%node)
        call dealloc_ele_param_smp(psf%psf_mesh(i_psf)%patch)
!
        call disconnect_merged_ucd_mesh(psf%psf_out(i_psf))
!
        call dealloc_inod_grp_psf(psf%psf_grp_list(i_psf))
        call dealloc_coefficients_4_psf(psf%psf_def(i_psf))
      end do
!
      call dealloc_psf_node_and_patch                                   &
    &    (psf%num_psf, psf%psf_list, psf%psf_mesh)
      call dealloc_psf_field_name(psf%num_psf, psf%psf_mesh)
      call dealloc_psf_field_data(psf%num_psf, psf%psf_mesh)
      call dealloc_psf_case_table(psf%psf_case_tbls)
!
      deallocate(psf%psf_mesh, psf%psf_list, psf%psf_grp_list)
      deallocate(psf%psf_search, psf%psf_file_IO)
      deallocate(psf%psf_out, psf%psf_param)
!
      end subroutine MAP_PROJECTION_finalize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_map_field_type(psf)
!
      use m_field_file_format
!
      type(sectioning_module), intent(inout) :: psf
      integer(kind = kint) :: i_psf
!
!
      allocate(psf%psf_mesh(psf%num_psf))
      allocate(psf%psf_list(psf%num_psf))
      allocate(psf%psf_grp_list(psf%num_psf))
      allocate(psf%psf_search(psf%num_psf))
      allocate(psf%psf_param(psf%num_psf))
      allocate(psf%psf_def(psf%num_psf))
!
      allocate(psf%psf_file_IO(psf%num_psf))
      allocate(psf%psf_out(psf%num_psf))
!
      do i_psf = 1, psf%num_psf
        call alloc_coefficients_4_psf(psf%psf_def(i_psf))
      end do
!
      psf%psf_file_IO(1:psf%num_psf)%iflag_format = iflag_sgl_udt
!
      end subroutine alloc_map_field_type
!
!  ---------------------------------------------------------------------
!
      end module t_map_projection
