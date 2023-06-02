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
!!     &          edge_comm, nod_fld, map_ctls, map, SR_sig, SR_il)
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: edge_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(map_rendering_module), intent(inout) :: map
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine MAP_PROJECTION_visualize                             &
!!     &         (istep_psf, time_d, geofem, nod_fld, map, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!      subroutine MAP_PROJECTION_finalize(map)
!!        type(map_rendering_module), intent(inout) :: map
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
      use t_pvr_image_array
      use t_map_rendering_data
!
      implicit  none
!
      type map_rendering_module
!>        Number of sections
        integer(kind = kint) :: num_map = 0
!
!>        Structure of case table for isosurface
        type(psf_cases) :: psf_case_tbls
!
!>        Structure for table for sections
        type(sectioning_list), allocatable :: map_list(:)
!>        Structure for table for sections
        type(grp_section_list), allocatable :: map_grp_list(:)
!
!>        Structure for search table for sections
        type(psf_search_lists), allocatable :: psf_search(:)
!
!>        Structure of sectioning module parameter
        type(psf_parameters), allocatable :: map_param(:)
!>        Structure of cross sectioning parameter
        type(section_define), allocatable  :: map_def(:)
!>        Structure of projection parameter
        type(pvr_view_parameter), allocatable:: view_param(:)
!>        Structure of color map parameter
        type(pvr_colormap_parameter), allocatable :: color_param(:)
!>        Structure of color bar parameter
        type(pvr_colorbar_parameter), allocatable :: cbar_param(:)
!
!>        Structure for psf patch data on local domain
        type(psf_local_data), allocatable :: map_mesh(:)
!
!>        Structure of color bar parameter
        type(psf_results), allocatable :: map_psf_dat1(:)
!>        Structure of color bar parameter
        type(map_rendering_data), allocatable :: map_data(:)
!>        Structure of color bar parameter
        type(pvr_image_type), allocatable :: map_rgb(:)
      end type map_rendering_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_initialize(increment_psf, geofem,       &
     &          edge_comm, nod_fld, map_ctls, map, SR_sig, SR_il)
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
      type(map_rendering_module), intent(inout) :: map
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: i_psf
!
!
      map%num_map = map_ctls%num_map_ctl
      if(increment_psf .le. 0) map%num_map = 0
      if(map%num_map .le. 0) return
!
      call init_psf_case_tables(map%psf_case_tbls)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_map_field_type'
      call alloc_map_field_type(map)
!
      call s_set_map_control(map%num_map, geofem%group, nod_fld,        &
     &    map_ctls, map%map_param, map%map_def,                         &
     &    map%map_mesh, map%map_rgb, map%view_param,                    &
     &    map%color_param, map%cbar_param)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &   (map%num_map, geofem%mesh, geofem%group,                       &
     &    map%map_param, map%psf_search)
!
!
      do i_psf = 1, map%num_map
        call alloc_node_param_smp(map%map_mesh(i_psf)%node)
        call alloc_ele_param_smp(map%map_mesh(i_psf)%patch)
!
        call alloc_ref_field_4_psf                                      &
     &     (geofem%mesh%node, map%map_list(i_psf))
      end do
!
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+1)
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (map%num_map, map%map_def, geofem%mesh%node, map%map_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (map%num_map, geofem%mesh, geofem%group, edge_comm,            &
     &    map%psf_case_tbls, map%map_def, map%psf_search, map%map_list, &
     &    map%map_grp_list, map%map_mesh, SR_sig, SR_il)
!
      call alloc_psf_field_data(map%num_map, map%map_mesh)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+1)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_mesh'
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+3)
      call output_map_mesh(map%num_map, map%view_param, map%cbar_param, &
     &    map%map_mesh, map%map_psf_dat1, map%map_data, map%map_rgb,    &
     &    SR_sig)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+3)
!
      end subroutine MAP_PROJECTION_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_visualize                               &
     &         (istep_psf, time_d, geofem, nod_fld, map, SR_sig)
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
      type(map_rendering_module), intent(inout) :: map
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if (map%num_map.le.0 .or. istep_psf.le.0) return
!
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+2)
      call set_field_4_psf(map%num_map, geofem%mesh%edge, nod_fld,      &
     &    map%map_def, map%map_param, map%map_list, map%map_grp_list,   &
     &    map%map_mesh)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+2)
!
      if (iflag_debug.eq.1) write(*,*) 'output_section_data'
      if(iflag_PSF_time) call start_elapsed_time(ist_elapsed_PSF+3)
      call output_map_file(map%num_map, istep_psf, time_d,              &
     &    map%map_mesh, map%color_param, map%cbar_param,                &
     &    map%map_psf_dat1, map%map_data, map%map_rgb, SR_sig)
      if(iflag_PSF_time) call end_elapsed_time(ist_elapsed_PSF+3)
!
      end subroutine MAP_PROJECTION_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine MAP_PROJECTION_finalize(map)
!
      use set_map_control
      use set_psf_control
      use set_fields_for_psf
      use find_node_and_patch_psf
!
      type(map_rendering_module), intent(inout) :: map
      integer(kind = kint) :: i_psf
!
!
      if(map%num_map .le. 0) return
!
      do i_psf = 1, map%num_map
        call dealloc_node_param_smp(map%map_mesh(i_psf)%node)
        call dealloc_ele_param_smp(map%map_mesh(i_psf)%patch)
!
        call dealloc_inod_grp_psf(map%map_grp_list(i_psf))
        call dealloc_coefficients_4_psf(map%map_def(i_psf))
        call dealloc_pvr_image_array(map%map_rgb(i_psf))
      end do
!
      call dealloc_psf_node_and_patch                                   &
    &    (map%num_map, map%map_list, map%map_mesh)
      call dealloc_psf_field_name(map%num_map, map%map_mesh)
      call dealloc_psf_field_data(map%num_map, map%map_mesh)
      call dealloc_psf_case_table(map%psf_case_tbls)
!
      deallocate(map%map_mesh, map%map_list, map%map_grp_list)
      deallocate(map%psf_search)
      deallocate(map%map_param)
      deallocate(map%map_rgb, map%map_data)
!
      end subroutine MAP_PROJECTION_finalize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_map_field_type(map)
!
      use m_field_file_format
!
      type(map_rendering_module), intent(inout) :: map
      integer(kind = kint) :: i_psf
!
!
      allocate(map%map_mesh(map%num_map))
      allocate(map%map_list(map%num_map))
      allocate(map%map_grp_list(map%num_map))
      allocate(map%psf_search(map%num_map))
      allocate(map%map_param(map%num_map))
      allocate(map%map_def(map%num_map))
!
      allocate(map%view_param(map%num_map))
      allocate(map%color_param(map%num_map))
      allocate(map%cbar_param(map%num_map))
      allocate(map%map_data(map%num_map))
      allocate(map%map_rgb(map%num_map))
      allocate(map%map_psf_dat1(map%num_map))
!
      do i_psf = 1, map%num_map
        call alloc_coefficients_4_psf(map%map_def(i_psf))
      end do
!
      end subroutine alloc_map_field_type
!
!  ---------------------------------------------------------------------
!
      end module t_map_projection
