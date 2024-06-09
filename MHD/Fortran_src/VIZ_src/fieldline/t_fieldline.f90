!>@file   t_fieldline.f90
!!@brief  module t_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize                                     &
!!     &         (increment_fline, geofem, nod_fld, fline_ctls, fline)
!!      subroutine FLINE_visualize(istep_fline, time_d, geofem,         &
!!     &                           para_surf, nod_fld, fline, m_SR)
!!      subroutine FLINE_finalize(fline)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!@endverbatim
!
      module t_fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_paralell_surface_indices
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_local_fline
      use t_ucd_data
!
      implicit  none
!
      type fieldline_module
        integer(kind = kint) :: num_fline
!
        type(fieldline_paramter), allocatable :: fln_prm(:)
!
        type(each_fieldline_source), allocatable :: fln_src(:)
        type(each_fieldline_trace), allocatable :: fln_tce(:)
        type(local_fieldline), allocatable  :: fline_lc(:)
        type(broadcast_trace_data), allocatable :: fln_bcast(:)
        type(trace_data_send_recv), allocatable :: fln_SR(:)
!
        type(ucd_data) :: fline_ucd
      end type fieldline_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_initialize                                       &
     &         (increment_fline, geofem, nod_fld, fline_ctls, fline)
!
      use calypso_mpi
      use calypso_mpi_int
      use m_connect_hexa_2_tetra
      use t_control_data_flines
      use multi_tracer_fieldline
!
      integer(kind = kint), intent(in) :: increment_fline
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
      fline%num_fline = fline_ctls%num_fline_ctl
      if(increment_fline .le. 0) fline%num_fline = 0
      if(fline%num_fline .le. 0) return
!
      call alloc_FLINE_modules(fline)
!
      call set_fline_controls(geofem%mesh, geofem%group, nod_fld,       &
     &    fline%num_fline, fline_ctls, fline%fln_prm, fline%fln_src)
      call dealloc_fline_ctl_struct(fline_ctls)
!
      call alloc_each_FLINE_data                                        &
     &   (fline%num_fline, fline%fln_prm, fline%fln_src, fline%fln_tce, &
     &    fline%fline_lc, fline%fln_SR, fline%fln_bcast)
      call set_fixed_FLINE_seed_points                                  &
     &   (geofem%mesh, nod_fld, fline%num_fline, fline%fln_prm,         &
     &    fline%fln_src, fline%fln_tce)
!
      end subroutine FLINE_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_FLINE_modules(fline)
!
      type(fieldline_module), intent(inout) :: fline
!
      allocate(fline%fln_prm(fline%num_fline))
      allocate(fline%fln_src(fline%num_fline))
      allocate(fline%fln_tce(fline%num_fline))
      allocate(fline%fln_SR(fline%num_fline))
      allocate(fline%fln_bcast(fline%num_fline))
      allocate(fline%fline_lc(fline%num_fline))
!
      end subroutine alloc_FLINE_modules
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_visualize(istep_fline, time_d, geofem,           &
     &                           para_surf, nod_fld, fline, m_SR)
!
      use multi_tracer_fieldline
      use const_field_lines
      use multi_tracer_file_IO
      use t_mesh_SR
!
!
      integer(kind = kint), intent(in) :: istep_fline
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_module), intent(inout) :: fline
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!  
      if (fline%num_fline.le.0 .or. istep_fline.le.0) return
!
      call set_FLINE_seed_fields                                        &
     &   (geofem%mesh, geofem%group, para_surf, nod_fld,                &
     &   fline%num_fline, fline%fln_prm, fline%fln_src, fline%fln_tce)

!
      call s_const_field_lines(geofem%mesh, para_surf, nod_fld,         &
     &    fline%num_fline, fline%fln_prm, fline%fln_tce,                &
     &    fline%fln_SR, fline%fln_bcast, fline%fline_lc, m_SR)
      call output_field_lines(istep_fline, time_d, fline%num_fline,     &
     &                         fline%fln_prm, fline%fline_lc)
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_finalize(fline)
!
      use multi_tracer_fieldline
!
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
!
      if (fline%num_fline .le. 0) return
!
!
      call dealloc_each_FLINE_data(fline%num_fline, fline%fln_prm,      &
     &    fline%fln_src, fline%fln_tce, fline%fline_lc,                 &
     &    fline%fln_SR, fline%fln_bcast)           
      deallocate(fline%fln_src, fline%fline_lc, fline%fln_bcast)
      deallocate(fline%fln_tce, fline%fln_prm, fline%fln_SR)
!
      end subroutine FLINE_finalize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_const_field_lines                                    &
     &         (mesh, para_surf, nod_fld, num_fline, fln_prm, fln_tce,  &
     &          fln_SR, fln_bcast, fline_lc, m_SR)
!
      use const_field_lines
!
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) ::      fline_lc(num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call const_field_line(mesh, para_surf, nod_fld, fln_prm(i_fln), &
     &      fln_tce(i_fln), fln_SR(i_fln), fln_bcast(i_fln),            &
     &      fline_lc(i_fln), m_SR)
      end do
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      end module t_fieldline
