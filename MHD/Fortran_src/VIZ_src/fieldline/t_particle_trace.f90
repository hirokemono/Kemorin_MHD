!>@file   t_particle_trace.f90
!!@brief  module t_particle_trace
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine TRACER_initialize(increment_fline, geofem, para_surf,&
!!     &                             nod_fld, fline_ctls, fline)
!!        integer(kind = kint), intent(in) :: increment_fline
!!        type(mesh_data), intent(in) :: geofem
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!      subroutine TRACER_visualize                                     &
!!     &         (istep_fline, time_d, fem, next_tbl, nod_fld, fline)
!!      subroutine TRACER_finalize(fline)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!@endverbatim
!
      module t_particle_trace
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
      use t_IO_step_parameter
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
      end type fieldline_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_initialize(init_d, finish_d, rst_step,          &
     &                             geofem, para_surf,                   &
     &                             nod_fld, fline_ctls, fline)
!
      use t_control_data_flines
      use m_connect_hexa_2_tetra
      use multi_tracer_fieldline
!
      type(time_data), intent(in) :: init_d
      type(finish_data), intent(in) :: finish_d
      type(IO_step_param), intent(in) :: rst_step
!
      type(mesh_data), intent(in) :: geofem
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
      fline%num_fline = fline_ctls%num_fline_ctl
      if(fline%num_fline .le. 0) return
!
      allocate(fline%fln_prm(fline%num_fline))
      allocate(fline%fln_src(fline%num_fline))
      allocate(fline%fln_tce(fline%num_fline))
      allocate(fline%fln_SR(fline%num_fline))
      allocate(fline%fln_bcast(fline%num_fline))
      allocate(fline%fline_lc(fline%num_fline))
!
      call set_fline_controls(geofem%mesh, geofem%group, nod_fld,       &
     &    fline%num_fline, fline_ctls, fline%fln_prm, fline%fln_src)
      call dealloc_fline_ctl_struct(fline_ctls)
!
      call alloc_each_FLINE_data                                        &
     &   (fline%num_fline, fline%fln_prm, fline%fln_src, fline%fln_tce, &
     &    fline%fline_lc, fline%fln_SR, fline%fln_bcast)
      call set_fixed_FLINE_seed_points(geofem%mesh, fline%num_fline,    &
     &    fline%fln_prm, fline%fln_src, fline%fln_tce)
      call set_FLINE_seed_fields                                        &
     &   (geofem%mesh, geofem%group, para_surf, nod_fld,                &
     &   fline%num_fline, fline%fln_prm, fline%fln_src, fline%fln_tce)
!
      end subroutine TRACER_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_evolution                                       &
     &         (increment_output, time_d, finish_d, rst_step, geofem,   &
     &          para_surf, nod_fld, fline, m_SR)
!
      use t_mesh_SR
      use set_fields_for_fieldline
      use trace_particle
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
      use tracer_restart_file_IO
!
!
      integer(kind = kint), intent(in) :: increment_output
      type(time_data), intent(in) :: time_d
      type(finish_data), intent(in) :: finish_d
      type(IO_step_param), intent(in) :: rst_step
      type(mesh_data), intent(in) :: geofem
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_module), intent(inout) :: fline
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!  
      call trace_particle_sets(time_d, geofem%mesh, para_surf,          &
     &    nod_fld, fline%num_fline, fline%fln_prm, fline%fln_src,       &
     &    fline%fln_tce, fline%fline_lc, fline%fln_SR, fline%fln_bcast, &
     &    m_SR)
!
      call output_tracer_restarts(time_d, finish_d, rst_step,           &
     &    fline%num_fline, fline%fln_prm, fline%fln_tce, fline%fline_lc)
!
      call TRACER_visualize(increment_output, time_d,                   &
     &    fline%num_fline, fline%fln_prm, fline%fline_lc)
!
      end subroutine TRACER_evolution
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_visualize(increment_output, time_d,             &
     &                            num_fline, fln_prm, fline_lc)
!
      use t_mesh_SR
      use set_fields_for_fieldline
      use trace_particle
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
!
!
      integer(kind = kint), intent(in) :: increment_output
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      type(time_data) :: t_IO
      type(ucd_data) :: fline_ucd
      integer(kind = kint) :: i_fln, istep_fline
!  
      if(mod(time_d%i_time_step, increment_output) .ne. 0) return
      istep_fline = time_d%i_time_step / increment_output
!
      do i_fln = 1, num_fline
        call copy_time_step_size_data(time_d, t_IO)
        call copy_local_particles_to_IO                                 &
     &     (fln_prm(i_fln)%fline_fields, fline_lc(i_fln),   &
     &      fline_ucd)
        call sel_write_parallel_ucd_file                                &
     &     (istep_fline, fln_prm(i_fln)%fline_file_IO, t_IO,      &
     &      fline_ucd)
        call deallocate_parallel_ucd_mesh(fline_ucd)
      end do
!
      end subroutine TRACER_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_finalize(fline)
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
      end subroutine TRACER_finalize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trace_particle_sets(time_d, mesh, para_surf, nod_fld,  &
     &          num_fline, fln_prm, fln_src, fln_tce, fline_lc,         &
     &          fln_SR, fln_bcast, m_SR)
!
!
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) ::      fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) ::      fline_lc(num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!  
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_trace_particle', i_fln
        call s_trace_particle(time_d%dt, mesh, para_surf, nod_fld,      &
     &      fln_prm(i_fln), fln_tce(i_fln), fline_lc(i_fln),            &
     &      fln_SR(i_fln), fln_bcast(i_fln), fln_src(i_fln)%v_prev,     &
     &      m_SR)
      end do
!
      end subroutine trace_particle_sets
!
!  ---------------------------------------------------------------------
!
      end module t_particle_trace
