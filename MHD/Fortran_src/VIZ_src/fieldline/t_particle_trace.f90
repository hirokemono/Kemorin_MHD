!>@file   t_particle_trace.f90
!!@brief  module t_particle_trace
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine TRACER_initialize(init_d, finish_d, rst_step,        &
!!     &                             geofem, para_surf,                 &
!!     &                             nod_fld, tracer_ctls, tracer)
!!        type(time_data), intent(in) :: init_d
!!        type(finish_data), intent(in) :: finish_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(mesh_data), intent(in) :: geofem
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: tracer_ctls
!!        type(tracer_module), intent(inout) :: tracer
!!      subroutine TRACER_evolution                                     &
!!     &         (time_d, finish_d, rst_step, istep_tracer,             &
!!     &          geofem, para_surf, nod_fld, tracer, m_SR)
!!        integer(kind = kint), intent(in) :: istep_tracer
!!        type(time_data), intent(in) :: time_d
!!        type(finish_data), intent(in) :: finish_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(IO_step_param), intent(in) :: TRACER_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(inout) :: tracer
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine TRACER_visualize(istep_tracer, time_d, rst_step,     &
!!     &                            tracer)
!!        integer(kind = kint), intent(in) :: istep_tracer
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(tracer_module), intent(inout) :: tracer
!!      subroutine TRACER_finalize(fline)
!!        type(tracer_module), intent(inout) :: tracer
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
!
      implicit  none
!
      type tracer_module
        integer(kind = kint) :: num_trace
!
        type(fieldline_paramter), allocatable :: fln_prm(:)
!
        type(each_fieldline_source), allocatable :: fln_src(:)
        type(each_fieldline_trace), allocatable :: fln_tce(:)
        type(local_fieldline), allocatable  :: fline_lc(:)
        type(broadcast_trace_data), allocatable :: fln_bcast(:)
        type(trace_data_send_recv), allocatable :: fln_SR(:)
      end type tracer_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_initialize(init_d, finish_d, rst_step,          &
     &                             geofem, para_surf,                   &
     &                             nod_fld, tracer_ctls, tracer)
!
      use t_control_data_flines
      use m_connect_hexa_2_tetra
      use multi_tracer_fieldline
      use multi_tracer_file_IO
!
      type(time_data), intent(in) :: init_d
      type(finish_data), intent(in) :: finish_d
      type(IO_step_param), intent(in) :: rst_step
!
      type(mesh_data), intent(in) :: geofem
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: tracer_ctls
      type(tracer_module), intent(inout) :: tracer
!
!
      tracer%num_trace = tracer_ctls%num_fline_ctl
      if(tracer%num_trace .le. 0) return
!
      call alloc_TRACER_modules(tracer)

      call set_fline_controls(geofem%mesh, geofem%group, nod_fld,       &
     &                        tracer%num_trace, tracer_ctls,            &
     &                        tracer%fln_prm, tracer%fln_src)
      call dealloc_fline_ctl_struct(tracer_ctls)
!
      call alloc_each_FLINE_data(tracer%num_trace, tracer%fln_prm,      &
     &    tracer%fln_src, tracer%fln_tce, tracer%fline_lc,              &
     &    tracer%fln_SR, tracer%fln_bcast)
      call alloc_each_TRACER_data(geofem%mesh%node, tracer%num_trace,   &
     &                            tracer%fln_src)

      call set_fixed_FLINE_seed_points(geofem%mesh, tracer%num_trace,   &
     &    tracer%fln_prm, tracer%fln_src, tracer%fln_tce)

      call set_FLINE_seed_fields                                        &
     &   (geofem%mesh, geofem%group, para_surf, nod_fld,                &
     &    tracer%num_trace, tracer%fln_prm, tracer%fln_src,             &
     &    tracer%fln_tce)
!
      call sel_input_tracer_restarts(init_d, rst_step,                  &
     &                               tracer%num_trace, tracer%fln_prm,  &
     &                               tracer%fln_tce, tracer%fline_lc)
!
      end subroutine TRACER_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_evolution                                       &
     &         (time_d, finish_d, rst_step, istep_tracer,               &
     &          geofem, para_surf, nod_fld, tracer, m_SR)
!
      use t_mesh_SR
      use set_fields_for_fieldline
      use trace_particle
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
      use multi_tracer_fieldline
      use multi_tracer_file_IO
!
!
      integer(kind = kint), intent(in) :: istep_tracer
      type(time_data), intent(in) :: time_d
      type(finish_data), intent(in) :: finish_d
      type(IO_step_param), intent(in) :: rst_step
      type(mesh_data), intent(in) :: geofem
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      type(tracer_module), intent(inout) :: tracer
      type(mesh_SR), intent(inout) :: m_SR
!
!  
      if(tracer%num_trace .le. 0) return
      call trace_particle_sets(time_d, geofem%mesh, para_surf,          &
     &    nod_fld, tracer%num_trace, tracer%fln_prm,                    &
     &    tracer%fln_src, tracer%fln_tce, tracer%fline_lc,              &
     &    tracer%fln_SR, tracer%fln_bcast, m_SR)
!
      call output_tracer_restarts(time_d, finish_d, rst_step,           &
     &    tracer%num_trace, tracer%fln_prm, tracer%fline_lc)
!
      if(istep_tracer .le. 0) return
      call output_tracer_viz_files(istep_tracer, time_d,                &
     &    tracer%num_trace, tracer%fln_prm, tracer%fline_lc)
!
      end subroutine TRACER_evolution
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_visualize(istep_tracer, time_d, rst_step,       &
     &                            tracer)
!
      use t_mesh_SR
      use set_fields_for_fieldline
      use multi_tracer_file_IO
!
!
      integer(kind = kint), intent(in) :: istep_tracer
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: rst_step
      type(tracer_module), intent(inout) :: tracer
!
!
      if(tracer%num_trace .le. 0) return
      call input_tracer_restarts(time_d, rst_step, tracer%num_trace,    &
     &                           tracer%fln_prm, tracer%fline_lc)
!
      if(istep_tracer .le. 0) return
      call output_tracer_viz_files(istep_tracer, time_d,                &
     &    tracer%num_trace, tracer%fln_prm, tracer%fline_lc)
!
      end subroutine TRACER_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trace_particle_sets(time_d, mesh, para_surf, nod_fld,  &
     &          num_trace, fln_prm, fln_src, fln_tce, fline_lc,         &
     &          fln_SR, fln_bcast, m_SR)
!
      use trace_particle
!
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_trace
      type(fieldline_paramter), intent(in) ::      fln_prm(num_trace)
      type(each_fieldline_source), intent(inout) :: fln_src(num_trace)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_trace)
      type(local_fieldline), intent(inout) ::      fline_lc(num_trace)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_trace)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_trace)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!  
      do i_fln = 1, num_trace
        if (iflag_debug.eq.1) write(*,*) 's_trace_particle start', i_fln
        call s_trace_particle(time_d%dt, mesh, para_surf, nod_fld,      &
     &      fln_prm(i_fln), fln_tce(i_fln), fline_lc(i_fln),            &
     &      fln_SR(i_fln), fln_bcast(i_fln), fln_src(i_fln)%v_prev,     &
     &      m_SR)
      end do
!
      end subroutine trace_particle_sets
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_TRACER_modules(tracer)
!
      type(tracer_module), intent(inout) :: tracer
!
      allocate(tracer%fln_prm(tracer%num_trace))
      allocate(tracer%fln_src(tracer%num_trace))
      allocate(tracer%fln_tce(tracer%num_trace))
      allocate(tracer%fln_SR(tracer%num_trace))
      allocate(tracer%fln_bcast(tracer%num_trace))
      allocate(tracer%fline_lc(tracer%num_trace))
!
      end subroutine alloc_TRACER_modules
!
!  ---------------------------------------------------------------------
!
      subroutine TRACER_finalize(tracer)
!
      use multi_tracer_fieldline
!
      type(tracer_module), intent(inout) :: tracer
!
!
      if (tracer%num_trace .le. 0) return
      call dealloc_each_TRACER_data(tracer%num_trace, tracer%fln_src)
      call dealloc_each_FLINE_data(tracer%num_trace, tracer%fln_prm,    &
     &    tracer%fln_src, tracer%fln_tce, tracer%fline_lc,              &
     &    tracer%fln_SR, tracer%fln_bcast)           
      deallocate(tracer%fln_src, tracer%fline_lc, tracer%fln_bcast)
      deallocate(tracer%fln_tce, tracer%fln_prm, tracer%fln_SR)
!
      end subroutine TRACER_finalize
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_each_TRACER_data(node, num_trace, fln_src)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_trace
!
      type(each_fieldline_source), intent(inout) :: fln_src(num_trace)
!
      integer(kind = kint) :: i_fln
!
      do i_fln = 1, num_trace
        call alloc_velocity_at_previous(node%numnod, fln_src(i_fln))
      end do
!
      end subroutine alloc_each_TRACER_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_TRACER_data(num_trace, fln_src)
!
      integer(kind = kint), intent(in) :: num_trace
!
      type(each_fieldline_source), intent(inout) :: fln_src(num_trace)
!
      integer(kind = kint) :: i_fln
!
      if (num_trace .le. 0) return
      do i_fln = 1, num_trace
        call dealloc_velocity_at_previous(fln_src(i_fln))
      end do
!
      end subroutine dealloc_each_TRACER_data
!
!  ---------------------------------------------------------------------
!
      end module t_particle_trace
