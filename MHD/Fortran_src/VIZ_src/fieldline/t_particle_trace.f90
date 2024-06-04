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
      use calypso_mpi
      use calypso_mpi_int
      use m_connect_hexa_2_tetra
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use set_fline_control
      use set_fline_seeds_from_list
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
      type(FLINE_element_size) :: fln_dist
      logical :: flag_fln_dist
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
      do i_fln = 1, fline%num_fline
        call s_set_fline_control(geofem%mesh, geofem%group, nod_fld,    &
     &      fline_ctls%fline_ctl_struct(i_fln), fline%fln_prm(i_fln),   &
     &      fline%fln_src(i_fln))
      end do
!
      call dealloc_fline_ctl_struct(fline_ctls)
!
      do i_fln = 1, fline%num_fline
        call alloc_start_point_fline(nprocs, fline%fln_prm(i_fln),      &
     &                               fline%fln_src(i_fln))
        call alloc_num_gl_start_fline(nprocs,                           &
     &      fline%fln_prm(i_fln)%fline_fields,                          &
     &      fline%fln_tce(i_fln))
        call alloc_broadcast_trace_data                                 &
     &     (fline%fln_prm(i_fln)%num_each_field_line,                   &
     &      fline%fln_prm(i_fln)%fline_fields, fline%fln_bcast(i_fln))
        call alloc_local_fline(fline%fln_prm(i_fln)%fline_fields,       &
     &                         fline%fline_lc(i_fln))
        call alloc_trace_data_SR_num(fline%fln_prm(i_fln)%fline_fields, &
     &                               fline%fln_SR(i_fln))
        call alloc_velocity_at_previous(nod_fld%n_point,                &
     &                                  fline%fln_src(i_fln))
      end do
!
!
      flag_fln_dist = .FALSE.
      do i_fln = 1, fline%num_fline
        if(fline%fln_prm(i_fln)%id_fline_seed_type                      &
     &      .eq. iflag_position_list) flag_fln_dist = .TRUE.
      end do
      if(flag_fln_dist) then
        call alloc_FLINE_element_size(geofem%mesh%ele, fln_dist)
        call cal_FLINE_element_size(geofem%mesh%node, geofem%mesh%ele,  &
     &                              fln_dist)
      end if
      do i_fln = 1, fline%num_fline
        if(fline%fln_prm(i_fln)%id_fline_seed_type                      &
     &                       .eq. iflag_position_list) then
          call alloc_init_tracer_position(fline%fln_prm(i_fln),         &
     &                                    fline%fln_src(i_fln))
          call init_FLINE_seed_from_list                                &
     &       (geofem%mesh%node, geofem%mesh%ele,                        &
     &        fline%fln_prm(i_fln), fline%fln_src(i_fln),               &
     &        fline%fln_tce(i_fln), fln_dist)
        end if
      end do
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      do i_fln = 1, fline%num_fline
        if(fline%fln_prm(i_fln)%id_fline_seed_type                      &
     &                       .eq. iflag_position_list) then
          call set_FLINE_seed_field_from_list                           &
     &       (geofem%mesh%node, geofem%mesh%ele, nod_fld,               &
     &        fline%fln_prm(i_fln), fline%fln_src(i_fln),               &
     &        fline%fln_tce(i_fln))
          call dealloc_init_tracer_position(fline%fln_src(i_fln))
        else
          if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
          call s_set_fields_for_fieldline(geofem%mesh, geofem%group,    &
     &        para_surf, nod_fld, fline%fln_prm(i_fln),                 &
     &        fline%fln_src(i_fln), fline%fln_tce(i_fln))
        end if
      end do
      
      call input_tracer_restarts(init_d, rst_step,                      &
     &   fline%num_fline, fline%fln_prm, fline%fln_tce, fline%fline_lc)

      call copy_velocity_as_previous(nod_fld, fline%num_fline,          &
     &                               fline%fln_prm, fline%fln_src)
!
      if(flag_fln_dist) call dealloc_FLINE_element_size(fln_dist)
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
      call copy_velocity_as_previous(nod_fld, fline%num_fline,          &
     &                               fline%fln_prm, fline%fln_src)
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
      do i_fln = 1, fline%num_fline
        call dealloc_local_fline(fline%fline_lc(i_fln))
        call dealloc_iflag_fline_used_ele(fline%fln_prm(i_fln))
        call dealloc_fline_starts_ctl(fline%fln_prm(i_fln))
!
        call dealloc_local_start_grp_item(fline%fln_src(i_fln))
        call dealloc_start_point_fline(fline%fln_src(i_fln))
        call dealloc_num_gl_start_fline(fline%fln_tce(i_fln))
        call dealloc_broadcast_trace_data(fline%fln_bcast(i_fln))
        call dealloc_trace_data_SR_num(fline%fln_SR(i_fln))
      end do
!
      deallocate(fline%fln_src, fline%fline_lc, fline%fln_bcast)
      deallocate(fline%fln_tce, fline%fln_prm, fline%fln_SR)
!
      end subroutine TRACER_finalize
!
!  ---------------------------------------------------------------------
!
      end module t_particle_trace
