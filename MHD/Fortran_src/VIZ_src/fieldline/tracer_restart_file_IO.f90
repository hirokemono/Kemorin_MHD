!>@file   tracer_restart_file_IO.f90
!!@brief  module tracer_restart_file_IO
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine output_tracer_restart(i_step, tracer_file_prm,       &
!!     &         time_d, rst_step, viz_fields, fln_tce, fline_lc)
!!        integer(kind = kint), intent(in) :: i_step
!!        type(field_IO_params), intent(in) :: tracer_file_prm
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine input_tracer_restart(tracer_file_prm, init_d,        &
!!     &          rst_step,  viz_fields, fln_tce, fline_lc)
!!        type(field_IO_params), intent(in) :: tracer_file_prm
!!        type(time_data), intent(inout) :: init_d
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module tracer_restart_file_IO
!
      use m_precision
      use t_time_data
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_ctl_params_viz_fields
      use t_source_of_filed_line
      use t_local_fline
      use t_read_mesh_data
      use t_field_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_tracer_restart(i_step, tracer_file_prm,         &
     &         time_d, rst_step, viz_fields, fln_tce, fline_lc)
!
      use set_sph_restart_IO
      use field_IO_select
      use local_fline_restart_IO
      use particle_MPI_IO_select
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: tracer_file_prm
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: rst_step
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      type(surf_edge_IO_file) :: particle_IO
      type(field_IO) :: fld_IO
      type(time_data) :: time_IO
      integer(kind = kint) :: istep_rst
!
!
      call copy_time_step_size_data(time_d, time_IO)
      call set_particles_to_trace_list(viz_fields, fln_tce, fline_lc)
      call count_global_num_of_tracer(fline_lc)
      call copy_local_tracer_to_IO(fline_lc, particle_IO)

      call sel_mpi_write_particle_file(tracer_file_prm,                 &
     &                                 time_IO, particle_IO)
      call dealloc_neib_id(particle_IO%comm)
      call dealloc_surf_geometry_data(particle_IO)
      call dealloc_ele_connect(particle_IO%ele)
!
      if(viz_fields%num_color_fields .le. 1) return
!
      istep_rst = set_IO_step(i_step, rst_step)
!
      call field_on_local_tracer_to_IO(viz_fields, fline_lc, fld_IO)

      call sel_write_step_FEM_field_file                                &
     &   (istep_rst, tracer_file_prm, time_IO, fld_IO)
      call dealloc_phys_data_IO(fld_IO)
      call dealloc_phys_name_IO(fld_IO)
!
      end subroutine output_tracer_restart
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_tracer_restart(tracer_file_prm, init_d,          &
     &          rst_step,  viz_fields, fln_tce, fline_lc)
!
      use set_sph_restart_IO
      use field_IO_select
      use local_fline_restart_IO
      use particle_MPI_IO_select
!
      type(field_IO_params), intent(in) :: tracer_file_prm
!
      type(time_data), intent(inout) :: init_d
      type(IO_step_param), intent(inout) :: rst_step
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: istep_rst
      type(surf_edge_IO_file) :: particle_IO
      type(time_data) :: time_IO
      type(field_IO) :: fld_IO
!
!
      call sel_mpi_read_particle_file(tracer_file_prm,                  &
     &                                time_IO, particle_IO)
      call copy_local_tracer_from_IO(particle_IO, fline_lc)
      call dealloc_neib_id(particle_IO%comm)
      call dealloc_ele_connect(particle_IO%ele)
      call dealloc_surf_geometry_data(particle_IO)
!
      call return_particles_to_trace_list(fline_lc, fln_tce)
!
      if(viz_fields%num_color_fields .le. 1) return
!
     istep_rst = set_IO_step(init_d%i_time_step, rst_step)
!
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    istep_rst, tracer_file_prm, time_IO, fld_IO)
      call field_on_local_tracer_from_IO(fld_IO, viz_fields, fline_lc)

      call copy_time_step_data(time_IO, init_d)
      call dealloc_phys_data_IO(fld_IO)
      call dealloc_phys_name_IO(fld_IO)
!
      end subroutine input_tracer_restart
!
! -----------------------------------------------------------------------
!
      end module tracer_restart_file_IO
