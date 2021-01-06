!>@file   FEM_analyzer_viz_surf.f90
!!@brief  module FEM_analyzer_viz_surf
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: time_d
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_surfacing
      use t_time_data
      use t_VIZ_step_parameter
      use t_IO_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_surface(ucd_step, init_d, sfcing)
!
      use t_field_list_for_vizs
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
      call mpi_input_mesh(sfcing%mesh_file_IO, nprocs, sfcing%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(sfcing%geofem%mesh,                  &
     &                             sfcing%geofem%group)
!
!     ---------------------
!
      sfcing%ucd_in%nnod = sfcing%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_udt_param(my_rank, istep_ucd, sfcing%ucd_file_IO,   &
     &                        sfcing%ucd_time, sfcing%ucd_in)
      call alloc_phys_name_type_by_output(sfcing%ucd_in,                &
     &                                    sfcing%nod_fld)
      call add_field_in_viz_controls(sfcing%viz_fld_list,               &
     &                               sfcing%nod_fld)
      call alloc_phys_data_type(sfcing%geofem%mesh%node%numnod,         &
     &                          sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, ucd_step, time_d, sfcing)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call set_data_by_read_ucd(istep_ucd, sfcing%ucd_file_IO,          &
     &    sfcing%ucd_time, sfcing%ucd_in, sfcing%nod_fld)
      call copy_time_step_size_data(sfcing%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(sfcing%geofem%mesh, sfcing%nod_fld)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
