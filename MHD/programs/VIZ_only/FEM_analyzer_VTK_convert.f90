!>@file   FEM_analyzer_VTK_convert.f90
!!@brief  module FEM_analyzer_VTK_convert
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief FEM top routines for surfacing
!!
!!@verbatim
!!      subroutine FEM_initialize_VTK_convert(ucd_step, init_d, sfcing)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!      subroutine FEM_analyze_VTK_convert                              &
!!     &         (i_step, ucd_step, time_d, sfcing)
!!        integer (kind =kint), intent(in) :: i_step
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(IO_step_param), intent(inout) :: ucd_step
!!        type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!!@endverbatim
!
      module FEM_analyzer_VTK_convert
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
      subroutine FEM_initialize_VTK_convert(ucd_step, init_d, sfcing)
!
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
      call alloc_phys_data_type(sfcing%geofem%mesh%node%numnod,         &
     &                          sfcing%nod_fld)
      call deallocate_surface_geom_type(sfcing%geofem%mesh%surf)
!
      end subroutine FEM_initialize_VTK_convert
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_VTK_convert                                &
     &         (i_step, ucd_step, time_d, sfcing)
!
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(inout) :: ucd_step
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_4_surfacing), intent(inout) :: sfcing
!
!
      call set_field_data_4_VIZ                                         &
     &   (i_step, ucd_step, sfcing%ucd_file_IO, sfcing%geofem,          &
     &    sfcing%ucd_time, sfcing%ucd_in, time_d, sfcing%nod_fld)
!
      end subroutine FEM_analyze_VTK_convert
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_VTK_convert
