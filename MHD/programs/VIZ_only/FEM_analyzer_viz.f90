!>@file   FEM_analyzer_viz.f90
!!@brief  module FEM_analyzer_viz
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine FEM_initialize_viz(init_d, ucd_step, FEM_viz, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine FEM_analyze_viz(istep, ucd_step,                     &
!!     &                           time_d, FEM_viz, m_SR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module FEM_analyzer_viz
!
      use m_precision
      use m_machine_parameter
      use m_elapsed_labels_4_REPART
      use m_work_time
      use calypso_mpi
!
      use t_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_ucd_data
      use t_mesh_SR
!
      implicit none
!
      private :: add_field_in_viz_ctls_w_SGS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz(init_d, ucd_step, FEM_viz, m_SR)
!
      use calypso_mpi_logical
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use field_to_new_partition
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      call FEM_comm_initialization(FEM_viz%geofem%mesh, m_SR)
!
!   --------------------------------
!       setup field information
!   --------------------------------
!
      FEM_viz%ucd_in%nnod = FEM_viz%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_parallel_udt_param(istep_ucd,                       &
     &    FEM_viz%ucd_file_IO, FEM_viz%ucd_time, FEM_viz%ucd_in)
!
      call alloc_phys_name_type_by_output(FEM_viz%ucd_in,               &
     &                                    FEM_viz%field)
!
      call add_field_in_viz_ctls_w_SGS(FEM_viz%viz_fld_list,            &
     &                                 FEM_viz%field)
      call dealloc_field_lists_for_vizs(FEM_viz%viz_fld_list)
!
      call alloc_phys_data(FEM_viz%geofem%mesh%node%numnod,             &
     &                     FEM_viz%field)
!
      end subroutine FEM_initialize_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_viz(istep, ucd_step,                       &
     &                           time_d, FEM_viz, m_SR)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
      use field_to_new_partition
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: ucd_step
!
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(istep, ucd_step)
      call set_data_by_read_ucd(istep_ucd, FEM_viz%ucd_file_IO,         &
     &    FEM_viz%ucd_time, FEM_viz%ucd_in, FEM_viz%field)
      call copy_time_step_size_data(FEM_viz%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(FEM_viz%geofem%mesh, FEM_viz%field,     &
     &                          m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine FEM_analyze_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_field_in_viz_ctls_w_SGS(viz_fld_list, phys_nod)
!
      use t_phys_data
      use set_each_field_name_w_SGS
!
      type(visulize_field_list), intent(in) :: viz_fld_list
      type(phys_data), intent(inout) :: phys_nod
!
      integer(kind = kint) :: i_fld, j_fld
      logical :: flag
!
!
      do i_fld = 1, viz_fld_list%num_field
        flag = .FALSE.
        do j_fld = 1, phys_nod%num_phys
          if(viz_fld_list%field_name(i_fld)                             &
     &       .eq. phys_nod%phys_name(j_fld)) then
            flag = .TRUE.
            exit
          end if
        end do
!
        if(flag) cycle
        call set_vector_field_name_w_SGS                                &
     &     (viz_fld_list%field_name(i_fld), (.TRUE.), (.TRUE.),         &
     &      phys_nod, flag)
        if(flag) cycle
        call set_scalar_field_name_w_SGS                                &
     &     (viz_fld_list%field_name(i_fld), (.TRUE.), (.TRUE.),         &
     &      phys_nod, flag)
        if(flag) cycle
        call set_tensor_field_name_w_SGS                                &
     &     (viz_fld_list%field_name(i_fld), (.TRUE.), (.TRUE.),         &
     &      phys_nod, flag)
      end do
!
      end subroutine add_field_in_viz_ctls_w_SGS
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
