!>@file   FEM_analyzer_viz.f90
!!@brief  module FEM_analyzer_viz
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_control_params_4_viz                             &
!!     &         (vizs_ctl, FEM_viz, VIZ_DAT, t_viz_param, ierr)
!!        type(control_data_vizs), intent(in) :: vizs_ctl
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!      subroutine FEM_initialize_viz                                   &
!!     &         (init_d, ucd_step, viz_step, FEM_viz)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!      subroutine FEM_analyze_viz(istep, ucd_step, time_d, FEM_viz)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
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
      use t_VIZ_mesh_field
      use t_ucd_data
      use t_VIZ_step_parameter
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
      subroutine set_control_params_4_viz                               &
     &         (vizs_ctl, FEM_viz, VIZ_DAT, t_viz_param, ierr)
!
      use t_control_data_all_vizs
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_vizs), intent(in) :: vizs_ctl
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, vizs_ctl%viz_plt)
      call set_control_smp_def(my_rank, vizs_ctl%viz_plt)
      call set_control_mesh_def(vizs_ctl%viz_plt, FEM_viz%mesh_file_IO)
      call set_merged_ucd_file_define(vizs_ctl%viz_plt,                 &
     &                                FEM_viz%ucd_file_IO)
!
      call init_viz_field_list_control(vizs_ctl%viz_field_ctl,          &
     &                                 FEM_viz%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (vizs_ctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      call set_ctl_param_vol_repart(vizs_ctl%repart_ctl,                &
     &                              VIZ_DAT%repart_p)
!
      end subroutine set_control_params_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz                                     &
     &         (init_d, ucd_step, viz_step, FEM_viz)
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
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!
      integer(kind = kint) :: istep_ucd, iflag
      logical :: flag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      call FEM_comm_initialization(FEM_viz%geofem%mesh, FEM_viz%v_sol)
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
      subroutine FEM_analyze_viz(istep, ucd_step, time_d, FEM_viz)
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
      call nod_fields_send_recv                                         &
     &   (FEM_viz%geofem%mesh, FEM_viz%field, FEM_viz%v_sol)
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
