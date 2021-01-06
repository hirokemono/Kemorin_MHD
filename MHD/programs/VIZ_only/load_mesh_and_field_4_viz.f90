!>@file   load_mesh_and_field_4_viz.f90
!!@brief  module load_mesh_and_field_4_viz
!!
!!@author H. Matsui
!!@date Programmed in July, 2020
!
!>@brief Load FEM data loading for visualize program
!!
!!@verbatim
!!      subroutine set_control_params_4_viz(tctl, plt, viz_field_ctl,   &
!!     &          mesh_file, ucd_param, viz_fld_list, t_viz_param, ierr)
!!        type(time_data_control), intent(in) :: tctl
!!        type(platform_data_control), intent(in) :: plt
!!        type(ctl_array_c3), intent(in) :: viz_field_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(field_IO_params), intent(inout) :: ucd_param
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!
!!      subroutine mesh_setup_4_VIZ(init_d, ucd_step, viz_fld_list,     &
!!     &          ucd_param, mesh_file, geofem, t_IO, ucd, field)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(mesh_data), intent(inout) :: geofem
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!        type(phys_data), intent(inout) :: field
!!      subroutine set_field_data_4_VIZ(istep, ucd_step, ucd_param,     &
!!     &          geofem, t_IO, ucd, time_d, field)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(inout) :: time_d
!!@endverbatim
!
      module load_mesh_and_field_4_viz
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_ucd_data
      use t_IO_step_parameter
      use t_file_IO_parameter
      use t_VIZ_only_step_parameter
      use t_field_list_for_vizs
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
      subroutine set_control_params_4_viz(tctl, plt, viz_field_ctl,     &
     &          mesh_file, ucd_param, viz_fld_list, t_viz_param, ierr)
!
      use t_ucd_data
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_array_character3
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use ucd_IO_select
!
      type(time_data_control), intent(in) :: tctl
      type(platform_data_control), intent(in) :: plt
      type(ctl_array_c3), intent(in) :: viz_field_ctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      type(visulize_field_list), intent(inout) :: viz_fld_list
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_ucd_file_define(plt, ucd_param)
!
      call set_viz_field_list_control(viz_field_ctl, viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (tctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_control_params_4_viz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ(init_d, ucd_step, viz_fld_list,       &
     &          ucd_param, mesh_file, geofem, t_IO, ucd, field)
!
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(field_IO_params), intent(in) :: mesh_file
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: init_d
      type(visulize_field_list), intent(in) :: viz_fld_list
!
      type(mesh_data), intent(inout) :: geofem
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: field
!
      integer(kind = kint) :: istep_ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
      call mpi_input_mesh(mesh_file, nprocs, geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!
!     ---------------------
!
      ucd%nnod = geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_udt_param(my_rank, istep_ucd, ucd_param, t_IO, ucd)
      call alloc_phys_name_type_by_output(ucd, field)
      call add_field_in_viz_ctls_w_SGS(viz_fld_list, field)
      call alloc_phys_data_type(geofem%mesh%node%numnod, field)
!
      end subroutine mesh_setup_4_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ(istep, ucd_step, ucd_param,       &
     &          geofem, t_IO, ucd, time_d, field)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: ucd_param
      type(mesh_data), intent(in) :: geofem
!
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(time_data), intent(inout) :: time_d
      type(phys_data), intent(inout) :: field
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(istep, ucd_step)
      call set_data_by_read_ucd                                         &
     &   (istep_ucd, ucd_param, t_IO, ucd, field)
      call copy_time_step_size_data(t_IO, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(geofem%mesh, field)
!
      end subroutine set_field_data_4_VIZ
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
      end module load_mesh_and_field_4_viz
