!>@file   FEM_analyzer_four_vizs.f90
!!@brief  module FEM_analyzer_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_ctl_params_four_vizs                             &
!!     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!!        type(control_data_four_vizs), intent(in) :: pvr_vizs_c
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!      subroutine FEM_initialize_four_vizs(init_d, ucd_step, viz_step, &
!!     &          FEM_viz, pvr, v_sol, SR_sig, SR_r, SR_i, SR_il)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(VIZ_mesh_field), intent(inout) :: pvr
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine FEM_analyze_four_vizs                                &
!!     &         (istep, ucd_step, time_d, FEM_viz, v_sol, SR_sig, SR_r)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module FEM_analyzer_four_vizs
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_time_data
      use t_FEM_mesh_field_4_viz
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_file_IO_parameter
      use t_field_list_for_vizs
      use t_VIZ_step_parameter
      use t_vector_for_solver
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
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
      subroutine set_ctl_params_four_vizs                               &
     &         (pvr_vizs_c, FEM_viz, t_viz_param, ierr)
!
      use t_control_data_four_vizs
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use parallel_ucd_IO_select
!
      type(control_data_four_vizs), intent(in) :: pvr_vizs_c
!
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, pvr_vizs_c%viz_plt)
      call set_control_smp_def(my_rank, pvr_vizs_c%viz_plt)
      call set_control_mesh_def(pvr_vizs_c%viz_plt,                     &
     &                          FEM_viz%mesh_file_IO)
      call set_merged_ucd_file_define(pvr_vizs_c%viz_plt,               &
     &                                FEM_viz%ucd_file_IO)
!
      call init_viz_field_list_control(pvr_vizs_c%viz_field_ctl,        &
     &                                 FEM_viz%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (pvr_vizs_c%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_ctl_params_four_vizs
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_four_vizs(init_d, ucd_step, viz_step,   &
     &          FEM_viz, pvr, v_sol, SR_sig, SR_r, SR_i, SR_il)
!
      use t_VIZ_mesh_field
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use FEM_to_VIZ_bridge
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
!
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(VIZ_mesh_field), intent(inout) :: pvr
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: istep_ucd, iflag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mpi_input_mesh(FEM_viz%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_viz%geofem%mesh, v_sol,          &
     &                             SR_sig, SR_r, SR_i, SR_il)
!
!     ---------------------
!
      FEM_viz%ucd_in%nnod = FEM_viz%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_parallel_udt_param(istep_ucd,                       &
     &    FEM_viz%ucd_file_IO, FEM_viz%ucd_time, FEM_viz%ucd_in)
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
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if(iflag_debug.gt.0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(viz_step, FEM_viz%geofem, pvr,        &
     &                            SR_sig, SR_r, SR_i, SR_il)
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_four_vizs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_four_vizs                                  &
     &         (istep, ucd_step, time_d, FEM_viz, v_sol, SR_sig, SR_r)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: ucd_step
!
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
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
     &                          v_sol, SR_sig, SR_r)
!
      end subroutine FEM_analyze_four_vizs
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
      end module FEM_analyzer_four_vizs
