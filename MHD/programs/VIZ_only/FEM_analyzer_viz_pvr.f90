!>@file   FEM_analyzer_viz_pvr.f90
!!@brief  module FEM_analyzer_viz_pvr
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!      subroutine set_control_params_4_pvr                             &
!!     &         (pvr_vizs_c, pvr, t_viz_param, ierr)
!!        type(control_data_pvr_vizs), intent(in) :: pvr_vizs_c
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!      subroutine FEM_initialize_pvr(init_d, ucd_step, viz_step,       &
!!     &                              FEM_viz, pvr)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
!!        type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
!!      subroutine FEM_analyze_pvr(istep, ucd_step, time_d,             &
!!     &                           FEM_viz, pvr)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
!!@endverbatim
!
      module FEM_analyzer_viz_pvr
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
!
      implicit none
!
!
!>      Structure of mesh and field for visualization only
      type FEM_mesh_field_4_pvr
!>        Structure for mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure for field file IO paramters
        type(field_IO_params) :: ucd_file_IO
!
!>          Instance of time data from data input
        type(time_data) :: ucd_time
!>          Instance of FEM field data IO
        type(ucd_data) :: ucd
!
!>        Structure of included element list for each node
        type(element_around_node) :: ele_4_nod
!>        Structure of shape function for PVR and fieldline
        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type) :: jacobians
!
!>      structure of field list for visualization
        type(visulize_field_list) :: viz_fld_list
      end type FEM_mesh_field_4_pvr
!
      private :: add_field_in_viz_ctls_w_SGS, element_normals_4_pvr
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_params_4_pvr                               &
     &         (pvr_vizs_c, pvr, t_viz_param, ierr)
!
      use t_control_data_vizs_pvr
      use t_VIZ_only_step_parameter
!
      use m_file_format_switch
      use m_default_file_prefix
      use set_control_platform_item
      use set_control_platform_data
      use ucd_IO_select
!
      type(control_data_pvr_vizs), intent(in) :: pvr_vizs_c
!
      type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, pvr_vizs_c%viz_plt)
      call set_control_smp_def(my_rank, pvr_vizs_c%viz_plt)
      call set_control_mesh_def(pvr_vizs_c%viz_plt, pvr%mesh_file_IO)
      call set_ucd_file_define(pvr_vizs_c%viz_plt, pvr%ucd_file_IO)
!
      call init_viz_field_list_control(pvr_vizs_c%viz_field_ctl,        &
     &                                 pvr%viz_fld_list)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (pvr_vizs_c%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
!
      end subroutine set_control_params_4_pvr
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_pvr(init_d, ucd_step, viz_step,         &
     &                              FEM_viz, pvr)
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
!
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
!
      integer(kind = kint) :: istep_ucd, iflag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mpi_input_mesh(pvr%mesh_file_IO, nprocs, FEM_viz%geofem)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(FEM_viz%geofem%mesh, FEM_viz%v_sol)
      call FEM_mesh_initialization(FEM_viz%geofem%mesh,                 &
     &                             FEM_viz%geofem%group)
!
!     ---------------------
!
      pvr%ucd%nnod = FEM_viz%geofem%mesh%node%numnod
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_udt_param(my_rank, istep_ucd, pvr%ucd_file_IO,      &
     &                        pvr%ucd_time, pvr%ucd)
      call alloc_phys_name_type_by_output(pvr%ucd, FEM_viz%field)
!
      call add_field_in_viz_ctls_w_SGS(pvr%viz_fld_list, FEM_viz%field)
      call dealloc_field_lists_for_vizs(pvr%viz_fld_list)
!
      call alloc_phys_data(FEM_viz%geofem%mesh%node%numnod,             &
     &                     FEM_viz%field)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment
      if(iflag .gt. 0) then
        call element_normals_4_pvr                                      &
     &     (FEM_viz%geofem, pvr%ele_4_nod, pvr%spfs, pvr%jacobians)
      end if
!
!     ---------------------
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_pvr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(istep, ucd_step, time_d,               &
     &                           FEM_viz, pvr)
!
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: istep
      type(IO_step_param), intent(in) :: ucd_step
!
      type(time_data), intent(inout) :: time_d
      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_viz
      type(FEM_mesh_field_4_pvr), intent(inout) :: pvr
!
      integer(kind = kint) :: istep_ucd
!
!
      istep_ucd = IO_step_exc_zero_inc(istep, ucd_step)
      call set_data_by_read_ucd(istep_ucd, pvr%ucd_file_IO,             &
     &    pvr%ucd_time, pvr%ucd, FEM_viz%field)
      call copy_time_step_size_data(pvr%ucd_time, time_d)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv                                         &
     &   (FEM_viz%geofem%mesh, FEM_viz%field, FEM_viz%v_sol)
!
      end subroutine FEM_analyze_pvr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine element_normals_4_pvr                                  &
     &         (geofem, ele_4_nod, spfs, jacobians)
!
      use int_volume_of_domain
      use set_table_4_RHS_assemble
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
!
      type(mesh_data), intent(inout) :: geofem
      type(element_around_node), intent(inout) :: ele_4_nod
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacobians
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_element_on_node_in_mesh'
      call set_element_on_node_in_mesh(geofem%mesh, ele_4_nod)
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (geofem%mesh%ele%nnod_4_ele, jacobians%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    geofem%mesh, geofem%group, spfs, jacobians)
!
      end subroutine element_normals_4_pvr
!
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
      end module FEM_analyzer_viz_pvr
