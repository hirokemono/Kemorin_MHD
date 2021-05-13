!>@file   LIC_re_partition.f90
!!@brief  module LIC_re_partition
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access all visualization programs
!!
!!@verbatim
!!      subroutine init_visualize(viz_step, geofem, nod_fld, VIZ_DAT,   &
!!     &                          viz_ctls, vizs)
!!      subroutine visualize_all(viz_step, time_d, geofem, nod_fld,     &
!!     &                         VIZ_DAT, vizs, v_sol)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(visualize_modules), intent(inout) :: vizs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module LIC_re_partition
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_VIZ
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_VIZ_mesh_field
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobians
!
      use t_control_data_vizs
      use t_lic_rendering
      use t_vector_for_solver
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_LIC_re_partition(viz_step, geofem, VIZ_DAT)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobians
!
      use field_to_new_partition
      use parallel_FEM_mesh_init
      use set_normal_vectors
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in), target :: geofem
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      type(shape_finctions_at_points) :: spfs_T
      type(jacobians_type) :: jac_viz
!
!
      if((viz_step%LIC_t%increment .gt. 0)                              &
     &           .and. VIZ_DAT%repart_p%flag_repartition) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
        allocate(VIZ_DAT%viz_fem)
        call load_or_const_new_partition                                &
     &     (VIZ_DAT%repart_p, geofem, VIZ_DAT%next_tbl,                 &
     &      VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl)
!
        if(iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization LIC'
        call FEM_mesh_initialization(VIZ_DAT%viz_fem%mesh,              &
     &                               VIZ_DAT%viz_fem%group)
        if(iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call set_max_integration_points(ione, jac_viz%g_FEM)
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      VIZ_DAT%viz_fem%mesh, VIZ_DAT%viz_fem%group,                &
     &      spfs_T, jac_viz)
      else
        VIZ_DAT%viz_fem => geofem
      end if
!
      end subroutine s_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      end module LIC_re_partition
