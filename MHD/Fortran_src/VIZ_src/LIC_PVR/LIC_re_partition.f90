!>@file   LIC_re_partition.f90
!!@brief  module LIC_re_partition
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access all visualization programs
!!
!!@verbatim
!!      subroutine s_LIC_re_partition(repart_p, geofem, next_tbl,       &
!!     &                              viz_fem, mesh_to_viz_tbl)
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(mesh_data), intent(in), target :: geofem
!!        type(mesh_data), intent(inout) :: viz_fem
!!        type(calypso_comm_table), intent(inout) :: mesh_to_viz_tbl
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
      use t_mesh_data
      use t_comm_table
      use t_calypso_comm_table
      use t_next_node_ele_4_node
      use t_jacobians
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_LIC_re_partition(repart_p, geofem, next_tbl,         &
     &                              viz_fem, mesh_to_viz_tbl)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
!
      use field_to_new_partition
      use parallel_FEM_mesh_init
      use set_normal_vectors
      use const_element_comm_tables
!
      type(volume_partioning_param), intent(in) :: repart_p
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(mesh_data), intent(in), target :: geofem
!
      type(mesh_data), intent(inout) :: viz_fem
      type(calypso_comm_table), intent(inout) :: mesh_to_viz_tbl
!
      type(shape_finctions_at_points) :: spfs_T
      type(jacobians_type) :: jac_viz
!
!
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
        call load_or_const_new_partition(repart_p, geofem, next_tbl,    &
     &      viz_fem, mesh_to_viz_tbl)
!
        if(iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization LIC'
        call FEM_mesh_initialization(viz_fem%mesh, viz_fem%group)
        if(iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
        call set_max_integration_points(ione, jac_viz%g_FEM)
        call surf_jacobian_sf_grp_normal(my_rank, nprocs,               &
     &      viz_fem%mesh, viz_fem%group, spfs_T, jac_viz)
!
      end subroutine s_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      end module LIC_re_partition
