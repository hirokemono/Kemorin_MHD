!>@file   t_LIC_re_partition.f90
!!@brief  module t_LIC_re_partition
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
      module t_LIC_re_partition
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
!>      Structure of repartition data
      type lic_repartioned_mesh
!>         Structure for mesh data for visualization
        type(mesh_data), pointer :: viz_fem
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
      end type lic_repartioned_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_LIC_re_partition(repart_p, geofem, next_tbl,         &
     &                              repart_data)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
!
      use field_to_new_partition
      use parallel_FEM_mesh_init
      use set_normal_vectors
      use const_element_comm_tables
      use const_jacobians_3d
!
      type(volume_partioning_param), intent(in) :: repart_p
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(mesh_data), intent(in), target :: geofem
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
      type(shape_finctions_at_points) :: spfs_T
      type(jacobians_type) :: jac_viz
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
      call load_or_const_new_partition(repart_p, geofem, next_tbl,      &
     &    repart_data%viz_fem, repart_data%mesh_to_viz_tbl)
!
      if(iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization LIC'
      call FEM_mesh_initialization                                      &
     &   (repart_data%viz_fem%mesh, repart_data%viz_fem%group)
      call set_max_integration_points(ione, jac_viz%g_FEM)
      call initialize_FEM_integration                                   &
     &   (jac_viz%g_FEM, spfs_T%spf_3d, spfs_T%spf_2d, spfs_T%spf_1d)
      if(iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
      call surf_jacobian_sf_grp_normal(my_rank, nprocs,                 &
     &    repart_data%viz_fem%mesh, repart_data%viz_fem%group,          &
     &    spfs_T, jac_viz)
!
      end subroutine s_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_re_partition                               &
     &         (repart_p, repart_data, lic_fld_pm)
!
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(LIC_field_params), intent(inout) :: lic_fld_pm
!
!
      if(lic_fld_pm%lic_param%each_part_p%flag_repartition) then
        call dealloc_calypso_comm_table(repart_data%mesh_to_viz_tbl)
!
        call dealloc_vectors_surf_group                                 &
     &     (repart_data%viz_fem%group%surf_grp_norm)
        call dealloc_normal_vector(repart_data%viz_fem%mesh%surf)
        call dealloc_numnod_stack(repart_data%viz_fem%mesh%node)
        call dealloc_mesh_infos_w_normal(repart_data%viz_fem%mesh,      &
     &                                   repart_data%viz_fem%group)
      else if(repart_p%flag_repartition) then
        call dealloc_calypso_comm_table(repart_data%mesh_to_viz_tbl)
!
        call dealloc_vectors_surf_group                                 &
     &     (repart_data%viz_fem%group%surf_grp_norm)
        call dealloc_normal_vector(repart_data%viz_fem%mesh%surf)
        call dealloc_numnod_stack(repart_data%viz_fem%mesh%node)
        call dealloc_mesh_infos_w_normal(repart_data%viz_fem%mesh,      &
     &                                   repart_data%viz_fem%group)
      else
        nullify(repart_data%viz_fem)
        nullify(lic_fld_pm%field_lic)
      end if
!
      call dealloc_nod_data_4_lic(lic_fld_pm%nod_fld_lic)
      deallocate(lic_fld_pm%nod_fld_lic)
!
      end subroutine dealloc_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_re_partition
