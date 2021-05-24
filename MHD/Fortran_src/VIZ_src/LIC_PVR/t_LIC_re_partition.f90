!>@file   t_LIC_re_partition.f90
!!@brief  module t_LIC_re_partition
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for re-partitiong for LIC
!!
!!@verbatim
!!      subroutine LIC_init_shared_mesh(geofem, next_tbl, repart_p,     &
!!     &                                num_lic, lic_param, repart_data)
!!        integer(kind = kint), intent(in) :: num_lic
!!        type(mesh_data), intent(in), target :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_parameters), intent(in) :: lic_param(num_lic)
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!      subroutine LIC_init_each_mesh(geofem, next_tbl, repart_p,       &
!!     &                              lic_param, repart_data)
!!        type(mesh_data), intent(in), target :: geofem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(lic_parameters), intent(inout) :: lic_param
!!
!!      subroutine set_LIC_each_field(geofem, nod_fld,                  &
!!     &          repart_p, lic_param, repart_data, v_sol)
!!      subroutine dealloc_LIC_each_mesh                                &
!!     &         (repart_p, each_part_p, repart_data)
!!        type(mesh_data), intent(in), target :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(lic_parameters), intent(in) :: lic_param
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine s_LIC_re_partition(repart_p, geofem, next_tbl,       &
!!     &                              repart_data)
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(mesh_data), intent(in), target :: geofem
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
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
      use t_phys_data
      use t_next_node_ele_4_node
      use t_lic_field_data
      use t_control_param_LIC
      use t_control_param_vol_grping
      use t_vector_for_solver
!
      implicit  none
!
!>      Structure of repartition data
      type lic_repartioned_mesh
!>         Structure for mesh data for visualization
        type(mesh_data), pointer :: viz_fem
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
!
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: nod_fld_lic
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: field_lic
      end type lic_repartioned_mesh
!
      private :: dealloc_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_shared_mesh(geofem, next_tbl, repart_p,       &
     &                                num_lic, lic_param, repart_data)
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_data), intent(in), target :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_parameters), intent(in) :: lic_param(num_lic)
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
      integer(kind = kint) :: i_lic, nmax_masking
!
!
      nmax_masking = 0
      do i_lic = 1, num_lic
        nmax_masking = max(nmax_masking, lic_param(i_lic)%num_masking)
      end do
!
      allocate(repart_data%nod_fld_lic)
      call alloc_nod_vector_4_lic(geofem%mesh%node, nmax_masking,       &
     &    repart_data%nod_fld_lic)
!
      if(repart_p%flag_repartition) then
!  -----  Repartition
        call s_LIC_re_partition                                         &
     &     (repart_p, geofem, next_tbl, repart_data)
!
        allocate(repart_data%field_lic)
          call alloc_nod_vector_4_lic                                   &
     &       (repart_data%viz_fem%mesh%node, nmax_masking,              &
     &        repart_data%field_lic)
      else
        repart_data%viz_fem => geofem
        repart_data%field_lic => repart_data%nod_fld_lic
      end if
!
      end subroutine LIC_init_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_each_mesh(geofem, next_tbl, repart_p,         &
     &                              lic_param, repart_data)
!
      type(mesh_data), intent(in), target :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(lic_parameters), intent(inout) :: lic_param
!
!
      allocate(repart_data%nod_fld_lic)
      call alloc_nod_vector_4_lic(geofem%mesh%node,                     &
     &    lic_param%num_masking, repart_data%nod_fld_lic)
!
!  -----  Repartition
      if(lic_param%each_part_p%flag_repartition) then
        call s_LIC_re_partition                                         &
     &     (lic_param%each_part_p, geofem, next_tbl, repart_data)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &      lic_param%num_masking, repart_data%field_lic)
      else if(repart_p%flag_repartition) then
        call s_LIC_re_partition(repart_p, geofem, next_tbl,             &
     &                          repart_data)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &      lic_param%num_masking, repart_data%field_lic)
      else
        repart_data%viz_fem =>  geofem
        repart_data%field_lic => repart_data%nod_fld_lic
      end if
!
      end subroutine LIC_init_each_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine set_LIC_each_field(geofem, nod_fld,                    &
     &          repart_p, lic_param, repart_data, v_sol)
!
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_parameters), intent(in) :: lic_param
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param, repart_data%nod_fld_lic)
!
        if(repart_p%flag_repartition                                    &
     &        .or. lic_param%each_part_p%flag_repartition) then
          call repartition_lic_field                                    &
     &       (geofem%mesh%node, repart_data%viz_fem%mesh,               &
     &        repart_data%mesh_to_viz_tbl, repart_data%nod_fld_lic,     &
     &        repart_data%field_lic, v_sol)
        end if
!
      end subroutine set_LIC_each_field
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_each_mesh                                  &
     &         (repart_p, each_part_p, repart_data)
!
      type(volume_partioning_param), intent(in) :: repart_p
      type(volume_partioning_param), intent(in) :: each_part_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
!
      if(repart_p%flag_repartition                                      &
     &        .or. each_part_p%flag_repartition) then
        call dealloc_LIC_re_partition(repart_data)
        call dealloc_nod_data_4_lic(repart_data%field_lic)
        deallocate(repart_data%field_lic)
      else
        nullify(repart_data%viz_fem)
        nullify(repart_data%field_lic)
      end if
!
      call dealloc_nod_data_4_lic(repart_data%nod_fld_lic)
      deallocate(repart_data%nod_fld_lic)
!
      end subroutine dealloc_LIC_each_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_LIC_re_partition(repart_p, geofem, next_tbl,         &
     &                              repart_data)
!
      use t_next_node_ele_4_node
      use t_jacobians
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
      type(sleeve_extension_work), save :: sleeve_exp_WK1
!
      type(shape_finctions_at_points) :: spfs_T
      type(jacobians_type) :: jac_viz
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
      allocate(repart_data%viz_fem)
      call load_or_const_new_partition(repart_p, geofem, next_tbl,      &
     &    repart_data%viz_fem, repart_data%mesh_to_viz_tbl,             &
     &    sleeve_exp_WK1)
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
      subroutine dealloc_LIC_re_partition(repart_data)
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
!
      call dealloc_calypso_comm_table(repart_data%mesh_to_viz_tbl)
!
      call dealloc_vectors_surf_group                                   &
     &   (repart_data%viz_fem%group%surf_grp_norm)
      call dealloc_normal_vector(repart_data%viz_fem%mesh%surf)
      call dealloc_numnod_stack(repart_data%viz_fem%mesh%node)
      call dealloc_mesh_infos_w_normal(repart_data%viz_fem%mesh,        &
     &                                 repart_data%viz_fem%group)
!
      end subroutine dealloc_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_re_partition
