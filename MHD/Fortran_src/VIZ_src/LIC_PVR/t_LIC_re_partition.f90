!>@file   t_LIC_re_partition.f90
!!@brief  module t_LIC_re_partition
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for re-partitiong for LIC
!!
!!@verbatim
!!      subroutine LIC_init_nodal_field(geofem, num_lic, lic_param,     &
!!     &                                repart_data)
!!      subroutine dealloc_LIC_nodal_field(repart_data)
!!        type(mesh_data), intent(in), target :: geofem
!!        integer(kind = kint), intent(in) :: num_lic
!!        type(lic_parameters), intent(inout) :: lic_param(num_lic)
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!
!!      subroutine LIC_init_shared_mesh(elps_LIC, geofem, ele_comm,     &
!!     &          next_tbl, repart_p, rep_ref_m, repart_data, m_SR)
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_data), intent(in), target :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repart_reference), intent(in) :: rep_ref_m
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine LIC_init_each_mesh(elps_LIC, geofem, ele_comm,       &
!!     &          next_tbl, repart_p, rep_ref, rep_ref_m,               &
!!     &          lic_param, repart_data, m_SR)
!!        type(mesh_data), intent(in), target :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repart_reference), intent(in) :: rep_ref, rep_ref_m
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(lic_parameters), intent(inout) :: lic_param
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine set_LIC_each_field(elps_LIC, geofem, repart_p,       &
!!     &                              lic_param, repart_data, m_SR)
!!      subroutine dealloc_LIC_each_mesh                                &
!!     &         (repart_p, each_part_p, repart_data)
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_data), intent(in), target :: geofem
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(lic_parameters), intent(in) :: lic_param
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_LIC_re_partition
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
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
      use t_lic_repart_reference
      use t_mesh_SR
!
      implicit  none
!
!>      Structure of repartition data
      type lic_repartioned_mesh
!>         Structure for mesh data for visualization
        type(mesh_data), pointer :: viz_fem
!>         Element communication table for visualization
        type(communication_table) :: viz_ele_comm
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
!>        Transfer table to visualization elements
        type(calypso_comm_table) :: mesh_to_viz_ele_tbl
!
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: nod_fld_lic
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: field_lic
!
!>        Structure of work area for sleeve expansion
        type(sleeve_extension_work) :: sleeve_exp_WK
      end type lic_repartioned_mesh
!
      private :: dealloc_LIC_re_partition
      private :: s_LIC_re_partition
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_nodal_field(geofem, num_lic, lic_param,       &
     &                                repart_data)
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_data), intent(in), target :: geofem
!
      type(lic_parameters), intent(inout) :: lic_param(num_lic)
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
     &                            repart_data%nod_fld_lic)
!
      end subroutine LIC_init_nodal_field
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_nodal_field(repart_data)
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
!
      call dealloc_nod_data_4_lic(repart_data%nod_fld_lic)
      deallocate(repart_data%nod_fld_lic)
!
      end subroutine dealloc_LIC_nodal_field
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_shared_mesh(elps_LIC, geofem, ele_comm,       &
     &          next_tbl, repart_p, rep_ref_m, repart_data, m_SR)
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_data), intent(in), target :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_repart_reference), intent(in) :: rep_ref_m
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(mesh_SR), intent(inout) :: m_SR
!
      type(masking_parameter), allocatable :: masking_tmp(:)
!
!
      if(repart_p%flag_repartition) then
!  -----  Repartition
        allocate(masking_tmp(0))
        call s_LIC_re_partition                                         &
     &     ((.TRUE.), repart_p, elps_LIC, geofem, ele_comm, next_tbl,   &
     &      izero, masking_tmp, rep_ref_m, repart_data, m_SR)
        deallocate(masking_tmp)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &        repart_data%nod_fld_lic%num_mask, repart_data%field_lic)
      else
        repart_data%viz_fem => geofem
        repart_data%field_lic => repart_data%nod_fld_lic
      end if
!
      end subroutine LIC_init_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_each_mesh(elps_LIC, geofem, ele_comm,         &
     &          next_tbl, repart_p, rep_ref, rep_ref_m,                 &
     &          lic_param, repart_data, m_SR)
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_data), intent(in), target :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_repart_reference), intent(in) :: rep_ref, rep_ref_m
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(lic_parameters), intent(inout) :: lic_param
      type(mesh_SR), intent(inout) :: m_SR
!
!  -----  Repartition
      if(lic_param%each_part_p%flag_repartition) then
        call s_LIC_re_partition                                         &
     &     (lic_param%flag_LIC_elapsed_dump, lic_param%each_part_p,     &
     &      elps_LIC, geofem, ele_comm, next_tbl,                       &
     &      lic_param%num_masking, lic_param%masking, rep_ref,          &
     &      repart_data, m_SR)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &      lic_param%num_masking, repart_data%field_lic)
      else if(repart_p%flag_repartition) then
        call s_LIC_re_partition(lic_param%flag_LIC_elapsed_dump,        &
     &      repart_p, elps_LIC, geofem, ele_comm, next_tbl,             &
     &      izero, lic_param%masking, rep_ref_m, repart_data, m_SR)
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
      subroutine set_LIC_each_field(elps_LIC, geofem, repart_p,         &
     &                              lic_param, repart_data, m_SR)
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_data), intent(in), target :: geofem
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_parameters), intent(in) :: lic_param
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &         call start_elapsed_time(elps_LIC%ist_elapsed+7)
        if(repart_p%flag_repartition                                    &
     &        .or. lic_param%each_part_p%flag_repartition) then
          call repartition_lic_field(geofem%mesh,                       &
     &        repart_data%viz_fem%mesh, repart_data%mesh_to_viz_tbl,    &
     &        repart_data%nod_fld_lic, repart_data%field_lic,           &
     &        m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
        end if
      if(elps_LIC%flag_elapsed)                                         &
     &         call end_elapsed_time(elps_LIC%ist_elapsed+7)
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
        deallocate(repart_data%viz_fem)
      else
        nullify(repart_data%field_lic)
        nullify(repart_data%viz_fem)
      end if
!
      end subroutine dealloc_LIC_each_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_LIC_re_partition(flag_lic_dump, repart_p, elps_LIC,  &
     &          geofem, ele_comm, next_tbl, num_mask, masking,          &
     &          rep_ref, repart_data, m_SR)
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
      use int_volume_of_domain
!
      logical, intent(in) :: flag_lic_dump
      integer(kind = kint), intent(in) :: num_mask
      type(elapsed_lables), intent(in) :: elps_LIC
      type(volume_partioning_param), intent(in) :: repart_p
      type(mesh_data), intent(in), target :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(masking_parameter), intent(in) :: masking(num_mask)
      type(lic_repart_reference), intent(in) :: rep_ref
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(mesh_SR), intent(inout) :: m_SR
!
      type(shape_finctions_at_points) :: spfs_T
      type(jacobians_type) :: jac_viz
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &         call start_elapsed_time(elps_LIC%ist_elapsed+6)
      allocate(repart_data%viz_fem)
      call load_or_const_new_partition(flag_lic_dump, repart_p,         &
     &    geofem, ele_comm, next_tbl, num_mask, masking,                &
     &    rep_ref%count_line_int, repart_data%nod_fld_lic%s_lic(1,1),   &
     &    repart_data%nod_fld_lic%v_lic,                                &
     &    repart_data%viz_fem, repart_data%viz_ele_comm,                &
     &    repart_data%mesh_to_viz_tbl, repart_data%mesh_to_viz_ele_tbl, &
     &    repart_data%sleeve_exp_WK, m_SR)
      call dealloc_calypso_comm_table(repart_data%mesh_to_viz_ele_tbl)
      call dealloc_comm_table(repart_data%viz_ele_comm)
      if(elps_LIC%flag_elapsed)                                         &
     &         call end_elapsed_time(elps_LIC%ist_elapsed+6)
!
      if(elps_LIC%flag_elapsed)                                         &
     &         call start_elapsed_time(elps_LIC%ist_elapsed+8)
      if(iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization LIC'
      call FEM_mesh_initialization                                      &
     &   (repart_data%viz_fem%mesh, repart_data%viz_fem%group,          &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      call set_max_integration_points(ione, jac_viz%g_FEM)
      call initialize_FEM_integration                                   &
     &   (jac_viz%g_FEM, spfs_T%spf_3d, spfs_T%spf_2d, spfs_T%spf_1d)
!
      if(iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
      call const_jacobian_and_volume(my_rank, nprocs,                   &
     &    repart_data%viz_fem%mesh, repart_data%viz_fem%group,          &
     &    spfs_T%spf_3d, jac_viz)
      call dealloc_vol_shape_func(spfs_T%spf_3d)
!
!
      if(iflag_debug.eq.1) write(*,*) 'surf_jacobian_sf_grp_normal'
      call surf_jacobian_sf_grp_normal(my_rank, nprocs,                 &
     &    repart_data%viz_fem%mesh, repart_data%viz_fem%group,          &
     &    spfs_T, jac_viz)
      if(elps_LIC%flag_elapsed)                                         &
     &         call end_elapsed_time(elps_LIC%ist_elapsed+8)
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
