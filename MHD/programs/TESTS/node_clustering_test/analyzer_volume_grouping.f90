!>@file   analyzer_volume_grouping.f90
!!@brief  module analyzer_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_volume_grouping
!!      subroutine analyze_volume_grouping
!!@endverbatim
!
      module analyzer_volume_grouping
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_volume_grouping
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_volume_grouping
      use t_control_param_vol_grping
      use t_1d_repartitioning_work
      use set_istack_4_domain_block
      use repart_in_xyz_by_volume
      use set_repartition_group_name
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
!
      use mpi_load_mesh_data
      use mesh_file_IO
      use copy_mesh_structures
      use append_group_data
!
      use calypso_mpi_int
      use calypso_mpi_real
      use const_jacobians_3d
      use parallel_FEM_mesh_init
      use output_test_mesh
      use set_table_4_RHS_assemble
      use nod_phys_send_recv
      use solver_SR_type
      use transfer_to_long_integers
!
      use set_parallel_file_name
      use int_volume_of_single_domain
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type node_volume_and_sorting
        integer(kind = kint) :: numnod
        integer(kind = kint), allocatable :: id_block(:,:)
        real(kind = kreal), allocatable :: node_volume(:)
        real(kind = kreal) :: nod_vol_tot
        real(kind = kreal) :: sub_volume
!
        integer(kind = kint), allocatable :: inod_sort(:)
      end type node_volume_and_sorting
!
      type(node_volume_and_sorting) :: vol_sort
!
      type(grouping_1d_work) :: sub_z
      type(grouping_1d_work) :: sub_y
      type(grouping_1d_work) :: sub_x
!
      integer(kind = kint) :: ndomain_yz
!
      real(kind = kreal) :: vol_ref
      integer(kind = kint) :: i
!
      type(group_data) :: z_part_grp
      type(group_data) :: yz_part_grp
!
      type(group_data) :: part_grp
      type(group_data) :: grp_tmp
!
      call init_elapse_time_by_TOTAL
!      call elapsed_label_4_ele_comm_tbl
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call s_set_ctl_params_4_test_mesh(part_tctl1, T_meshes)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_meshes%mesh_file_IO, nprocs, fem_T)
!
!  -------------------------------
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs_T, jacobians_T)
!
      vol_ref = fem_T%mesh%ele%volume
      call calypso_mpi_allreduce_one_real                               &
     &   (vol_ref, fem_T%mesh%ele%volume, MPI_SUM)
      if (fem_T%mesh%ele%volume .eq. 0.0d0) then
        fem_T%mesh%ele%a_vol = 1.0d30
      else
        fem_T%mesh%ele%a_vol = 1.0d0 / fem_T%mesh%ele%volume
      end if
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
      allocate(vol_sort%node_volume(fem_T%mesh%node%numnod))
      allocate(vol_sort%id_block(fem_T%mesh%node%numnod,3))
      allocate(vol_sort%inod_sort(fem_T%mesh%node%numnod))
!
      call set_xyz_block_by_nod_volume                                  &
     &   (fem_T%mesh, T_meshes, vol_sort%node_volume, vol_sort%nod_vol_tot, vol_sort%id_block)
!
      call const_single_domain_list(sub_z)
!
!    For z direction
!
      vol_sort%sub_volume                                               &
     &          = vol_sort%nod_vol_tot / dble(T_meshes%ndomain_eb(3))
      call const_istack_z_domain_block(fem_T%mesh, T_meshes,            &
     &    vol_sort%id_block, vol_sort%node_volume, vol_sort%nod_vol_tot, vol_sort%sub_volume, vol_sort%inod_sort, sub_z)
      call const_z_div_domain_group_data                                &
     &   (fem_T%mesh, T_meshes, sub_z, vol_sort%inod_sort, z_part_grp)
!
      call const_z_subdomain_list(T_meshes, z_part_grp, sub_y)
      call dealloc_grouping_1d_work(sub_z)
!
!   For y direction
!
      ndomain_yz = T_meshes%ndomain_eb(2) * T_meshes%ndomain_eb(3)
      vol_sort%sub_volume = vol_sort%nod_vol_tot / dble(ndomain_yz)
      call const_istack_xyz_domain_block                                &
     &   (itwo, fem_T%mesh, T_meshes, z_part_grp,                       &
     &    vol_sort%id_block, vol_sort%node_volume, vol_sort%nod_vol_tot, vol_sort%sub_volume, vol_sort%inod_sort,    &
     &    sub_y)
!
      call const_newdomain_group_data(itwo, ndomain_yz, fem_T%mesh,     &
     &    T_meshes, z_part_grp, sub_y, vol_sort%inod_sort, yz_part_grp)
!
      call const_yz_subdomain_list(T_meshes, sub_y, yz_part_grp, sub_x)
      call dealloc_grouping_1d_work(sub_y)
      call dealloc_group(z_part_grp)
!
!   For x direction
!
      vol_sort%sub_volume = vol_sort%nod_vol_tot / dble(T_meshes%new_nprocs)
      call const_istack_xyz_domain_block                                &
     &   (ione, fem_T%mesh, T_meshes, yz_part_grp,                      &
     &    vol_sort%id_block, vol_sort%node_volume, vol_sort%nod_vol_tot, vol_sort%sub_volume, vol_sort%inod_sort,    &
     &    sub_x)
!
      call const_newdomain_group_data(ione, T_meshes%new_nprocs,        &
     &    fem_T%mesh, T_meshes, yz_part_grp, sub_x, vol_sort%inod_sort, part_grp)
      call dealloc_grouping_1d_work(sub_x)
      call dealloc_group(yz_part_grp)
!
      deallocate(vol_sort%node_volume)
      deallocate(vol_sort%id_block)
      deallocate(vol_sort%inod_sort)
!
!       Append group data
      call s_append_group_data(part_grp, fem_T%group%nod_grp)
!
!       Output appended mesh
      call mpi_output_mesh(T_meshes%new_mesh_file_IO,                   &
     &    fem_T%mesh, fem_T%group)
!
      end subroutine initialize_volume_grouping
!
! ----------------------------------------------------------------------
!
      subroutine analyze_volume_grouping
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_volume_grouping
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_single_domain_list(sub_z)
!
      use t_1d_repartitioning_work
!
      type(grouping_1d_work), intent(inout) :: sub_z
!
!
      sub_z%ndomain_done = 1
      call alloc_new_domain_list(sub_z)
      sub_z%idomain_done = 1
!
      end subroutine const_single_domain_list
!
! ----------------------------------------------------------------------
!
      subroutine const_z_subdomain_list(part_param, z_part_grp, sub_y)
!
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
      use set_istack_4_domain_block
!
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: z_part_grp
!
      type(grouping_1d_work), intent(inout) :: sub_y
!
!
      sub_y%ndomain_done = count_z_subdomain_num(part_param,            &
     &                       z_part_grp%num_grp, z_part_grp%istack_grp)
      call alloc_new_domain_list(sub_y)
!
      call set_z_subdomain_list                                         &
     &   (part_param, z_part_grp%num_grp, z_part_grp%istack_grp,        &
     &    sub_y%ndomain_done, sub_y%idomain_done)
!
      end subroutine const_z_subdomain_list
!
! ----------------------------------------------------------------------
!
      subroutine const_yz_subdomain_list                                &
     &         (part_param, sub_y, yz_part_grp, sub_x)
!
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
      use set_istack_4_domain_block
!
      type(mesh_test_files_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_y
      type(group_data), intent(in) :: yz_part_grp
!
      type(grouping_1d_work), intent(inout) :: sub_x
!
!
      sub_x%ndomain_done = count_yz_subdomain_num(part_param,           &
     &                    sub_y%ndomain_done, sub_y%idomain_done,       &
     &                    yz_part_grp%num_grp, yz_part_grp%istack_grp)
      call alloc_new_domain_list(sub_x)
!
      call set_yz_subdomain_list                                        &
     &   (part_param, sub_y%ndomain_done, sub_y%idomain_done,           &
     &    yz_part_grp%num_grp, yz_part_grp%istack_grp,                  &
     &    sub_x%ndomain_done, sub_x%idomain_done)
!
      end subroutine const_yz_subdomain_list
!
! ----------------------------------------------------------------------
!
      subroutine const_istack_z_domain_block(mesh, part_param,          &
     &          id_block, node_volume, nod_vol_tot, sub_volume, inod_sort, sub_z)
!
      use t_mesh_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
!
      use repart_in_xyz_by_volume
      use set_istack_4_domain_block
!
      implicit none
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: id_block(mesh%node%numnod,3)
      real(kind = kreal), intent(in) :: node_volume(mesh%node%numnod)
      real(kind = kreal), intent(in) :: nod_vol_tot, sub_volume
!
      integer(kind = kint), intent(inout) :: inod_sort(mesh%node%numnod)
      type(grouping_1d_work), intent(inout) :: sub_z
!
      integer(kind = kint) :: istack_intnod(0:1)
!
!
      call alloc_grouping_1d_work(ione,                                 &
     &    part_param%ndivide_eb(3), part_param%ndomain_eb(3), sub_z)
      call set_z_sorted_node_and_stack(mesh%node, id_block(1,3),        &
     &    sub_z%n_block, sub_z%istack_block(0,1), inod_sort)
!
      istack_intnod(0) = 0
      istack_intnod(1) = mesh%node%internal_node
!
      call set_istack_xyz_domain_block                                  &
     &   (mesh%node, inod_sort, id_block(1,3), node_volume,             &
     &    sub_z%n_block, sub_volume, sub_z%n_domain,                    &
     &    ione, istack_intnod, sub_z%istack_vol, sub_z%vol_grp)
!
!      call check_blocks_4_z_domain(my_rank, mesh%node,                 &
!          part_param, inod_sort, id_block, sub_z)
      if(my_rank .eq. 0) then
        write(*,*) 'vol_grp_z', nod_vol_tot, sub_volume
        call check_z_divided_volumes(part_param, sub_z)
      end if
!
      end subroutine const_istack_z_domain_block
!
! ----------------------------------------------------------------------
!
      subroutine const_istack_xyz_domain_block                          &
     &         (nd, mesh, part_param, prev_part_grp,                    &
     &          id_block, node_volume, nod_vol_tot, sub_volume, inod_sort, part_1d)
!
      use t_mesh_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
!
      use repart_in_xyz_by_volume
      use set_istack_4_domain_block
!
      implicit none
!
      integer(kind = kint), intent(in) :: nd
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: prev_part_grp
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: id_block(mesh%node%numnod,3)
      real(kind = kreal), intent(in) :: node_volume(mesh%node%numnod)
      real(kind = kreal), intent(in) :: nod_vol_tot, sub_volume
!
      integer(kind = kint), intent(inout) :: inod_sort(mesh%node%numnod)
      type(grouping_1d_work), intent(inout) :: part_1d
!
!
      call alloc_grouping_1d_work(prev_part_grp%num_grp,                &
     &   part_param%ndivide_eb(nd), part_param%ndomain_eb(nd), part_1d)
      call set_sorted_node_and_stack(nd, mesh%node, id_block(1,nd),     &
     &   part_1d%ndomain_done, part_1d%idomain_done, part_1d%n_block,   &
     &   prev_part_grp%num_grp, prev_part_grp%istack_grp,               &
     &   part_1d%istack_block, inod_sort)
!
      call set_istack_xyz_domain_block                                  &
     &   (mesh%node, inod_sort, id_block(1,nd), node_volume,            &
     &    part_1d%n_block, sub_volume, part_1d%n_domain,                &
     &    prev_part_grp%num_grp, prev_part_grp%istack_grp,              &
     &    part_1d%istack_vol, part_1d%vol_grp)
!
      if(nd .eq. 1) then
!        call check_blocks_4_xyz_domain(my_rank, mesh%node,             &
!     &      part_param, inod_sort, id_block, part_1d)
        if(my_rank .eq. 0) then
          write(*,*) 'vol_grp_x', nod_vol_tot, sub_volume
          call check_xyz_divided_volumes(part_param, part_1d)
        end if
      else if(nd .eq. 2) then
!        call check_blocks_4_yz_domain(my_rank, mesh%node,              &
!     &      part_param, inod_sort, id_block, part_1d)
        if(my_rank .eq. 0) then
          write(*,*) 'vol_grp_y', nod_vol_tot, sub_volume
          call check_yz_divided_volumes(part_param, part_1d)
        end if
      end if
!
      end subroutine const_istack_xyz_domain_block
!
! ----------------------------------------------------------------------
      subroutine const_z_div_domain_group_data(mesh, part_param, sub_z, inod_sort, z_part_grp)
!
      use calypso_mpi
      use t_mesh_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
      use repart_in_xyz_by_volume
      use set_repartition_group_name
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_test_files_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_z
      integer(kind = kint), intent(in) :: inod_sort(mesh%node%numnod)
!
      type(group_data), intent(inout) :: z_part_grp
!
!
      z_part_grp%num_grp = sub_z%n_domain
      call alloc_group_num(z_part_grp)
      call set_z_domain_grp_name                                        &
     &   (part_param, z_part_grp%num_grp, z_part_grp%grp_name)
      call set_z_domain_grp_stack(sub_z%n_block, sub_z%n_domain,        &
     &    sub_z%istack_block(0,1), sub_z%istack_vol,                    &
     &    z_part_grp%num_grp, z_part_grp%istack_grp)
!      call check_stacks_4_z_domain                                     &
!     &   (my_rank, mesh%node, part_param,                              &
!     &    inod_sort, z_part_grp%num_grp, z_part_grp%istack_grp)
!
      z_part_grp%num_item = z_part_grp%istack_grp(z_part_grp%num_grp)
      call alloc_group_item(z_part_grp)
      call set_domain_grp_item(mesh%node, inod_sort,                    &
     &    z_part_grp%num_grp, z_part_grp%num_item,                      &
     &    z_part_grp%istack_grp, z_part_grp%item_grp)
!
      end subroutine const_z_div_domain_group_data
!
! ----------------------------------------------------------------------
!
      subroutine const_newdomain_group_data                             &
     &         (nd, num_group, mesh, part_param,                        &
     &          prev_part_grp, part_1d, inod_sort, part_grp)
!
      use calypso_mpi
      use t_mesh_data
      use t_group_data
      use t_1d_repartitioning_work
      use t_control_param_vol_grping
      use repart_in_xyz_by_volume
      use set_repartition_group_name
!
      integer(kind = kint), intent(in) :: nd, num_group
      type(mesh_geometry), intent(in) :: mesh
      type(group_data), intent(in) :: prev_part_grp
      type(mesh_test_files_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: part_1d
      integer(kind = kint), intent(in) :: inod_sort(mesh%node%numnod)
!
      type(group_data), intent(inout) :: part_grp
!
      part_grp%num_grp = num_group
      call alloc_group_num(part_grp)
!
      call set_newdomain_grp_stack                                      &
     &   (part_1d%n_block, part_1d%n_domain, prev_part_grp%num_grp,     &
     &    part_1d%ndomain_done, part_1d%idomain_done,                   &
     &    part_1d%istack_block, part_1d%istack_vol, part_grp%num_grp,   &
     &    part_grp%istack_grp)
!      call check_stacks_4_new_domain(my_rank, mesh%node, part_param,   &
!     &    inod_sort, part_1d%ndomain_done, part_1d%idomain_done,       &
!     &    part_param%new_nprocs, part_grp%istack_grp)
!
      if(nd .eq. 1) then
        call set_xyz_domain_grp_name                                    &
     &     (part_param, part_grp%num_grp, part_grp%grp_name)
      else if(nd .eq. 2) then
        call set_yz_domain_grp_name                                     &
     &     (part_param, part_grp%num_grp, part_grp%grp_name)
      end if
!
      part_grp%num_item = part_grp%istack_grp(part_grp%num_grp)
      call alloc_group_item(part_grp)
      call set_domain_grp_item(mesh%node, inod_sort,                    &
     &                         part_grp%num_grp, part_grp%num_item,     &
     &                         part_grp%istack_grp, part_grp%item_grp)
!
      end subroutine const_newdomain_group_data
!
! ----------------------------------------------------------------------
!
      subroutine set_xyz_block_by_nod_volume                            &
     &         (mesh, part_param, node_volume, nod_vol_tot, id_block)
!
      use calypso_mpi
      use t_mesh_data
      use t_control_param_vol_grping
!
      use calypso_mpi_real
      use int_volume_of_single_domain
      use solver_SR_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_test_files_param), intent(in) :: part_param
      real(kind = kreal), intent(inout)                                 &
     &                   :: node_volume(mesh%node%numnod)
      real(kind = kreal), intent(inout) :: nod_vol_tot
      integer(kind = kint), intent(inout)                               &
     &                    :: id_block(mesh%node%numnod,3)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      real(kind = kreal) :: vol_lc
      integer(kind = kint) :: inod, nd
!
!
      call cal_node_volue(mesh%node, mesh%ele, node_volume)
      call SOLVER_SEND_RECV_type                                        &
     &   (mesh%node%numnod, mesh%nod_comm, node_volume)
!
      vol_lc = 0.0d0
      do inod = 1, mesh%node%internal_node
        vol_lc = vol_lc + node_volume(inod)
      end do
      call calypso_mpi_allreduce_one_real(vol_lc, nod_vol_tot, MPI_SUM)
!
      if(my_rank .eq. 0) then
        write(*,*) 'xyz_min_gl', mesh%node%xyz_min_gl(1:3)
        write(*,*) 'xyz_max_gl', mesh%node%xyz_max_gl(1:3)
      end if
!
      size_gl(1:3) = mesh%node%xyz_max_gl(1:3)                         &
     &              - mesh%node%xyz_min_gl(1:3)
      size_blk(1:3) = size_gl(1:3) / dble(part_param%ndivide_eb(1:3))
      do nd = 1, 3
!$omp parallel do private(inod)
        do inod = 1, mesh%node%numnod
          id_block(inod,nd) = int((mesh%node%xx(inod,nd)                &
     &                     - mesh%node%xyz_min_gl(nd)) / size_blk(nd))
          id_block(inod,nd)                                             &
     &          = min(id_block(inod,nd)+1,part_param%ndivide_eb(nd))
        end do
!$omp end parallel do
      end do
!
      end subroutine set_xyz_block_by_nod_volume
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_grouping
!
