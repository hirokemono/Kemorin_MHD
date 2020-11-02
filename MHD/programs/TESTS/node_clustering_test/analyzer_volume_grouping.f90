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
      use nod_phys_send_recv
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
      use quicksort
!
      use int_volume_of_single_domain
      use set_parallel_file_name
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
      integer(kind = kint), allocatable :: id_block(:,:)
      real(kind = kreal), allocatable :: node_volume(:)
!
      integer(kind = kint), allocatable :: inod_sort(:)
!
      type grouping_1d_work
        integer(kind = kint) :: ndomain_done
        integer(kind = kint), allocatable :: idomain_finished(:)
!
        integer(kind = kint) :: n_block
        integer(kind = kint) :: n_domain
        integer(kind = kint), allocatable :: istack_block(:,:)
        integer(kind = kint), allocatable :: istack_volume(:,:)
        real(kind = kreal), allocatable :: vol_grp(:,:)
      end type grouping_1d_work
!
      type(grouping_1d_work) :: sub_z
      type(grouping_1d_work) :: sub_y
      type(grouping_1d_work) :: sub_x
!
      integer(kind = kint), allocatable :: istack_vol_x(:,:)
      real(kind = kreal), allocatable :: vol_grp_x(:,:)
      integer(kind = kint), allocatable :: istack_vol_y(:,:)
      real(kind = kreal), allocatable :: vol_grp_y(:,:)
!
      integer(kind = kint), allocatable :: istack_vol_z(:)
      real(kind = kreal), allocatable :: vol_grp_z(:)
!
      integer(kind = kint) :: num_nod_grp_yz
      integer(kind = kint), allocatable :: idomain_nod_grp_yz(:)
      integer(kind = kint), allocatable :: istack_block_x(:,:)
!
      integer(kind = kint) :: ndomain_yz
      integer(kind = kint) :: num_nod_grp_z
      integer(kind = kint), allocatable :: idomain_nod_grp_z(:)
      integer(kind = kint), allocatable :: istack_block_y(:,:)
!
      integer(kind = kint), allocatable :: istack_block_z(:)
      integer(kind = kint) :: istack_intnod(0:1)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      real(kind = kreal) :: nod_vol_tot, vol_ref, sub_volume
      integer(kind = kint) :: inod
      integer(kind = kint) :: i, nd
!
      type(group_data) :: z_part_grp
      type(group_data) :: yz_part_grp
!
      type(group_data) :: part_grp
      type(group_data) :: grp_tmp
!
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
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
      allocate(node_volume(fem_T%mesh%node%numnod))
      allocate(id_block(fem_T%mesh%node%numnod,3))
      call cal_node_volue                                               &
     &   (fem_T%mesh%node, fem_T%mesh%ele, node_volume)
!
      call init_send_recv(fem_T%mesh%nod_comm)
      call SOLVER_SEND_RECV_type(fem_T%mesh%node%numnod,                &
     &                           fem_T%mesh%nod_comm, node_volume)
!
      sub_volume = 0.0d0
      do inod = 1, fem_T%mesh%node%internal_node
        sub_volume = sub_volume + node_volume(inod)
      end do
!
      call calypso_mpi_allreduce_one_real                               &
     &   (sub_volume, nod_vol_tot, MPI_SUM)
!
      if(my_rank .eq. 0) then
        write(*,*) 'xyz_min_gl', fem_T%mesh%node%xyz_min_gl(1:3)
        write(*,*) 'xyz_max_gl', fem_T%mesh%node%xyz_max_gl(1:3)
      end if
!
      size_gl(1:3) =  fem_T%mesh%node%xyz_max_gl(1:3)                   &
     &              - fem_T%mesh%node%xyz_min_gl(1:3)
      size_blk(1:3) = size_gl(1:3) / dble(T_meshes%ndivide_eb(1:3))
      do nd = 1, 3
!$omp parallel do private(inod)
        do inod = 1,fem_T%mesh%node%numnod
          id_block(inod,nd) = int((fem_T%mesh%node%xx(inod,nd)          &
     &               - fem_T%mesh%node%xyz_min_gl(nd)) / size_blk(nd))
          id_block(inod,nd)                                             &
     &          = min(id_block(inod,nd)+1,T_meshes%ndivide_eb(nd))
        end do
!$omp end parallel do
      end do
!
!
      allocate(inod_sort(fem_T%mesh%node%numnod))
      allocate(istack_block_z(0:T_meshes%ndivide_eb(3)))
!
      call set_z_sorted_node_and_stack(fem_T%mesh%node, id_block(1,3),  &
     &    T_meshes%ndivide_eb(3), istack_block_z, inod_sort)
!      call check_blocks_4_z_domain                                     &
!     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,     &
!     &    istack_block_z)
!
!
      istack_intnod(0) = 0
      istack_intnod(1) = fem_T%mesh%node%internal_node
      sub_volume = nod_vol_tot / dble(T_meshes%ndomain_eb(3))
      allocate(istack_vol_z(0:T_meshes%ndomain_eb(3)))
      allocate(vol_grp_z(T_meshes%ndomain_eb(3)))
      call set_istack_xyz_domain_block                                  &
     &   (fem_T%mesh%node, inod_sort, id_block(1,3), node_volume,       &
     &    T_meshes%ndivide_eb(3), sub_volume, T_meshes%ndomain_eb(3),   &
     &    ione, istack_intnod, istack_vol_z(0), vol_grp_z(1))
!
      if(my_rank .eq. 0) then
        call check_z_divided_volumes                                    &
     &     (T_meshes, nod_vol_tot, sub_volume, istack_vol_z, vol_grp_z)
      end if
!
!
      z_part_grp%num_grp = T_meshes%ndomain_eb(3)
      call alloc_group_num(z_part_grp)
      call set_z_domain_grp_name                                        &
     &   (T_meshes, z_part_grp%num_grp, z_part_grp%grp_name)
      call set_z_domain_grp_stack                                       &
     &   (T_meshes%ndivide_eb(3), T_meshes%ndomain_eb(3),               &
     &    istack_block_z, istack_vol_z, z_part_grp%num_grp,             &
     &    z_part_grp%istack_grp)
!      call check_stacks_4_z_domain                                     &
!     &   (my_rank, fem_T%mesh%node, T_meshes,                          &
!     &    inod_sort, z_part_grp%num_grp, z_part_grp%istack_grp)
!
!      z_part_grp%num_item = z_part_grp%istack_grp(z_part_grp%num_grp)
!      call alloc_group_item(z_part_grp)
!      call set_domain_grp_item(fem_T%mesh%node, inod_sort,             &
!     &    z_part_grp%num_grp, z_part_grp%num_item,                     &
!     &    z_part_grp%istack_grp, z_part_grp%item_grp)
!
      num_nod_grp_z = count_z_subdomain_num(T_meshes,                   &
     &                       z_part_grp%num_grp, z_part_grp%istack_grp)
      allocate(idomain_nod_grp_z(num_nod_grp_z+1))
!
      call set_z_subdomain_list                                         &
     &   (T_meshes, z_part_grp%num_grp, z_part_grp%istack_grp,          &
     &    num_nod_grp_z, idomain_nod_grp_z)
!
      deallocate(vol_grp_z, istack_vol_z)
!
!   For y direction
!
      allocate(istack_block_y(0:T_meshes%ndivide_eb(2),num_nod_grp_z))
      allocate(istack_vol_y(0:T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3)))
      allocate(vol_grp_y(T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3)))
!$omp parallel workshare
        istack_block_y(0:T_meshes%ndivide_eb(2),1:num_nod_grp_z) = 0
!$omp end parallel workshare
!
      call set_sorted_node_and_stack(2, fem_T%mesh%node, id_block(1,2), &
     &    num_nod_grp_z, idomain_nod_grp_z, T_meshes%ndivide_eb(2),     &
     &    z_part_grp%num_grp, z_part_grp%istack_grp,                    &
     &    istack_block_y, inod_sort)
!
!      call check_blocks_4_yz_domain                                    &
!     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,     &
!     &    num_nod_grp_z, idomain_nod_grp_z, istack_block_y)
!
      ndomain_yz = T_meshes%ndomain_eb(2) * T_meshes%ndomain_eb(3)
      sub_volume = nod_vol_tot / dble(ndomain_yz)
!
      call set_istack_xyz_domain_block                                  &
     &   (fem_T%mesh%node, inod_sort, id_block(1,2), node_volume,       &
     &    T_meshes%ndivide_eb(2), sub_volume, T_meshes%ndomain_eb(2),   &
     &    T_meshes%ndomain_eb(3), z_part_grp%istack_grp,                &
     &    istack_vol_y, vol_grp_y)
!
      if(my_rank .eq. 0) then
        call check_yz_divided_volumes                                   &
     &     (T_meshes, nod_vol_tot, sub_volume, istack_vol_y, vol_grp_y)
      end if
!
      yz_part_grp%num_grp = ndomain_yz
      call alloc_group_num(yz_part_grp)
      call set_yz_domain_grp_name                                       &
     &   (T_meshes, yz_part_grp%num_grp, yz_part_grp%grp_name)
      call set_newdomain_grp_stack(T_meshes%ndivide_eb(2),              &
     &    T_meshes%ndomain_eb(2), T_meshes%ndomain_eb(3),               &
     &    num_nod_grp_z, idomain_nod_grp_z, istack_block_y,             &
     &    istack_vol_y, yz_part_grp%num_grp, yz_part_grp%istack_grp)
!      call check_stacks_4_yz_domain(my_rank, fem_T%mesh%node,          &
!     &    T_meshes, inod_sort, num_nod_grp_z, idomain_nod_grp_z,       &
!     &    yz_part_grp%num_grp, yz_part_grp%istack_grp)
!
!      yz_part_grp%num_item = yz_part_grp%istack_grp(yz_part_grp%num_grp)
!      call alloc_group_item(yz_part_grp)
!      call set_domain_grp_item(fem_T%mesh%node, inod_sort,             &
!     &    yz_part_grp%num_grp, yz_part_grp%num_item,                   &
!     &    yz_part_grp%istack_grp, yz_part_grp%item_grp)
!
!
      num_nod_grp_yz = count_yz_subdomain_num(T_meshes,                 &
     &                     num_nod_grp_z, idomain_nod_grp_z,            &
     &                     yz_part_grp%num_grp, yz_part_grp%istack_grp)
      allocate(idomain_nod_grp_yz(num_nod_grp_yz+1))
!
      call set_yz_subdomain_list                                        &
     &   (T_meshes, num_nod_grp_z, idomain_nod_grp_z,                   &
     &    yz_part_grp%num_grp, yz_part_grp%istack_grp, num_nod_grp_yz,  &
     &    idomain_nod_grp_yz)
!
      deallocate(vol_grp_y, istack_vol_y)
!
!   For x direction
!
      allocate(istack_block_x(0:T_meshes%ndivide_eb(1),num_nod_grp_yz))
      allocate(istack_vol_x(0:T_meshes%ndomain_eb(1),yz_part_grp%num_grp))
      allocate(vol_grp_x(T_meshes%ndomain_eb(1),yz_part_grp%num_grp))
!$omp parallel workshare
      istack_block_x(0:T_meshes%ndivide_eb(1),1:num_nod_grp_yz) = 0
!$omp end parallel workshare
!$omp parallel workshare
      istack_vol_x(0:T_meshes%ndomain_eb(1),1:yz_part_grp%num_grp) = 0
!$omp end parallel workshare
!$omp parallel workshare
      vol_grp_x(1:T_meshes%ndomain_eb(1),1:yz_part_grp%num_grp) =    0
!$omp end parallel workshare
!
      call set_sorted_node_and_stack(1, fem_T%mesh%node, id_block(1,1), &
     &    num_nod_grp_yz, idomain_nod_grp_yz, T_meshes%ndivide_eb(1),   &
     &    yz_part_grp%num_grp, yz_part_grp%istack_grp,                  &
     &    istack_block_x, inod_sort)
!      call check_blocks_4_xyz_domain                                   &
!     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,     &
!     &    num_nod_grp_yz, idomain_nod_grp_yz, istack_block_x)
!
      sub_volume = nod_vol_tot / dble(T_meshes%new_nprocs)
      call set_istack_xyz_domain_block                                  &
     &   (fem_T%mesh%node, inod_sort, id_block(1,1), node_volume,       &
     &    T_meshes%ndivide_eb(1), sub_volume, T_meshes%ndomain_eb(1),   &
     &    yz_part_grp%num_grp, yz_part_grp%istack_grp,                  &
     &    istack_vol_x, vol_grp_x)
!
      deallocate(node_volume)
      deallocate(id_block)
!
      if(my_rank .eq. 0) then
        call check_xyz_divided_volumes                                  &
     &     (T_meshes, nod_vol_tot, sub_volume,                          &
     &      yz_part_grp%num_grp, istack_vol_x, vol_grp_x)
      end if
!
!
      part_grp%num_grp = T_meshes%new_nprocs
      call alloc_group_num(part_grp)
      call set_xyz_domain_grp_name                                      &
     &   (T_meshes, part_grp%num_grp, part_grp%grp_name)
      call set_newdomain_grp_stack                                      &
     &   (T_meshes%ndivide_eb(1), T_meshes%ndomain_eb(1),               &
     &    yz_part_grp%num_grp, num_nod_grp_yz, idomain_nod_grp_yz,      &
     &    istack_block_x, istack_vol_x, part_grp%num_grp,               &
     &    part_grp%istack_grp)
!      call check_stacks_4_new_domain(my_rank,                          &
!     &    fem_T%mesh%node, T_meshes, inod_sort, num_nod_grp_yz,        &
!     &    idomain_nod_grp_yz, T_meshes%new_nprocs, part_grp%istack_grp)
!
      part_grp%num_item = part_grp%istack_grp(part_grp%num_grp)
      call alloc_group_item(part_grp)
      call set_domain_grp_item(fem_T%mesh%node, inod_sort,              &
     &                         part_grp%num_grp, part_grp%num_item,     &
     &                         part_grp%istack_grp, part_grp%item_grp)
!
      deallocate(istack_vol_x, vol_grp_x)
      deallocate(istack_block_x)
      deallocate(inod_sort)
!
      deallocate(idomain_nod_grp_z, idomain_nod_grp_yz)
      call dealloc_group_num(z_part_grp)
      call dealloc_group_num(yz_part_grp)
!
!       Append group data
      call copy_group_data(fem_T%group%nod_grp, grp_tmp)
      call dealloc_group(fem_T%group%nod_grp)
!
      fem_T%group%nod_grp%num_grp = grp_tmp%num_grp + part_grp%num_grp
      call alloc_group_num(fem_T%group%nod_grp)
!
      fem_T%group%nod_grp%istack_grp(0) =  grp_tmp%istack_grp(0)
      do i = 1, grp_tmp%num_grp
        fem_T%group%nod_grp%grp_name(i) =   grp_tmp%grp_name(i)
        fem_T%group%nod_grp%istack_grp(i) = grp_tmp%istack_grp(i)
      end do
      do i = 1, part_grp%num_grp
        fem_T%group%nod_grp%grp_name(i+grp_tmp%num_grp)                 &
     &      = part_grp%grp_name(i)
        fem_T%group%nod_grp%istack_grp(i+grp_tmp%num_grp)               &
     &      = part_grp%istack_grp(i) + grp_tmp%num_item
      end do
!
      fem_T%group%nod_grp%num_item                                      &
     &   = fem_T%group%nod_grp%istack_grp(fem_T%group%nod_grp%num_grp)
      call alloc_group_item(fem_T%group%nod_grp)
!
      do i = 1, grp_tmp%num_item
        fem_T%group%nod_grp%item_grp(i) =   grp_tmp%item_grp(i)
      end do
      do i = 1, part_grp%num_item
        fem_T%group%nod_grp%item_grp(i+grp_tmp%num_item)                &
     &     =   part_grp%item_grp(i)
      end do
!
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
!
      integer(kind = kint) function count_z_subdomain_num               &
     &                            (T_meshes, ndomain_z, istack_z_grp)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: ndomain_z
      integer(kind = kint), intent(in) :: istack_z_grp(0:ndomain_z)
!
      integer(kind = kint) :: icou, iz, num
!
      icou = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        num = istack_z_grp(iz) - istack_z_grp(iz-1)
        if(num .gt. 0) icou = icou + 1
      end do
      count_z_subdomain_num = icou
!
      end function count_z_subdomain_num
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_yz_subdomain_num              &
     &         (T_meshes, num_nod_grp_z, idomain_nod_grp_z,             &
     &          ndomain_yz, istack_yz_grp)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
!
      integer(kind = kint) :: i, icou, iy, iz, jk, num
!
!
      icou = 0
      do i = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(i)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          num = istack_yz_grp(jk) - istack_yz_grp(jk-1)
          if(num .gt. 0) icou = icou + 1
        end do
      end do
      count_yz_subdomain_num = icou
!
      end function count_yz_subdomain_num
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_grouping
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_z_subdomain_list                                   &
     &         (T_meshes, ndomain_z, istack_z_grp, num_nod_grp_z,       &
     &          idomain_nod_grp_z)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: ndomain_z
      integer(kind = kint), intent(in) :: istack_z_grp(0:ndomain_z)
!
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(inout)                               &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
!
      integer(kind = kint) :: icou, iz, num
!
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_z+1
        idomain_nod_grp_z(icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        num = istack_z_grp(iz) - istack_z_grp(iz-1)
        if(num .gt. 0) then
          icou = icou + 1
          idomain_nod_grp_z(icou) = iz
        end if
      end do
      idomain_nod_grp_z(num_nod_grp_z+1) = T_meshes%ndomain_eb(3) + 1
!
      end subroutine set_z_subdomain_list
!
! ----------------------------------------------------------------------
!
      subroutine set_yz_subdomain_list                                  &
     &         (T_meshes, num_nod_grp_z, idomain_nod_grp_z,             &
     &          ndomain_yz, istack_yz_grp, num_nod_grp_yz,              &
     &          idomain_nod_grp_yz)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(inout)                               &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz+1)
!
      integer(kind = kint) :: i, icou, iy, iz, jk, num
!
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_yz+1
        idomain_nod_grp_yz(icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do i = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(i)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          num = istack_yz_grp(jk) - istack_yz_grp(jk-1)
          if(num .gt. 0) then
            icou = icou + 1
            idomain_nod_grp_yz(icou)                                    &
     &           = iy + (iz-1) * T_meshes%ndomain_eb(2)
!            idomain_nod_grp_yz(2,icou) = iy
!            idomain_nod_grp_yz(3,icou) = iz
          end if
        end do
      end do
      idomain_nod_grp_yz(num_nod_grp_yz+1)                              &
     &              = T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3) + 1
!      idomain_nod_grp_yz(2,num_nod_grp_yz+1) = T_meshes%ndomain_eb(2)+1
!      idomain_nod_grp_yz(3,num_nod_grp_yz+1) = T_meshes%ndomain_eb(3)
!
      end subroutine set_yz_subdomain_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_z_sorted_node_and_stack(node, id_block,            &
     &          nblock_z, istack_block_z, inod_sort)
!
      use m_constants
      use t_geometry_data
      use quicksort
!
      implicit none
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: id_block(node%numnod)
      integer(kind = kint), intent(in) :: nblock_z
!
      integer(kind = kint), intent(inout) :: istack_block_z(0:nblock_z)
      integer(kind = kint), intent(inout) :: inod_sort(node%numnod)
!
      integer(kind = kint) :: inum, inod, jnod, iz, jz
      real(kind = kreal), allocatable :: data_sort(:)
!
!
      allocate(data_sort(node%numnod))
!
!$omp parallel do private(inod)
      do inod = 1, node%internal_node
        data_sort(inod) = node%xx(inod,3)
        inod_sort(inod) = inod
      end do
!$omp end parallel do
!$omp parallel do private(inod)
      do inod = node%internal_node+1, node%numnod
        data_sort(inod) = node%xx(inod,3)
        inod_sort(inod) = inod
      end do
!$omp end parallel do
!
      call quicksort_real_w_index(node%numnod, data_sort,               &
     &    ione, node%internal_node, inod_sort)
!
!$omp parallel workshare
      istack_block_z(0:nblock_z) = 0
!$omp end parallel workshare
      do inum = 1, node%internal_node-1
        inod = inod_sort(inum)
        jnod = inod_sort(inum+1)
        iz = id_block(inod)
        jz = id_block(jnod)
        if(iz .ne. jz) istack_block_z(iz:jz) = inum
      end do
      inum = node%internal_node
      inod = inod_sort(inum)
      iz = id_block(inod)
      istack_block_z(iz:nblock_z) = inum
!
      deallocate(data_sort)
!
      end subroutine set_z_sorted_node_and_stack
!
! ----------------------------------------------------------------------
!
      subroutine set_sorted_node_and_stack(nd, node, id_block,          &
     &          num_nod_grp_yz, idomain_nod_grp_yz, nblock_x,           &
     &          ndomain_yz, istack_yz_grp, istack_block_x, inod_sort)
!
      use t_geometry_data
      use quicksort
!
      implicit none
!
      integer(kind = kint), intent(in) :: nd
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: id_block(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz)
!
      integer(kind = kint), intent(in) :: nblock_x
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_block_x(0:nblock_x,num_nod_grp_yz)
      integer(kind = kint), intent(inout) :: inod_sort(node%numnod)
!
      integer(kind = kint) :: jk, ist, ied, icou, inum
      integer(kind = kint) :: inod, jnod, ix, jx
      real(kind = kreal), allocatable :: data_sort(:)
!
!
      allocate(data_sort(node%numnod))
!
!$omp parallel do private(inod)
      do inum = 1, node%internal_node
        inod = inod_sort(inum)
        data_sort(inum) = node%xx(inod,nd)
      end do
!$omp end parallel do
!$omp parallel do private(inod)
      do inod = node%internal_node+1, node%numnod
        data_sort(inod) = node%xx(inod,nd)
      end do
!$omp end parallel do
!
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(icou)
        ist = istack_yz_grp(jk-1) + 1
        ied = istack_yz_grp(jk)
        if(ied .gt. ist) then
          call quicksort_real_w_index                                   &
     &       (node%numnod, data_sort, ist, ied, inod_sort)
        end if
!
!$omp parallel workshare
        istack_block_x(0:nblock_x,icou) = istack_yz_grp(jk-1)
!$omp end parallel workshare
        do inum = ist, ied-1
          inod = inod_sort(inum)
          jnod = inod_sort(inum+1)
          ix = id_block(inod)
          jx = id_block(jnod)
          if(ix .ne. jx) istack_block_x(ix:jx,icou) = inum
        end do
        inod = inod_sort(ied)
        ix = id_block(inod)
        istack_block_x(ix:nblock_x,icou) = ied
      end do
!
      deallocate(data_sort)
!
      end subroutine set_sorted_node_and_stack
!
! ----------------------------------------------------------------------
!
      subroutine set_istack_xyz_domain_block                            &
     &         (node, inod_sort, id_block, node_volume,                 &
     &          nblock_x, sub_volume, ndomain_x, ndomain_yz,            &
     &          istack_yz_grp, istack_vol, vol_grp)
!
      use t_geometry_data
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use transfer_to_long_integers
!
      implicit none
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod)
      real(kind = kreal), intent(in) :: node_volume(node%numnod)
!
      integer(kind = kint), intent(in) :: nblock_x
      integer(kind = kint), intent(in) :: ndomain_x, ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
      real(kind = kreal), intent(in) :: sub_volume
!
      integer(kind = kint), intent(inout)                               &
     &                      :: istack_vol(0:ndomain_x,ndomain_yz)
      real(kind = kreal), intent(inout)                                 &
     &                      :: vol_grp(ndomain_x,ndomain_yz)
!
!      integer(kind = kint) :: ip
      integer(kind = kint) :: jk, ist, ied, i, j, inod, inum
      real(kind = kreal), allocatable :: vol_block_lc(:)
      real(kind = kreal), allocatable :: vol_block_gl(:)
      real(kind = kreal) :: vol_ref
!
!
      allocate(vol_block_lc(nblock_x))
      allocate(vol_block_gl(nblock_x))
!
      do jk = 1, ndomain_yz
        vol_block_lc(1:nblock_x) = 0.0d0
        ist = istack_yz_grp(jk-1) + 1
        ied = istack_yz_grp(jk  )
        do inum = ist, ied
          inod = inod_sort(inum)
          i = id_block(inod)
          vol_block_lc(i) = vol_block_lc(i) + node_volume(inod)
        end do
!
        call calypso_mpi_reduce_real(vol_block_lc, vol_block_gl,        &
     &      cast_long(nblock_x), MPI_SUM, int(jk-1))
      end do
      deallocate(vol_block_lc)
!
!      do ip = 1, ndomain_yz
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. ndomain_yz) then
        jk = 1 + my_rank
!
        vol_ref = 0.0d0
        vol_grp(1:ndomain_x,jk) = 0.0d0
        istack_vol(0,jk) = 0
        do i = 1, nblock_x
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),ndomain_x)
          istack_vol(j,jk) = i
          vol_grp(j,jk) = vol_ref
        end do
!
        do j = ndomain_x, 2, - 1
          vol_grp(j,jk) = vol_grp(j,jk) - vol_grp(j-1,jk)
        end do
      end if
!      end if
!      end do
      deallocate(vol_block_gl)
!
      call calypso_mpi_barrier
      do jk = 1, ndomain_yz
        call calypso_mpi_bcast_int(istack_vol(0,jk),                    &
     &                             cast_long(ndomain_x+1), int(jk-1))
        call calypso_mpi_bcast_real(vol_grp(1,jk),                      &
     &                              cast_long(ndomain_x), int(jk-1))
      end do
!
      end subroutine set_istack_xyz_domain_block
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_z_domain_grp_name                                  &
     &         (T_meshes, num_domain_grp, domain_grp_name)
!
      use t_control_param_vol_grping
      use set_parallel_file_name
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in) :: num_domain_grp
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara), parameter :: base_name = 'new_domain'
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz
!
!$omp parallel do private(iz)
      do iz = 1, T_meshes%ndomain_eb(3)
        write(chara_tmp,'(a,a2)') trim(base_name), '_z'
        call add_index_after_name(iz, chara_tmp, domain_grp_name(iz))
      end do
!$omp end parallel do
!
      end subroutine set_z_domain_grp_name
!
! ----------------------------------------------------------------------
!
      subroutine set_yz_domain_grp_name                                 &
     &         (T_meshes, num_domain_grp, domain_grp_name)
!
      use t_control_param_vol_grping
      use set_parallel_file_name
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in) :: num_domain_grp
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara), parameter :: base_name = 'new_domain'
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz, iy, jk
!
!$omp parallel do private(iy,iz,jk)
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
!
          write(chara_tmp,'(a,a2)') trim(base_name),'_y'
          call add_index_after_name(iy, chara_tmp, domain_grp_name(jk))
          write(chara_tmp,'(a,a2)') trim(domain_grp_name(jk)),'_z'
          call add_index_after_name(iz, chara_tmp, domain_grp_name(jk))
        end do
      end do
!$omp end parallel do
!
      end subroutine set_yz_domain_grp_name
!
! ----------------------------------------------------------------------
!
      subroutine set_xyz_domain_grp_name                                &
     &         (T_meshes, num_domain_grp, domain_grp_name)
!
      use t_control_param_vol_grping
      use set_parallel_file_name
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in) :: num_domain_grp
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara), parameter :: base_name = 'new_domain'
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz, iy, ix, jk, i
!
!$omp parallel do private(i,ix,iy,iz,jk)
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          do ix = 1, T_meshes%ndomain_eb(1)
            i = ix + (jk-1) * T_meshes%ndomain_eb(1)
!
            write(chara_tmp,'(a,a2)') trim(base_name), '_x'
            call add_index_after_name                                   &
     &         (ix, chara_tmp, domain_grp_name(i))
            write(chara_tmp,'(a,a2)') trim(domain_grp_name(i)), '_y'
            call add_index_after_name                                   &
     &         (iy, chara_tmp, domain_grp_name(i))
            write(chara_tmp,'(a,a2)') trim(domain_grp_name(i)), '_z'
            call add_index_after_name                                   &
     &         (iz, chara_tmp, domain_grp_name(i))
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_xyz_domain_grp_name
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_z_domain_grp_stack                                 &
     &         (nblock_z, ndomain_z, istack_block_z, istack_vol_z,      &
     &          num_domain_grp, istack_domain_grp)
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), intent(in) :: nblock_z
      integer(kind = kint), intent(in) :: ndomain_z
      integer(kind = kint), intent(in) :: istack_block_z(0:nblock_z)
      integer(kind = kint), intent(in) :: istack_vol_z(0:ndomain_z)
!
      integer(kind = kint), intent(in) :: num_domain_grp
      integer(kind = kint), intent(inout)                               &
     &       :: istack_domain_grp(0:num_domain_grp)
!
      integer(kind = kint) :: j, ist, ied, jst, jed
!
!
!$omp parallel workshare
      istack_domain_grp(0:num_domain_grp) = 0
!$omp end parallel workshare
!
!$omp parallel do private(ist,ied,jst,jed,j)
      do j = 1, ndomain_z
        jst = istack_vol_z(j-1) + 1
        jed = istack_vol_z(j)
        ist = istack_block_z(jst-1) + 1
        ied = istack_block_z(jed)
        istack_domain_grp(j) = ied
      end do
!$omp end parallel do
!
      end subroutine set_z_domain_grp_stack
!
! ----------------------------------------------------------------------
!
      subroutine set_newdomain_grp_stack                                &
     &         (nblock, ndomain_x, ndomain_yz,                          &
     &          num_nod_grp, idomain_nod_grp, istack_block,             &
     &          istack_volume, num_domain_grp, istack_domain_grp)
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), intent(in) :: nblock
      integer(kind = kint), intent(in) :: ndomain_x, ndomain_yz
      integer(kind = kint), intent(in) :: num_nod_grp
      integer(kind = kint), intent(in)                                  &
     &       :: idomain_nod_grp(num_nod_grp+1)
      integer(kind = kint), intent(in)                                  &
     &       :: istack_block(0:nblock,num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &       :: istack_volume(0:ndomain_x,ndomain_yz)
!
      integer(kind = kint), intent(in) :: num_domain_grp
      integer(kind = kint), intent(inout)                               &
     &       :: istack_domain_grp(0:num_domain_grp)
!
      integer(kind = kint) :: ist, ied, jst, jed
      integer(kind = kint) :: icou, i, ix, jk, jk1, jk2
!
!
!$omp parallel workshare
      istack_domain_grp(0:num_domain_grp) = 0
!$omp end parallel workshare
!
!$omp parallel do private(icou,ist,ied,jst,jed,i,ix,jk,jk1,jk2)
      do icou = 1, num_nod_grp
        jk1 = idomain_nod_grp(icou)
        jk2 = idomain_nod_grp(icou+1)
        do ix = 1, ndomain_x
          i = ix + (jk1-1) * ndomain_x
          jst = istack_volume(ix-1,jk1) + 1
          jed = istack_volume(ix,  jk1)
          ist = istack_block(jst-1,icou) + 1
          ied = istack_block(jed,  icou)
          istack_domain_grp(i) = ied
        end do
!
        do jk = jk1+1, jk2-1
          ist = (jk-1) * ndomain_x
          istack_domain_grp(ist+1:ist+ndomain_x)                        &
     &        = istack_domain_grp(jk1*ndomain_x)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_newdomain_grp_stack
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_domain_grp_item(node, inod_sort,                   &
     &          num_domain_grp, num_domain_item,                        &
     &          istack_domain_grp, item_domain_grp)
!
      use m_constants
      use t_geometry_data
      use quicksort
!
      implicit none
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_domain_grp
      integer(kind = kint), intent(in) :: num_domain_item
      integer(kind = kint), intent(in)                                  &
     &       :: istack_domain_grp(0:num_domain_grp)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: item_domain_grp(num_domain_item)
!
      integer(kind = kint) :: inum, inod, i, ist, num
!
!
!$omp parallel do private(inum,inod)
      do inum = 1, num_domain_item
        inod = inod_sort(inum)
        item_domain_grp(inum) = inod
      end do
!$omp end parallel do
!
!$omp parallel do private(i,ist,num)
      do i = 1, num_domain_grp
        ist = istack_domain_grp(i-1)
        num = istack_domain_grp(i  ) - ist
        if(num .gt. 1) then
          call quicksort_int(num, item_domain_grp(ist+1), ione, num)
        end if
      end do
!$omp end parallel do
!
      end subroutine set_domain_grp_item
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_z_divided_volumes(T_meshes,                      &
     &          nod_vol_tot, sub_volume, istack_vol_z, vol_grp_z)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in)                                  &
     &   :: istack_vol_z(0:T_meshes%ndomain_eb(3))
      real(kind = kreal), intent(in)                                    &
     &   :: vol_grp_z(T_meshes%ndomain_eb(3))
      real(kind = kreal), intent(in) :: nod_vol_tot, sub_volume
!
      integer(kind = kint) :: iz
!
!
      write(*,*) 'vol_grp_z', nod_vol_tot, sub_volume
      do iz = 1, T_meshes%ndomain_eb(3)
        write(*,*) iz, istack_vol_z(iz), vol_grp_z(iz)
      end do
!
      end subroutine check_z_divided_volumes
!
! ----------------------------------------------------------------------
!
      subroutine check_yz_divided_volumes(T_meshes,                     &
     &          nod_vol_tot, sub_volume, istack_vol_y, vol_grp_y)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in)                                  &
     & :: istack_vol_y(0:T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3))
      real(kind = kreal), intent(in)                                    &
     & :: vol_grp_y(T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3))
      real(kind = kreal), intent(in) :: nod_vol_tot, sub_volume
!
      integer(kind = kint) :: iy, iz
!
!
      write(*,*) 'vol_grp_y', nod_vol_tot, sub_volume
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          write(*,*) iy, iz, istack_vol_y(iy,iz), vol_grp_y(iy,iz)
        end do
      end do
!
      end subroutine check_yz_divided_volumes
!
! ----------------------------------------------------------------------
!
      subroutine check_xyz_divided_volumes                              &
     &         (T_meshes, nod_vol_tot, sub_volume,                      &
     &         ndomain_yz, istack_vol_x, vol_grp_x)
!
      use t_control_param_vol_grping
!
      implicit none
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in)                                  &
     &      :: istack_vol_x(0:T_meshes%ndomain_eb(1),ndomain_yz)
      real(kind = kreal), intent(in)                                    &
     &      :: vol_grp_x(T_meshes%ndomain_eb(1),ndomain_yz)
      real(kind = kreal), intent(in) :: nod_vol_tot, sub_volume
!
      integer(kind = kint) :: ix, iy, iz, jk
!
!
      write(*,*) 'vol_grp_x', nod_vol_tot, sub_volume
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          do ix = 1, T_meshes%ndomain_eb(1)
            write(*,*) ix,iy,iz, istack_vol_x(ix,jk), vol_grp_x(ix,jk)
          end do
        end do
      end do
!
      end subroutine check_xyz_divided_volumes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_blocks_4_z_domain(my_rank, node, T_meshes,       &
     &          inod_sort, id_block, istack_block_z)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint), intent(in)                                  &
     &     :: istack_block_z(0:T_meshes%ndivide_eb(3))
!
      integer(kind = kint) :: iz, ist, ied, inum, inod
!
      do iz = 1, T_meshes%ndivide_eb(3)
        write(100+my_rank,*) 'istack_block_z', iz, istack_block_z(iz)
        ist = istack_block_z(iz-1) + 1
        ied = istack_block_z(iz)
        do inum = ist, ied
          inod = inod_sort(inum)
          write(100+my_rank,*) 'inod', inum, inod, id_block(inod,3),    &
     &                        node%xx(inod,3)
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_blocks_4_z_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_blocks_4_yz_domain                               &
     &         (my_rank, node, T_meshes, inod_sort, id_block,           &
     &          num_nod_grp_z, idomain_nod_grp_z, istack_block_yz)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z)
      integer(kind = kint), intent(in)                                  &
     &     :: istack_block_yz(0:T_meshes%ndivide_eb(2),num_nod_grp_z)
!
      integer(kind = kint) :: iy, iz, ist, ied, inum, inod, icou
!
      do icou = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(icou)
        write(100+my_rank,*) 'istack_block_y0', istack_block_yz(0,icou)
        do iy = 1, T_meshes%ndivide_eb(2)
          write(100+my_rank,*) 'istack_block_yz',                       &
     &                        iy, istack_block_yz(iy,icou)
          ist = istack_block_yz(iy-1,icou) + 1
          ied = istack_block_yz(iy,icou)
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                  id_block(inod,2:3), node%xx(inod,2:3)
          end do
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_blocks_4_yz_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_blocks_4_xyz_domain                              &
     &         (my_rank, node, T_meshes, inod_sort, id_block,           &
     &          num_nod_grp_yz, idomain_nod_grp_yz, istack_block_xyz)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz)
      integer(kind = kint), intent(in)                                  &
     &     :: istack_block_xyz(0:T_meshes%ndivide_eb(1),num_nod_grp_yz)
!
      integer(kind = kint) :: ix, iy, iz, jk, ist, ied
      integer(kind = kint) :: inum, inod, icou
!
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(icou)
        iy = 1 + mod(jk-1,T_meshes%ndomain_eb(2))
        iz = 1 + (jk-1) / T_meshes%ndomain_eb(2)
        do ix = 1, T_meshes%ndivide_eb(2)
          write(100+my_rank,*) 'istack_block_xyz',                      &
     &                        ix, iy, iz, istack_block_xyz(ix,icou)
          ist = istack_block_xyz(ix-1,icou) + 1
          ied = istack_block_xyz(ix,  icou)
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                 id_block(inod,1:3), node%xx(inod,1:3)
          end do
        end do
      end do
!
      end subroutine check_blocks_4_xyz_domain
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_z_domain(my_rank, node, T_meshes,      &
     &          inod_sort, num_nod_group_z, istack_nod_grp_z)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_group_z
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_z(0:num_nod_group_z)
!
      integer(kind = kint) :: iz, ist, ied, inum, inod
!
      do iz = 1, T_meshes%ndomain_eb(3)
        if(istack_nod_grp_z(iz) .eq. istack_nod_grp_z(iz-1)) cycle
!
        write(100+my_rank,*) 'istack_nod_grp_z', iz, istack_nod_grp_z(iz)
        ist = istack_nod_grp_z(iz-1) + 1
        ied = istack_nod_grp_z(iz  )
        do inum = ist, ied
          inod = inod_sort(inum)
          write(100+my_rank,*) 'inod', inum, inod, iz, node%xx(inod,3)
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_z_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_yz_domain(my_rank, node, T_meshes,      &
     &          inod_sort, num_nod_grp_z, idomain_nod_grp_z,            &
     &          num_nod_group_yz, istack_nod_grp_y)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z)
      integer(kind = kint), intent(in) :: num_nod_group_yz
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_y(0:num_nod_group_yz)
!
      integer(kind = kint) :: iy, iz, jk, ist, ied, inum, inod, icou
!
      do icou = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(icou)
        jk = (iz-1) * T_meshes%ndomain_eb(2)
        write(100+my_rank,*) 'istack_nod_grp_y0', iz,                   &
     &                      istack_nod_grp_y(jk)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          write(100+my_rank,*) 'istack_nod_grp_y', iy, iz,              &
     &                        istack_nod_grp_y(jk)
          ist = istack_nod_grp_y(jk-1) + 1
          ied = istack_nod_grp_y(jk  )
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                           iy, iz, node%xx(inod,2:3)
          end do
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_yz_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_new_domain(my_rank, node, T_meshes,     &
     &          inod_sort, num_nod_grp_yz, idomain_nod_grp_yz,          &
     &          num_nod_grp_xyz, istack_nod_grp_xyz)
!
      use t_geometry_data
      use t_control_param_vol_grping
!
      implicit none
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: T_meshes
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz)
      integer(kind = kint), intent(in) :: num_nod_grp_xyz
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_xyz(0:num_nod_grp_xyz)
!
      integer(kind = kint) :: ix, iy, iz, jk, ist, ied
      integer(kind = kint) :: inum, inod, icou, i
!
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(icou)
        iy = 1 + mod(jk-1,T_meshes%ndomain_eb(2))
        iz = 1 + (jk-1) / T_meshes%ndomain_eb(2)
        i = (jk-1) * T_meshes%ndomain_eb(1)
        write(100+my_rank,*) 'istack_nod_grp_x0',                       &
     &                        iy, iz, istack_nod_grp_xyz(i)
        do ix = 1, T_meshes%ndomain_eb(1)
          i = ix + (jk-1) * T_meshes%ndomain_eb(1)
          write(100+my_rank,*) 'istack_nod_grp_xyz', ix, iy, iz,        &
     &                          istack_nod_grp_xyz(i)
          ist = istack_nod_grp_xyz(i-1) + 1
          ied = istack_nod_grp_xyz(i  )
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                            ix, iy, iz, node%xx(inod,1:3)
          end do
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_new_domain
!
! ----------------------------------------------------------------------
!
