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
      use set_parallel_file_name
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
      real(kind = kreal), allocatable :: data_sort(:)
!
      real(kind = kreal), allocatable :: vol_block_lc(:)
      real(kind = kreal), allocatable :: vol_block_gl(:)
!
      integer(kind = kint), allocatable :: istack_vol_x(:,:)
      real(kind = kreal), allocatable :: vol_grp_x(:,:)
!
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
      integer(kind = kint) :: num_group_yz
      integer(kind = kint) :: num_nod_grp_z
      integer(kind = kint), allocatable :: idomain_nod_grp_z(:)
      integer(kind = kint), allocatable :: istack_block_y(:,:)
!
      integer(kind = kint), allocatable :: istack_block_z(:)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      real(kind = kreal) :: nod_vol_tot, vol_ref, sub_volume
      integer(kind = kint) :: inod, inum, icou
      integer(kind = kint) :: ist, ied, num, jnod
      integer(kind = kint) :: i, j, nd, ip, ix, iy, iz, iz1, jk, jx, jy, jz
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
      allocate(data_sort(fem_T%mesh%node%numnod))
      allocate(inod_sort(fem_T%mesh%node%numnod))
!
!$omp parallel do private(inod)
      do inod = 1, fem_T%mesh%node%internal_node
        data_sort(inod) = fem_T%mesh%node%xx(inod,3)
        inod_sort(inod) = inod
      end do
!$omp end parallel do
!$omp parallel do private(inod)
      do inod = fem_T%mesh%node%internal_node+1, fem_T%mesh%node%numnod
        data_sort(inod) = fem_T%mesh%node%xx(inod,3)
        inod_sort(inod) = inod
      end do
!$omp end parallel do
!
      call quicksort_real_w_index(fem_T%mesh%node%numnod, data_sort,    &
     &    ione, fem_T%mesh%node%internal_node, inod_sort)
!
      allocate(istack_block_z(0:T_meshes%ndivide_eb(3)))
!$omp parallel workshare
      istack_block_z(0:T_meshes%ndivide_eb(3)) = 0
!$omp end parallel workshare
      do inum = 1, fem_T%mesh%node%internal_node-1
        inod = inod_sort(inum)
        jnod = inod_sort(inum+1)
        iz = id_block(inod,3)
        jz = id_block(jnod,3)
        if(iz .ne. jz) istack_block_z(iz:jz) = inum
      end do
      inum = fem_T%mesh%node%internal_node
      inod = inod_sort(inum)
      iz = id_block(inod,3)
      istack_block_z(iz:T_meshes%ndivide_eb(3)) = inum
!
      go to 10
      call check_blocks_4_z_domain                                      &
     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,      &
     &    istack_block_z)
  10  continue
!
!
      allocate(vol_block_gl(T_meshes%ndivide_eb(3)))
      allocate(vol_block_lc(T_meshes%ndivide_eb(3)))
      vol_block_lc(1:T_meshes%ndivide_eb(3)) = 0.0d0
      do inod = 1, fem_T%mesh%node%internal_node
        i = id_block(inod,3)
        vol_block_lc(i) = vol_block_lc(i) + node_volume(inod)
      end do
!
      call calypso_mpi_reduce_real                                      &
     &  (vol_block_lc, vol_block_gl, cast_long(T_meshes%ndivide_eb(3)), &
     &    MPI_SUM, 0)
!
      allocate(istack_vol_z(0:T_meshes%ndomain_eb(3)))
      allocate(vol_grp_z(T_meshes%ndomain_eb(3)))
      if(my_rank .eq. 0) then
        sub_volume = nod_vol_tot / dble(T_meshes%ndomain_eb(3))
!
        vol_ref = 0.0d0
        vol_grp_z(1:T_meshes%ndomain_eb(3)) = 0.0d0
        istack_vol_z(0) = 0
        do i = 1, T_meshes%ndivide_eb(3)
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),T_meshes%ndomain_eb(3))
          istack_vol_z(j) = i
          vol_grp_z(j) = vol_ref
!          write(*,*) i,j, vol_grp_z(j), sub_volume
        end do
!
        do j = T_meshes%ndomain_eb(3), 2, - 1
          vol_grp_z(j) = vol_grp_z(j) - vol_grp_z(j-1)
        end do
      end if
      call calypso_mpi_barrier
!
      if(my_rank .eq. 0) then
        call check_z_divided_volumes                                    &
     &     (T_meshes, nod_vol_tot, sub_volume, istack_vol_z, vol_grp_z)
      end if
      deallocate(vol_block_lc, vol_block_gl)
!
!
      call calypso_mpi_bcast_int                                        &
     &   (istack_vol_z, cast_long(T_meshes%ndomain_eb(3)+1), 0)
      call calypso_mpi_bcast_real                                       &
     &   (vol_grp_z, cast_long(T_meshes%ndomain_eb(3)), 0)
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
      num_nod_grp_z = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        num = z_part_grp%istack_grp(iz) - z_part_grp%istack_grp(iz-1)
        if(num .gt. 0) num_nod_grp_z = num_nod_grp_z + 1
      end do
!
      allocate(idomain_nod_grp_z(num_nod_grp_z+1))
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_z+1
        idomain_nod_grp_z(icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        num = z_part_grp%istack_grp(iz) - z_part_grp%istack_grp(iz-1)
        if(num .gt. 0) then
          icou = icou + 1
          idomain_nod_grp_z(icou) = iz
        end if
      end do
      idomain_nod_grp_z(num_nod_grp_z+1) = T_meshes%ndomain_eb(3) + 1
!
      deallocate(vol_grp_z, istack_vol_z)
!
!   For y direction
!
!$omp parallel do private(inod)
      do inum = 1, fem_T%mesh%node%internal_node
        inod = inod_sort(inum)
        data_sort(inum) = fem_T%mesh%node%xx(inod,2)
      end do
!$omp end parallel do
!$omp parallel do private(inod)
      do inod = fem_T%mesh%node%internal_node+1, fem_T%mesh%node%numnod
        data_sort(inod) = fem_T%mesh%node%xx(inod,2)
      end do
!$omp end parallel do
!
      allocate(istack_block_y(0:T_meshes%ndivide_eb(2),num_nod_grp_z))
!$omp parallel workshare
        istack_block_y(0:T_meshes%ndivide_eb(2),1:num_nod_grp_z) = 0
!$omp end parallel workshare
!
      do icou = 1, num_nod_grp_z
        iz1 = idomain_nod_grp_z(icou)
!      do iz = 1, T_meshes%ndomain_eb(3)
        ist = z_part_grp%istack_grp(iz1-1) + 1
        ied = z_part_grp%istack_grp(iz1)
        if(ied .gt. ist) then
          call quicksort_real_w_index                                   &
     &       (fem_T%mesh%node%numnod, data_sort, ist, ied, inod_sort)
!
        end if
!
!$omp parallel workshare
        istack_block_y(0:T_meshes%ndivide_eb(2),icou)                   &
     &                            = z_part_grp%istack_grp(iz1-1)
!$omp end parallel workshare
        do inum = ist, ied-1
          inod = inod_sort(inum)
          jnod = inod_sort(inum+1)
          iy = id_block(inod,2)
          jy = id_block(jnod,2)
          if(iy .ne. jy) istack_block_y(iy:jy,icou) = inum
        end do
        inod = inod_sort(ied)
        iy = id_block(inod,2)
        istack_block_y(iy:T_meshes%ndivide_eb(2),icou) = ied
      end do
!
      go to 20
      call check_blocks_4_yz_domain                                     &
     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,      &
     &    num_nod_grp_z, idomain_nod_grp_z, istack_block_y)
  20  continue
!
      allocate(vol_block_gl(T_meshes%ndivide_eb(2)))
      allocate(vol_block_lc(T_meshes%ndivide_eb(2)))
!
      do iz = 1, T_meshes%ndomain_eb(3)
        vol_block_lc(1:T_meshes%ndivide_eb(2)) = 0.0d0
        ist = z_part_grp%istack_grp(iz-1) + 1
        ied = z_part_grp%istack_grp(iz)
        do inum = ist, ied
          inod = inod_sort(inum)
          i = id_block(inod,2)
          vol_block_lc(i) = vol_block_lc(i) + node_volume(inod)
        end do
!
        call calypso_mpi_reduce_real(vol_block_lc, vol_block_gl,        &
     &      cast_long(T_meshes%ndivide_eb(2)), MPI_SUM, int(iz-1))
      end do
!
      allocate(istack_vol_y(0:T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3)))
      allocate(vol_grp_y(T_meshes%ndomain_eb(2),T_meshes%ndomain_eb(3)))
      num_group_yz = T_meshes%ndomain_eb(2) * T_meshes%ndomain_eb(3)
!
!      do ip = 1, T_meshes%ndomain_eb(3)
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. T_meshes%ndomain_eb(3)) then
        iz = my_rank + 1
        sub_volume = nod_vol_tot / dble(num_group_yz)
!
        vol_ref = 0.0d0
        vol_grp_y(1:T_meshes%ndomain_eb(2),iz) = 0.0d0
        istack_vol_y(0,iz) = 0
        do i = 1, T_meshes%ndivide_eb(2)
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),T_meshes%ndomain_eb(2))
          istack_vol_y(j,iz) = i
          vol_grp_y(j,iz) = vol_ref
!          write(*,*) i,j, vol_grp_y(j,iz), sub_volume
        end do
!
        do j = T_meshes%ndomain_eb(2),2, - 1
          vol_grp_y(j,iz) = vol_grp_y(j,iz) - vol_grp_y(j-1,iz)
        end do
      end if
!      end if
!      end do
!
      call calypso_mpi_barrier
      do iz = 1, T_meshes%ndomain_eb(3)
        call calypso_mpi_bcast_int(istack_vol_y(0,iz),                  &
     &      cast_long(T_meshes%ndomain_eb(2)+1), int(iz-1))
        call calypso_mpi_bcast_real(vol_grp_y(1,iz),                    &
     &      cast_long(T_meshes%ndomain_eb(2)), int(iz-1))
      end do
!
      if(my_rank .eq. 0) then
        call check_yz_divided_volumes                                   &
     &     (T_meshes, nod_vol_tot, sub_volume, istack_vol_y, vol_grp_y)
      end if
!
      yz_part_grp%num_grp = num_group_yz
      call alloc_group_num(yz_part_grp)
      call set_yz_domain_grp_name                                       &
     &   (T_meshes, yz_part_grp%num_grp, yz_part_grp%grp_name)
      call set_newdomain_grp_stack(T_meshes%ndivide_eb(2),              &
     &    T_meshes%ndomain_eb(2), T_meshes%ndomain_eb(3),               &
     &    num_nod_grp_z, idomain_nod_grp_z, istack_block_y,             &
     &    istack_vol_y, part_grp%num_grp, yz_part_grp%istack_grp)
!      call check_stacks_4_yz_domain(my_rank, fem_T%mesh%node,          &
!     &    T_meshes, inod_sort, num_nod_grp_z, idomain_nod_grp_z,       &
!     &    num_group_yz, yz_part_grp%istack_grp)
!
      deallocate(vol_block_lc, vol_block_gl)
!
!
      num_nod_grp_yz = 0
      do icou = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(icou)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          num = yz_part_grp%istack_grp(jk)                              &
     &         - yz_part_grp%istack_grp(jk-1)
          if(num .gt. 0) num_nod_grp_yz = num_nod_grp_yz + 1
        end do
      end do
!
      allocate(idomain_nod_grp_yz(num_nod_grp_yz+1))
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
          num = yz_part_grp%istack_grp(jk)                              &
     &        - yz_part_grp%istack_grp(jk-1)
          if(num .gt. 0) then
            icou = icou + 1
            idomain_nod_grp_yz(icou)                                    &
     &           = iy + (iz-1) * T_meshes%ndomain_eb(2)
!            idomain_nod_grp_yz(2,icou) = iy
!            idomain_nod_grp_yz(3,icou) = iz
          end if
        end do
      end do
      idomain_nod_grp_yz(num_nod_grp_yz+1) = num_group_yz+1
!      idomain_nod_grp_yz(2,num_nod_grp_yz+1) = T_meshes%ndomain_eb(2)+1
!      idomain_nod_grp_yz(3,num_nod_grp_yz+1) = T_meshes%ndomain_eb(3)
!
!   For x direction
!
!$omp parallel do private(inod)
      do inum = 1, fem_T%mesh%node%internal_node
        inod = inod_sort(inum)
        data_sort(inum) = fem_T%mesh%node%xx(inod,1)
      end do
!$omp end parallel do
!$omp parallel do private(inod)
      do inod = fem_T%mesh%node%internal_node+1, fem_T%mesh%node%numnod
        data_sort(inod) = fem_T%mesh%node%xx(inod,1)
      end do
!$omp end parallel do
!
      allocate(istack_block_x(0:T_meshes%ndivide_eb(1),num_nod_grp_yz))
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(icou)
        ist = yz_part_grp%istack_grp(jk-1) + 1
        ied = yz_part_grp%istack_grp(jk)
        if(ied .gt. ist) then
          call quicksort_real_w_index                                   &
     &       (fem_T%mesh%node%numnod, data_sort, ist, ied, inod_sort)
        end if
!
!$omp parallel workshare
        istack_block_x(0:T_meshes%ndivide_eb(1),icou)                   &
     &                            = yz_part_grp%istack_grp(jk-1)
!$omp end parallel workshare
        do inum = ist, ied-1
          inod = inod_sort(inum)
          jnod = inod_sort(inum+1)
          ix = id_block(inod,1)
          jx = id_block(jnod,1)
          if(ix .ne. jx) istack_block_x(ix:jx,icou) = inum
        end do
        inod = inod_sort(ied)
        ix = id_block(inod,1)
        istack_block_x(ix:T_meshes%ndivide_eb(1),icou) = ied
      end do
!
      go to 30
      call check_blocks_4_xyz_domain                                    &
     &   (my_rank, fem_T%mesh%node, T_meshes, inod_sort, id_block,      &
     &    num_nod_grp_yz, idomain_nod_grp_yz, istack_block_x)
  30  continue
!
!
      allocate(vol_block_gl(T_meshes%ndivide_eb(1)))
      allocate(vol_block_lc(T_meshes%ndivide_eb(1)))
!
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          vol_block_lc(1:T_meshes%ndivide_eb(1)) = 0.0d0
          ist = yz_part_grp%istack_grp(jk-1) + 1
          ied = yz_part_grp%istack_grp(jk  )
          do inum = ist, ied
            inod = inod_sort(inum)
            i = id_block(inod,1)
            vol_block_lc(i) = vol_block_lc(i) + node_volume(inod)
          end do
!
          call calypso_mpi_reduce_real(vol_block_lc, vol_block_gl,      &
     &        cast_long(T_meshes%ndivide_eb(1)), MPI_SUM, int(jk-1))
        end do
      end do
!
      allocate(istack_vol_x(0:T_meshes%ndomain_eb(1),num_group_yz))
      allocate(vol_grp_x(T_meshes%ndomain_eb(1),num_group_yz))
!
!      do ip = 1, num_group_yz
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. num_group_yz) then
        jk = 1 + my_rank
        sub_volume = nod_vol_tot / dble(T_meshes%new_nprocs)
!
        vol_ref = 0.0d0
        vol_grp_x(1:T_meshes%ndomain_eb(1),jk) = 0.0d0
        istack_vol_x(0,jk) = 0
        do i = 1, T_meshes%ndivide_eb(1)
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),T_meshes%ndomain_eb(1))
          istack_vol_x(j,jk) = i
          vol_grp_x(j,jk) = vol_ref
        end do
!
        do j = T_meshes%ndomain_eb(1), 2, - 1
          vol_grp_x(j,jk) = vol_grp_x(j,jk) - vol_grp_x(j-1,jk)
        end do
      end if
!      end if
!      end do
!
      deallocate(vol_block_lc, vol_block_gl)
      deallocate(id_block)
!
      call calypso_mpi_barrier
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          call calypso_mpi_bcast_int(istack_vol_x(0,jk),                &
     &        cast_long(T_meshes%ndomain_eb(1)+1), int(jk-1))
          call calypso_mpi_bcast_real(vol_grp_x(1,jk),                  &
     &        cast_long(T_meshes%ndomain_eb(1)), int(jk-1))
        end do
      end do
!
      if(my_rank .eq. 0) then
        call check_xyz_divided_volumes                                  &
     &     (T_meshes, nod_vol_tot, sub_volume,                          &
     &      num_group_yz, istack_vol_x, vol_grp_x)
      end if
!
!
      part_grp%num_grp = T_meshes%new_nprocs
      call alloc_group_num(part_grp)
      call set_xyz_domain_grp_name                                      &
     &   (T_meshes, part_grp%num_grp, part_grp%grp_name)
      call set_newdomain_grp_stack                                      &
     &   (T_meshes%ndivide_eb(1), T_meshes%ndomain_eb(1), num_group_yz, &
     &    num_nod_grp_yz, idomain_nod_grp_yz, istack_block_x,           &
     &    istack_vol_x, part_grp%num_grp, part_grp%istack_grp)
!      call check_stacks_4_new_domain(my_rank,                          &
!     &    fem_T%mesh%node, T_meshes, inod_sort, num_nod_grp_yz,        &
!     &    idomain_nod_grp_yz, T_meshes%new_nprocs, part_grp%istack_grp)
!
      deallocate(istack_vol_x, vol_grp_x)
      deallocate(istack_block_x)
!
!
!
      part_grp%num_item = part_grp%istack_grp(part_grp%num_grp)
      call alloc_group_item(part_grp)
!
!$omp parallel do private(inum,inod)
      do inum = 1, part_grp%num_item
        inod = inod_sort(inum)
        part_grp%item_grp(inum) = inod
      end do
!$omp end parallel do
!
!$omp parallel do private(i,ist,num)
      do i = 1, part_grp%num_grp
        ist = part_grp%istack_grp(i-1)
        num = part_grp%istack_grp(i  ) - ist
        if(num .gt. 1) then
          call quicksort_int(num, part_grp%item_grp(ist+1), ione, num)
        end if
      end do
!$omp end parallel do
!
      deallocate(idomain_nod_grp_yz)
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
      deallocate(vol_grp_y, istack_vol_y)
      deallocate(node_volume)
      deallocate(data_sort, inod_sort)
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
      end module analyzer_volume_grouping
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
      subroutine check_z_divided_volumes(T_meshes,                      &
     &          nod_vol_tot, sub_volume, istack_vol_z, vol_grp_z)
!
      use t_control_param_vol_grping
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
     &         num_group_yz, istack_vol_x, vol_grp_x)
!
      use t_control_param_vol_grping
!
      type(mesh_test_files_param), intent(in) :: T_meshes
!
      integer(kind = kint), intent(in) :: num_group_yz
      integer(kind = kint), intent(in)                                  &
     &      :: istack_vol_x(0:T_meshes%ndomain_eb(1),num_group_yz)
      real(kind = kreal), intent(in)                                    &
     &      :: vol_grp_x(T_meshes%ndomain_eb(1),num_group_yz)
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
      integer(kind = kint) :: iy, iz, ist, ied, inum, inod
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
      integer(kind = kint) :: ix, iy, iz, jk, ist, ied, inum, inod
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
      integer(kind = kint) :: inum, inod, icou
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
