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
      integer(kind = kint), allocatable :: id_vol_z(:)
      real(kind = kreal), allocatable :: vol_grp_z(:)
!
      integer(kind = kint), allocatable :: istack_nod_grp_z(:)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      real(kind = kreal) :: nod_vol_tot, vol_ref, sub_volume
      integer(kind = kint) :: inod, inum, ist, ied
      integer(kind = kint) :: i, j, nd, ip
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
      allocate(id_vol_z(T_meshes%ndivide_eb(3)))
      allocate(vol_grp_z(T_meshes%ndomain_eb(3)))
      if(my_rank .eq. 0) then
        sub_volume = nod_vol_tot / dble(T_meshes%ndomain_eb(3))
!
        vol_ref = 0.0d0
        vol_grp_z(1:T_meshes%ndomain_eb(3)) = 0.0d0
        do i = 1, T_meshes%ndivide_eb(3)
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),T_meshes%ndomain_eb(3))
          id_vol_z(i) = j
          vol_grp_z(j) = vol_ref
!          write(*,*) i,j, vol_grp_z(j), sub_volume
        end do
!
        do j = T_meshes%ndomain_eb(3),2, - 1
          vol_grp_z(j) = vol_grp_z(j) - vol_grp_z(j-1)
        end do
!
        write(*,*) 'vol_grp_z'
        do j = 1, T_meshes%ndomain_eb(3)
          write(*,*) j, vol_grp_z(j)
        end do
      end if
      call calypso_mpi_barrier
!
      deallocate(vol_block_lc, vol_block_gl)
!
      call calypso_mpi_bcast_int                                        &
     &   (id_vol_z, cast_long(T_meshes%ndivide_eb(3)), 0)
      call calypso_mpi_bcast_real                                       &
     &   (vol_grp_z, cast_long(T_meshes%ndomain_eb(3)), 0)
!
!$omp parallel do private(inod)
      do inod = 1,fem_T%mesh%node%numnod
        i = id_block(inod,3)
        id_block(inod,3) = id_vol_z(i)
      end do
!$omp end parallel do
!
      deallocate(vol_grp_z, id_vol_z)
!
!
      allocate(istack_nod_grp_z(0:T_meshes%ndomain_eb(3)))
!$omp parallel workshare
      istack_nod_grp_z(0:T_meshes%ndomain_eb(3)) = 0
!$omp end parallel workshare
      do inum = 1, fem_T%mesh%node%internal_node
        inod = inod_sort(inum)
        i = id_block(inod,3)
        istack_nod_grp_z(i) = inum
      end do
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
      do i = 1, T_meshes%ndomain_eb(3)
        ist = istack_nod_grp_z(i-1) + 1
        ied = istack_nod_grp_z(i)
        if(ied .gt. ist) then
          call quicksort_real_w_index                                   &
     &       (fem_T%mesh%node%numnod, data_sort, ist, ied, inod_sort)
        end if
      end do
!
      allocate(vol_block_gl(T_meshes%ndivide_eb(2)))
      allocate(vol_block_lc(T_meshes%ndivide_eb(2)))
!
      do j = 1, T_meshes%ndomain_eb(3)
        vol_block_lc(1:T_meshes%ndivide_eb(2)) = 0.0d0
        ist = istack_nod_grp_z(j-1) + 1
        ied = istack_nod_grp_z(j)
        do inum = ist, ied
          inod = inod_sort(inum)
          i = id_block(inod,2)
          vol_block_lc(i) = vol_block_lc(i) + node_volume(inod)
        end do
!
        call calypso_mpi_reduce_real(vol_block_lc, vol_block_gl,        &
     &      cast_long(T_meshes%ndivide_eb(2)), MPI_SUM, int(j-1))
      end do
!
      allocate(id_vol_z(T_meshes%ndivide_eb(2)))
      allocate(vol_grp_z(T_meshes%ndomain_eb(2)))

!      do ip = 1, T_meshes%ndomain_eb(3)
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. T_meshes%ndomain_eb(3)) then
        sub_volume = nod_vol_tot                                        &
     &            / dble(T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3))
!
        vol_ref = 0.0d0
        vol_grp_z(1:T_meshes%ndomain_eb(2)) = 0.0d0
        do i = 1, T_meshes%ndivide_eb(2)
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),T_meshes%ndomain_eb(2))
          id_vol_z(i) = j
          vol_grp_z(j) = vol_ref
!          write(*,*) i,j, vol_grp_z(j), sub_volume
        end do
!
        do j = T_meshes%ndomain_eb(2),2, - 1
          vol_grp_z(j) = vol_grp_z(j) - vol_grp_z(j-1)
        end do
!
        write(*,*) 'vol_grp_z'
        do j = 1, T_meshes%ndomain_eb(2)
          write(*,*) my_rank, j, vol_grp_z(j)
        end do
      end if
!      end if
      call calypso_mpi_barrier
!      end do
!
      deallocate(vol_block_lc, vol_block_gl)
      deallocate(vol_grp_z, id_vol_z)
      deallocate(istack_nod_grp_z)
      deallocate(node_volume, id_block)
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
