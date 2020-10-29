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
      integer(kind = kint), allocatable :: idomain_nod_grp_yz(:,:)
      integer(kind = kint), allocatable :: istack_block_x(:,:)
      integer(kind = kint), allocatable :: istack_nod_grp_x(:)
!
      integer(kind = kint), allocatable :: istack_block_y(:,:)
      integer(kind = kint), allocatable :: istack_nod_grp_y(:)
!
      integer(kind = kint), allocatable :: istack_block_z(:)
      integer(kind = kint), allocatable :: istack_nod_grp_z(:)
!
      real(kind = kreal) :: size_gl(3), size_blk(3)
      real(kind = kreal) :: nod_vol_tot, vol_ref, sub_volume
      integer(kind = kint) :: inod, inum, icou
      integer(kind = kint) :: ist, ied, num, jnod, jst, jed, jk1, jk2
      integer(kind = kint) :: i, j, nd, ip, ix, iy, iz, jk, jx, jy, jz
      character(len = kchara) :: chara_tmp
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
      do iz = 1, T_meshes%ndivide_eb(3)
        write(100+my_rank,*) 'istack_block_z', iz, istack_block_z(iz)
        ist = istack_block_z(iz-1) + 1
        ied = istack_block_z(iz)
        do inum = ist, ied
          inod = inod_sort(inum)
          write(100+my_rank,*) 'inod', inum, inod, id_block(inod,3), &
     &                        fem_T%mesh%node%xx(inod,3)
        end do
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
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
!
        write(*,*) 'vol_grp_z'
        do j = 1, T_meshes%ndomain_eb(3)
          write(*,*) j, istack_vol_z(j), vol_grp_z(j)
        end do
      end if
      call calypso_mpi_barrier
!
      deallocate(vol_block_lc, vol_block_gl)
!
!
      call calypso_mpi_bcast_int                                        &
     &   (istack_vol_z, cast_long(T_meshes%ndomain_eb(3)+1), 0)
      call calypso_mpi_bcast_real                                       &
     &   (vol_grp_z, cast_long(T_meshes%ndomain_eb(3)), 0)
!
      allocate(istack_nod_grp_z(0:T_meshes%ndomain_eb(3)))
!$omp parallel workshare
      istack_nod_grp_z(0:T_meshes%ndomain_eb(3)) = 0
!$omp end parallel workshare
!
!$omp parallel do private(inod,inum,ist,ied,jst,jed,j)
      do j = 1, T_meshes%ndomain_eb(3)
        jst = istack_vol_z(j-1) + 1
        jed = istack_vol_z(j)
        ist = istack_block_z(jst-1) + 1
        ied = istack_block_z(jed)
        istack_nod_grp_z(j) = ied
        do inum = ist, ied
          inod = inod_sort(inum)
          id_block(inod,3) = j
        end do
      end do
!$omp end parallel do
!
      go to 110
      do iz = 1, T_meshes%ndomain_eb(3)
        write(100+my_rank,*) 'istack_nod_grp_z', iz, istack_nod_grp_z(iz)
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
  110 continue
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
      allocate(istack_block_y(0:T_meshes%ndivide_eb(2),T_meshes%ndomain_eb(3)))
      do iz = 1, T_meshes%ndomain_eb(3)
        ist = istack_nod_grp_z(iz-1) + 1
        ied = istack_nod_grp_z(iz)
        if(ied .gt. ist) then
          call quicksort_real_w_index                                   &
     &       (fem_T%mesh%node%numnod, data_sort, ist, ied, inod_sort)
!
        end if
!
!$omp parallel workshare
        istack_block_y(0:T_meshes%ndivide_eb(2),iz)                     &
     &                            = istack_nod_grp_z(iz-1)
!$omp end parallel workshare
        do inum = ist, ied-1
          inod = inod_sort(inum)
          jnod = inod_sort(inum+1)
          iy = id_block(inod,2)
          jy = id_block(jnod,2)
          if(iy .ne. jy) istack_block_y(iy:jy,iz) = inum
        end do
        inod = inod_sort(ied)
        iy = id_block(inod,2)
        istack_block_y(iy:T_meshes%ndivide_eb(2),iz) = ied
      end do
!
      go to 20
      do iz = 1, T_meshes%ndomain_eb(3)
        write(100+my_rank,*) 'istack_block_y0', istack_block_z(iz), istack_block_y(0,iz)
        do iy = 1, T_meshes%ndivide_eb(2)
          write(100+my_rank,*) 'istack_block_y', iy, istack_block_y(iy,iz)
          ist = istack_block_y(iy-1,iz) + 1
          ied = istack_block_y(iy,iz)
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod, id_block(inod,2:3), &
     &                        fem_T%mesh%node%xx(inod,2:3)
          end do
        end do
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
  20  continue
!
      allocate(vol_block_gl(T_meshes%ndivide_eb(2)))
      allocate(vol_block_lc(T_meshes%ndivide_eb(2)))
!
      do iz = 1, T_meshes%ndomain_eb(3)
        vol_block_lc(1:T_meshes%ndivide_eb(2)) = 0.0d0
        ist = istack_nod_grp_z(iz-1) + 1
        ied = istack_nod_grp_z(iz)
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

!      do ip = 1, T_meshes%ndomain_eb(3)
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. T_meshes%ndomain_eb(3)) then
        iz = my_rank + 1
        sub_volume = nod_vol_tot                                        &
     &            / dble(T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3))
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
        write(*,*) 'vol_grp_y'
        do iz = 1, T_meshes%ndomain_eb(3)
          do j = 1, T_meshes%ndomain_eb(2)
            write(*,*) j, iz, istack_vol_y(j,iz), vol_grp_y(j,iz)
          end do
        end do
      end if
!
      allocate(istack_nod_grp_y(0:T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)))
!
      istack_nod_grp_y(0) = 0
!$omp parallel do private(inod,inum,ist,ied,jst,jed,j,iz)
      do iz = 1, T_meshes%ndomain_eb(3)
!        istack_nod_grp_y(0:T_meshes%ndomain_eb(2),iz)    &
!     &          = istack_nod_grp_z(iz-1)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          jst = istack_vol_y(iy-1,iz) + 1
          jed = istack_vol_y(iy,iz)
          ist = istack_block_y(jst-1,iz) + 1
          ied = istack_block_y(jed,iz)
          istack_nod_grp_y(jk) = ied
          do inum = ist, ied
            inod = inod_sort(inum)
            id_block(inod,2) = iy
          end do
        end do
      end do
!$omp end parallel do
!
      go to 120
      do iz = 1, T_meshes%ndomain_eb(3)
        jk = (iz-1) * T_meshes%ndomain_eb(2)
        write(100+my_rank,*) 'istack_nod_grp_y0', iz,           &
     &       istack_nod_grp_y(jk), istack_nod_grp_z(iz)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          write(100+my_rank,*) 'istack_nod_grp_y', iy, iz,      &
     &                        istack_nod_grp_y(jk)
        end do
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
  120 continue
!
      deallocate(vol_block_lc, vol_block_gl)
!
!
      num_nod_grp_yz = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          num = istack_nod_grp_y(jk) - istack_nod_grp_y(jk-1)
          if(num .gt. 0) num_nod_grp_yz = num_nod_grp_yz + 1
        end do
      end do
!
!
      allocate(idomain_nod_grp_yz(3,num_nod_grp_yz+1))
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_yz
        idomain_nod_grp_yz(1,icou) = 0
        idomain_nod_grp_yz(2,icou) = 0
        idomain_nod_grp_yz(3,icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          num = istack_nod_grp_y(jk) - istack_nod_grp_y(jk-1)
          if(num .gt. 0) then
            icou = icou + 1
            idomain_nod_grp_yz(1,icou)                                  &
     &           = iy + (iz-1) * T_meshes%ndomain_eb(2)
            idomain_nod_grp_yz(2,icou) = iy
            idomain_nod_grp_yz(3,icou) = iz
          end if
        end do
      end do
      idomain_nod_grp_yz(1,num_nod_grp_yz+1)                            &
     &                = T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)+1
      idomain_nod_grp_yz(2,num_nod_grp_yz+1) = T_meshes%ndomain_eb(2)+1
      idomain_nod_grp_yz(3,num_nod_grp_yz+1) = T_meshes%ndomain_eb(3)
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
        jk = idomain_nod_grp_yz(1,icou)
        iy = idomain_nod_grp_yz(2,icou)
        iz = idomain_nod_grp_yz(3,icou)
        jk2 = idomain_nod_grp_yz(1,icou+1) - 1
!        iy2 = idomain_nod_grp_yz(2,icou+1) - 1
!        iz2 = idomain_nod_grp_yz(3,icou+1)
          ist = istack_nod_grp_y(jk-1) + 1
          ied = istack_nod_grp_y(jk)
          if(ied .gt. ist) then
            call quicksort_real_w_index                                 &
     &         (fem_T%mesh%node%numnod, data_sort, ist, ied, inod_sort)
          end if
!
!$omp parallel workshare
          istack_block_x(0:T_meshes%ndivide_eb(1),icou)                 &
     &                            = istack_nod_grp_y(jk-1)
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
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(1,icou)
        iy = idomain_nod_grp_yz(2,icou)
        iz = idomain_nod_grp_yz(3,icou)
        write(100+my_rank,*) 'istack_block_x0',                         &
     &               istack_block_y(iy,iz), istack_block_x(0,icou)
        do ix = 1, T_meshes%ndivide_eb(2)
          write(100+my_rank,*) 'istack_block_x',                        &
     &                        ix, istack_block_x(ix,icou)
          ist = istack_block_x(ix-1,icou) + 1
          ied = istack_block_x(ix,  icou)
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                 id_block(inod,1:3), fem_T%mesh%node%xx(inod,1:3)
          end do
        end do
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
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
          ist = istack_nod_grp_y(jk-1) + 1
          ied = istack_nod_grp_y(jk  )
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
      allocate(istack_vol_x(0:T_meshes%ndomain_eb(1),T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)))
      allocate(vol_grp_x(T_meshes%ndomain_eb(1),T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)))
!
!      do ip = 1, T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. T_meshes%ndomain_eb(2)*T_meshes%ndomain_eb(3)) then
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
        write(*,*) 'vol_grp_x', nod_vol_tot, sub_volume
        do iz = 1, T_meshes%ndomain_eb(3)
          do iy = 1, T_meshes%ndomain_eb(2)
            jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
            do ix = 1, T_meshes%ndomain_eb(1)
              write(*,*) ix, iy, iz, istack_vol_x(ix,jk), vol_grp_x(ix,jk)
            end do
          end do
        end do
      end if
!
!
      allocate(istack_nod_grp_x(0:T_meshes%new_nprocs))
!$omp parallel workshare
      istack_nod_grp_x(0:T_meshes%new_nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(icou,inod,inum,ist,ied,jst,jed,i,ix,jk,jk1,jk2)
      do icou = 1, num_nod_grp_yz
        jk1 = idomain_nod_grp_yz(1,icou)
        jk2 = idomain_nod_grp_yz(1,icou+1) - 1
        do ix = 1, T_meshes%ndomain_eb(1)
          i = ix + (jk1-1) * T_meshes%ndomain_eb(1)
          jst = istack_vol_x(ix-1,jk1) + 1
          jed = istack_vol_x(ix,  jk1)
          ist = istack_block_x(jst-1,icou) + 1
          ied = istack_block_x(jed,  icou)
          istack_nod_grp_x(i) = ied
        end do
!
        do jk = jk1+1, jk2
          ist = (jk-1) * T_meshes%ndomain_eb(1)
          istack_nod_grp_x(ist+1:ist+T_meshes%ndomain_eb(1))            &
     &        = istack_nod_grp_x(jk1*T_meshes%ndomain_eb(1))
        end do
      end do
!$omp end parallel do
!
      deallocate(istack_vol_x, vol_grp_x)
      deallocate(istack_block_x)
!
      go to 130
      do iz = 1, T_meshes%ndomain_eb(3)
        jk = (iz-1) * T_meshes%ndomain_eb(2)
        write(100+my_rank,*) 'istack_nod_grp_y0', iz,                   &
     &     istack_nod_grp_y(jk), istack_nod_grp_z(iz)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          i = (jk-1) * T_meshes%ndomain_eb(1)
          if(istack_nod_grp_x(i+T_meshes%ndomain_eb(1))                 &
     &       .eq. istack_nod_grp_x(i)) cycle
!
          write(100+my_rank,*) 'istack_nod_grp_x0', iy, iz,             &
     &       istack_nod_grp_x(i), istack_nod_grp_y(jk)
          do ix = 1, T_meshes%ndomain_eb(1)
            i = ix + (jk-1) * T_meshes%ndomain_eb(1)
            write(100+my_rank,*) 'istack_nod_grp_x', ix, iy, iz,        &
     &                          istack_nod_grp_x(i)
            ist = istack_nod_grp_x(i-1) + 1
            ied = istack_nod_grp_x(i  )
            do inum = ist, ied
              inod = inod_sort(inum)
              write(100+my_rank,*) 'inod', inum, inod, ix,              &
     &                id_block(inod,2:3), fem_T%mesh%node%xx(inod,1:3)
            end do
          end do
        end do
      end do
      write(100+my_rank,*) fem_T%mesh%node%internal_node
  130 continue
      deallocate(id_block)
!
!
      part_grp%num_grp = T_meshes%new_nprocs
      call alloc_group_num(part_grp)
!
      part_grp%istack_grp(0) = 0
      do iz = 1, T_meshes%ndomain_eb(3)
        do iy = 1, T_meshes%ndomain_eb(2)
          jk = iy + (iz-1) * T_meshes%ndomain_eb(2)
          do ix = 1, T_meshes%ndomain_eb(1)
            i = ix + (jk-1) * T_meshes%ndomain_eb(1)
            part_grp%istack_grp(i) = istack_nod_grp_x(i)
!
            part_grp%grp_name(i) = 'new_domain'
            write(chara_tmp,'(a,a1)') trim(part_grp%grp_name(i)), '_'
            call add_index_after_name(ix, chara_tmp, part_grp%grp_name(i))
            write(chara_tmp,'(a,a1)') trim(part_grp%grp_name(i)), '_'
            call add_index_after_name(iy, chara_tmp, part_grp%grp_name(i))
            write(chara_tmp,'(a,a1)') trim(part_grp%grp_name(i)), '_'
            call add_index_after_name(iz, chara_tmp, part_grp%grp_name(i))
          end do
        end do
      end do
!
      deallocate(idomain_nod_grp_yz)
      deallocate(istack_nod_grp_y)
      deallocate(istack_nod_grp_x)
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
      do i = 1, part_grp%num_grp
        ist = part_grp%istack_grp(i-1)
        num = part_grp%istack_grp(i  ) - ist
        if(num .gt. 1) then
          call quicksort_int(num, part_grp%item_grp(ist+1), ione, num)
        end if
      end do
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
      deallocate(istack_nod_grp_z)
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
