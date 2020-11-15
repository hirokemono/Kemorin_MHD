!>@file   t_1d_repartitioning_work.f90
!!@brief  module t_1d_repartitioning_work
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Work area for re-partitioning in one direction
!!
!!@verbatim
!!      subroutine alloc_new_domain_list(part_1d)
!!      subroutine alloc_grouping_1d_work                               &
!!     &         (num_prev_grp, n_block, n_domain, part_1d)
!!      subroutine dealloc_grouping_1d_work(part_1d)
!!        type(grouping_1d_work), intent(inout) :: part_1d
!!
!!      subroutine check_z_divided_volumes(part_param, sub_z)
!!      subroutine check_yz_divided_volumes(part_param, sub_y)
!!      subroutine check_xyz_divided_volumes(part_param, sub_x)
!!      subroutine check_blocks_4_z_domain                              &
!!     &         (my_rank, node, part_param, inod_sort, id_block, sub_z)
!!      subroutine check_blocks_4_yz_domain                             &
!!     &         (my_rank, node, part_param, inod_sort, id_block, sub_y)
!!      subroutine check_blocks_4_xyz_domain                            &
!!     &         (my_rank, node, part_param, inod_sort, id_block, sub_x)
!!        type(node_data), intent(in) :: node
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(grouping_1d_work), intent(in) :: sub_z
!!        type(grouping_1d_work), intent(in) :: sub_y
!!        type(grouping_1d_work), intent(in) :: sub_x
!!@endverbatim
!
      module t_1d_repartitioning_work
!
      use m_precision
      use t_control_param_vol_grping
!
      implicit none
!
!>      Work area for re-partitioning in one direction
      type grouping_1d_work
!>        Number of already divided domain in each process
        integer(kind = kint) :: ndomain_done
!>        List of already divided domain in each process
        integer(kind = kint), allocatable :: idomain_done(:)
!
!>        Number of blocks for re-partitioning
        integer(kind = kint) :: n_block
!>        Number of subdomain for re-partitioning
        integer(kind = kint) :: n_domain
!>        Node stacks for re-partitioning
        integer(kind = kint), allocatable :: istack_block(:,:)
!>        group stacks for re-partitioning
        integer(kind = kint), allocatable :: istack_vol(:,:)
!>        Volumes for each subdoamin
        real(kind = kreal), allocatable :: vol_grp(:,:)
      end type grouping_1d_work
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_new_domain_list(part_1d)
!
      type(grouping_1d_work), intent(inout) :: part_1d
!
      allocate(part_1d%idomain_done(part_1d%ndomain_done+1))
!
!$omp parallel workshare
      part_1d%idomain_done(part_1d%ndomain_done+1) = 0
!$omp end parallel workshare
!
      end subroutine alloc_new_domain_list
!
! ----------------------------------------------------------------------
!
      subroutine alloc_grouping_1d_work                                 &
     &         (num_prev_grp, n_block, n_domain, part_1d)
!
      integer(kind = kint), intent(in) :: num_prev_grp
      integer(kind = kint), intent(in) :: n_block, n_domain
      type(grouping_1d_work), intent(inout) :: part_1d
!
      integer(kind = kint) :: i
!
      part_1d%n_block =  n_block
      allocate(part_1d%istack_block(0:n_block,part_1d%ndomain_done))
!
      part_1d%n_domain = n_domain
      allocate(part_1d%istack_vol(0:part_1d%n_domain,num_prev_grp))
      allocate(part_1d%vol_grp(part_1d%n_domain,num_prev_grp))
!
!$omp parallel
      do i = 1, part_1d%ndomain_done
!$omp workshare
        part_1d%istack_block(0:part_1d%n_block,i) = 0
!$omp end workshare
      end do
!$omp end parallel
!
!$omp parallel
      do i = 1, num_prev_grp
!$omp workshare
        part_1d%istack_vol(0:part_1d%n_domain,i) = 0
        part_1d%vol_grp(1:part_1d%n_domain,i) =  0.0d0
!$omp end workshare
      end do
!$omp end parallel
!
      end subroutine alloc_grouping_1d_work
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_grouping_1d_work(part_1d)
!
      type(grouping_1d_work), intent(inout) :: part_1d
!
      deallocate(part_1d%vol_grp, part_1d%istack_vol)
      deallocate(part_1d%istack_block, part_1d%idomain_done)
!
      end subroutine dealloc_grouping_1d_work
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_z_divided_volumes(part_param, sub_z)
!
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_z
!
      integer(kind = kint) :: iz
!
      do iz = 1, part_param%ndomain_eb(3)
        write(*,*) iz, sub_z%istack_vol(iz,1), sub_z%vol_grp(iz,1)
      end do
!
      end subroutine check_z_divided_volumes
!
! ----------------------------------------------------------------------
!
      subroutine check_yz_divided_volumes(part_param, sub_y)
!
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_y
!
      integer(kind = kint) :: iy, iz
!
      do iz = 1, part_param%ndomain_eb(3)
        do iy = 1, part_param%ndomain_eb(2)
          write(*,*) iy, iz,                                            &
     &              sub_y%istack_vol(iy,iz), sub_y%vol_grp(iy,iz)
        end do
      end do
!
      end subroutine check_yz_divided_volumes
!
! ----------------------------------------------------------------------
!
      subroutine check_xyz_divided_volumes(part_param, sub_x)
!
!
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_x
!
      integer(kind = kint) :: ix, iy, iz, jk
!
      do iz = 1, part_param%ndomain_eb(3)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
          do ix = 1, part_param%ndomain_eb(1)
            write(*,*) ix, iy, iz,                                      &
     &                sub_x%istack_vol(ix,jk), sub_x%vol_grp(ix,jk)
          end do
        end do
      end do
!
      end subroutine check_xyz_divided_volumes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_blocks_4_z_domain                                &
     &         (my_rank, node, part_param, inod_sort, id_block, sub_z)
!
      use t_geometry_data
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_z
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint) :: iz, ist, ied, inum, inod
!
      do iz = 1, part_param%ndivide_eb(3)
        write(100+my_rank,*) 'sub_z%istack_block',                      &
     &                      iz, sub_z%istack_block(iz,1)
        ist = sub_z%istack_block(iz-1,1) + 1
        ied = sub_z%istack_block(iz,1)
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
     &         (my_rank, node, part_param, inod_sort, id_block, sub_y)
!
      use t_geometry_data
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_y
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint) :: iy, iz, ist, ied, inum, inod, icou
!
      do icou = 1, sub_y%ndomain_done
        iz = sub_y%idomain_done(icou)
        write(100+my_rank,*) 'sub_y%istack_block',                      &
     &                      sub_y%istack_block(0,icou)
        do iy = 1, part_param%ndivide_eb(2)
          write(100+my_rank,*) 'sub_y%istack_block',                    &
     &                        iy, sub_y%istack_block(iy,icou)
          ist = sub_y%istack_block(iy-1,icou) + 1
          ied = sub_y%istack_block(iy,icou)
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
     &         (my_rank, node, part_param, inod_sort, id_block, sub_x)
!
      use t_geometry_data
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      type(grouping_1d_work), intent(in) :: sub_x
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod,3)
!
      integer(kind = kint) :: ix, iy, iz, jk, ist, ied
      integer(kind = kint) :: inum, inod, icou
!
      do icou = 1, sub_x%ndomain_done
        jk = sub_x%idomain_done(icou)
        iy = 1 + mod(jk-1,part_param%ndomain_eb(2))
        iz = 1 + (jk-1) / part_param%ndomain_eb(2)
        do ix = 1, part_param%ndivide_eb(2)
          write(100+my_rank,*) 'sub_x%istack_block',                    &
     &                        ix, iy, iz, sub_x%istack_block(ix,icou)
          ist = sub_x%istack_block(ix-1,icou) + 1
          ied = sub_x%istack_block(ix,  icou)
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
!
      end module t_1d_repartitioning_work
