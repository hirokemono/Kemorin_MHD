!>@file   t_1d_repartitioning_work.f90
!!@brief  module t_1d_repartitioning_work
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
      module t_1d_repartitioning_work
!
      use m_precision
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
!
      end module t_1d_repartitioning_work
