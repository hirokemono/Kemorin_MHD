!>@file   repart_in_xyz_by_volume.f90
!!@brief  module repart_in_xyz_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct block stack list for re-partitioning
!!
!!@verbatim
!!      subroutine set_z_sorted_node_and_stack(node, id_block,          &
!!     &          nblock_z, istack_block_z, inod_sort)
!!      subroutine set_sorted_node_and_stack(nd, node, id_block,        &
!!     &          num_nod_grp_yz, idomain_nod_grp_yz, nblock_x,         &
!!     &          ndomain_yz, istack_yz_grp, istack_block_x, inod_sort)
!!        type(node_data), intent(in) :: node
!!
!!      subroutine set_z_domain_grp_stack                               &
!!     &         (nblock_z, ndomain_z, istack_block_z, istack_vol_z,    &
!!     &          num_domain_grp, istack_domain_grp)
!!      subroutine set_newdomain_grp_stack                              &
!!     &         (nblock, ndomain_x, ndomain_yz,                        &
!!     &          num_nod_grp, idomain_nod_grp, istack_block,           &
!!     &          istack_volume, num_domain_grp, istack_domain_grp)
!!
!!      subroutine set_domain_grp_item(node, inod_sort,                 &
!!     &          num_domain_grp, num_domain_item,                      &
!!     &          istack_domain_grp, item_domain_grp)
!!        type(node_data), intent(in) :: node
!!@endverbatim
!
      module repart_in_xyz_by_volume
!
      use m_precision
      use m_constants
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_z_sorted_node_and_stack(node, id_block,            &
     &          nblock_z, istack_block_z, inod_sort)
!
      use quicksort
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
      if(node%internal_node .gt. 1) then
        call quicksort_real_w_index(node%numnod, data_sort,             &
     &      ione, node%internal_node, inod_sort)
      end if
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
      use quicksort
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
! ----------------------------------------------------------------------
!
      subroutine set_z_domain_grp_stack                                 &
     &         (nblock_z, ndomain_z, istack_block_z, istack_vol_z,      &
     &          num_domain_grp, istack_domain_grp)
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
      use quicksort
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
!
      end module repart_in_xyz_by_volume
