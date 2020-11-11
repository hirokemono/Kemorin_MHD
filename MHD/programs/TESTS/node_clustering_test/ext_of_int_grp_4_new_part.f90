!>@file   ext_of_int_grp_4_new_part.f90
!!@brief  module ext_of_int_grp_4_new_part
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct re-partitioned external node
!!
!!@verbatim
!!      subroutine const_ext_of_int_grp_new_part                        &
!!     &         (node, neib_nod, part_param, part_grp, ext_int_grp)
!!        type(node_data), intent(in) :: node
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(mesh_test_files_param), intent(in) :: part_param
!!        type(group_data), intent(in) :: part_grp
!!        type(group_data), intent(inout) :: ext_int_grp
!!@endverbatim
!
      module ext_of_int_grp_4_new_part
!
      use m_precision
      use m_constants
      use t_geometry_data
      use t_group_data
      use t_next_node_ele_4_node
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: external_name = 'new_ext_int'
!
      private :: count_ext_of_int_grp_new_part
      private :: set_ext_of_int_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_ext_of_int_grp_new_part(idomain_new, inod_new,   &
     &          node, neib_nod, part_param, part_grp, ext_int_grp)
!
      use t_control_param_vol_grping
!
      use quicksort
      use set_repartition_group_name
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      type(group_data), intent(inout) :: ext_int_grp
!
      integer(kind = kint), allocatable :: iflag_nod(:)
      integer(kind = kint) :: i, ist, num
!
!
      allocate(iflag_nod(node%numnod))
!$omp parallel workshare
      iflag_nod(1:node%numnod) = 1
!$omp end parallel workshare
!
      ext_int_grp%num_grp = part_grp%num_grp
      call alloc_group_num(ext_int_grp)
!
      call set_xyz_domain_grp_name(part_param, external_name,           &
     &    ext_int_grp%num_grp, ext_int_grp%grp_name)
!
!
      call count_ext_of_int_grp_new_part                                &
     &   (idomain_new, inod_new, node, neib_nod, part_grp,              &
     &    ext_int_grp%num_grp, ext_int_grp%num_item,                    &
     &    ext_int_grp%istack_grp, iflag_nod)
      call alloc_group_item(ext_int_grp)
!
      call set_ext_of_int_grp_4_new_part                                &
     &   (idomain_new, inod_new, node, neib_nod, part_grp,              &
     &    ext_int_grp%num_grp, ext_int_grp%num_item,                    &
     &    ext_int_grp%istack_grp, ext_int_grp%item_grp, iflag_nod)
!
!$omp parallel do private(i,ist,num)
      do i = 1, ext_int_grp%num_grp
        ist = ext_int_grp%istack_grp(i-1)
        num = ext_int_grp%istack_grp(i  ) - ist
        if(num .gt. 1) then
          call quicksort_int(num, ext_int_grp%item_grp(ist+1),          &
     &                       ione, num)
        end if
      end do
!$omp end parallel do
!
      deallocate(iflag_nod)
!
      end subroutine const_ext_of_int_grp_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_ext_of_int_grp_new_part                          &
     &         (idomain_new, inod_new, node, neib_nod, part_grp,        &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp, iflag_nod)
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in) :: num_ext_grp
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      integer(kind = kint), intent(inout) :: ntot_ext_grp
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_ext_grp(0:num_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: igrp, icou
!
!
      istack_ext_grp(0) = 0
      do igrp = 1, part_grp%num_grp
!$omp parallel workshare
        iflag_nod(1:node%internal_node) = 1
!$omp end parallel workshare
        if(node%numnod .gt. node%internal_node) then
!$omp parallel workshare
          iflag_nod(node%internal_node+1:node%numnod) = 0
!$omp end parallel workshare
        end if
!
        icou = istack_ext_grp(igrp-1)
        ist = part_grp%istack_grp(igrp-1) + 1
        ied = part_grp%istack_grp(igrp)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          iflag_nod(inod) = 0
        end do
!$omp end parallel do
!
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(jnod .le. node%internal_node                             &
     &          .and. iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              iflag_nod(jnod) = 0
            end if
          end do
        end do
        istack_ext_grp(igrp) = icou
      end do
!
      ntot_ext_grp = istack_ext_grp(part_grp%num_grp)
!
      end subroutine count_ext_of_int_grp_new_part
!
! ----------------------------------------------------------------------
!
      subroutine set_ext_of_int_grp_4_new_part                          &
     &         (idomain_new, inod_new, node, neib_nod, part_grp,        &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp,              &
     &          item_ext_grp, iflag_nod)
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in) :: num_ext_grp
      integer(kind = kint), intent(in) :: ntot_ext_grp
      integer(kind = kint), intent(in) :: istack_ext_grp(0:num_ext_grp)
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      integer(kind = kint), intent(inout) :: item_ext_grp(ntot_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: igrp, icou
!
!
      do igrp = 1, part_grp%num_grp
!$omp parallel workshare
        iflag_nod(1:node%internal_node) = 1
!$omp end parallel workshare
        if(node%numnod .gt. node%internal_node) then
!$omp parallel workshare
          iflag_nod(node%internal_node+1:node%numnod) = 0
!$omp end parallel workshare
        end if
!
        icou = istack_ext_grp(igrp-1)
        ist = part_grp%istack_grp(igrp-1) + 1
        ied = part_grp%istack_grp(igrp)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          iflag_nod(inod) = 0
        end do
!$omp end parallel do
!
        do inum = ist, ied
          inod = part_grp%item_grp(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(jnod .le. node%internal_node                             &
     &          .and. iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              item_ext_grp(icou) = jnod
              iflag_nod(jnod) =    0
            end if
          end do
        end do
      end do
!
      end subroutine set_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      end module ext_of_int_grp_4_new_part
