!>@file   external_group_4_new_part.f90
!!@brief  module external_group_4_new_part
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct re-partitioned external node
!!
!!@verbatim
!!      subroutine const_external_grp_4_new_part(idomain_new, node,     &
!!     &          part_param, part_grp, ext_grp)
!!        type(node_data), intent(in) :: node
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(mesh_test_files_param), intent(in) :: part_param
!!        type(group_data), intent(in) :: part_grp
!!        type(group_data), intent(inout) :: ext_grp
!!@endverbatim
!
      module external_group_4_new_part
!
      use m_precision
      use m_constants
      use t_geometry_data
      use t_group_data
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: external_name = 'new_external'
!
      private :: count_external_grp_4_new_part
      private :: set_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_external_grp_4_new_part(idomain_new, node,       &
     &          part_param, part_grp, ext_grp)
!
      use t_control_param_vol_grping
!
      use quicksort
      use set_repartition_group_name
!
      type(node_data), intent(in) :: node
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      type(group_data), intent(inout) :: ext_grp
!
!
      ext_grp%num_grp = part_grp%num_grp
      call alloc_group_num(ext_grp)
!
      call set_xyz_domain_grp_name(part_param, external_name,           &
     &    ext_grp%num_grp, ext_grp%grp_name)
!
!
      call count_external_grp_4_new_part(idomain_new, node,             &
     &    ext_grp%num_grp, ext_grp%num_item, ext_grp%istack_grp)
      call alloc_group_item(ext_grp)
!
      call set_external_grp_4_new_part(idomain_new, node,               &
     &    ext_grp%num_grp, ext_grp%num_item,                            &
     &    ext_grp%istack_grp, ext_grp%item_grp)
!
      end subroutine const_external_grp_4_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_external_grp_4_new_part(idomain_new, node,       &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_ext_grp
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      integer(kind = kint), intent(inout) :: ntot_ext_grp
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_ext_grp(0:num_ext_grp)
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel workshare
      istack_ext_grp(0:num_ext_grp) = 0
!$omp end parallel workshare
!
      do inod = node%internal_node+1, node%numnod
        inum = idomain_new(inod) + 1
        istack_ext_grp(inum) = istack_ext_grp(inum) + 1
      end do
      do inum = 1, num_ext_grp
        istack_ext_grp(inum) = istack_ext_grp(inum)                     &
     &                        + istack_ext_grp(inum-1)
      end do
      ntot_ext_grp = istack_ext_grp(num_ext_grp)
!
      end subroutine count_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      subroutine set_external_grp_4_new_part(idomain_new, node,         &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp,              &
     &          item_ext_grp)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_ext_grp
      integer(kind = kint), intent(in) :: ntot_ext_grp
      integer(kind = kint), intent(in) :: istack_ext_grp(0:num_ext_grp)
!
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
!
      integer(kind = kint), intent(inout) :: item_ext_grp(ntot_ext_grp)
!
!
      integer(kind = kint) :: inum, inod, icou
      integer(kind = kint), allocatable :: icou_grp(:)
!
!
      allocate(icou_grp(num_ext_grp))
!$omp parallel workshare
      icou_grp(1:num_ext_grp) = istack_ext_grp(0:num_ext_grp-1)
!$omp end parallel workshare
!
      do inod = node%internal_node+1, node%numnod
        inum = idomain_new(inod) + 1
        icou_grp(inum) = icou_grp(inum) + 1
        icou = icou_grp(inum)
        item_ext_grp(icou) = inod
      end do
      deallocate(icou_grp)
!
      end subroutine set_external_grp_4_new_part
!
! ----------------------------------------------------------------------
!
      end module external_group_4_new_part
