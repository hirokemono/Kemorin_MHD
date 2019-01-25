!
!     module node_equaly_sectioning
!
!
!     Written by K. Nakajima
!     Modified by H. Matsui
!
!       output: nod_d_grp%IGROUP
!
!!      subroutine equaly_bisection(nnod, inter_nod, xx, nod_d_grp)
!!      subroutine equaly_volume_bisection                              &
!!      &        (nnod, inter_nod, xx, node_volume, tot_vol, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!
!!      subroutine proportionally_bisection                             &
!!     &         (nnod, inter_nod, xx, part_tbl, nod_d_grp)
!!      subroutine proportion_volume_bisection(nnod, inter_nod, xx,     &
!!     &         part_volume, n_volume, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!
!!      subroutine eb_spherical(nnod, inter_nod,                        &
!!     &          radius, colatitude, longitude, nod_d_grp)
!!      subroutine eb_spherical_w_egrp(nnod, inter_nod, num_mat,        &
!!     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,      &
!!     &          inod_ele_grp, radius, colatitude, longitude,          &
!!     &          nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      module node_equaly_sectioning
!
      use m_precision
!
      use m_ctl_param_partitioner
      use t_domain_group_4_partition
      use sort_by_position_4_rcb
      use sort_by_position_4_eb3d
!
      implicit none
!
      type(partitioner_comm_params), private :: part_comm
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine equaly_bisection(nnod, inter_nod, xx, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod) = 0
      nod_d_grp%IGROUP(1:inter_nod) = 1

      call s_sort_by_position_4_eb3d(inter_nod, part_p1%ndivide_eb,     &
     &    nod_d_grp%IGROUP(1), xx(1,1), xx(1,2), xx(1,3),               &
     &    part_comm%VAL, part_comm%IS1)
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine equaly_bisection
!   --------------------------------------------------------------------
!
      subroutine equaly_volume_bisection                                &
      &        (nnod, inter_nod, xx, node_volume, tot_vol, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
      real(kind= kreal), intent(in) :: node_volume(nnod)
      real(kind = kreal), intent(in) :: tot_vol
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      real(kind = kreal), allocatable :: group_v(:)
      integer(kind = kint) :: i, i_grp, num
!
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod) = 0
      nod_d_grp%IGROUP(1:inter_nod) = 1

      call s_sort_by_position_with_volume(                              &
      &   inter_nod, part_p1%ndivide_eb, node_volume, tot_vol,          &
      &   nod_d_grp%IGROUP(1), xx(1,1), xx(1,2), xx(1,3),               &
      &   part_comm%VAL, part_comm%IS1)
!
!     ========= verify partition is equal volume =========
      num = part_p1%ndivide_eb(1) * part_p1%ndivide_eb(2)               &
     &     * part_p1%ndivide_eb(3)
      allocate(group_v(num))
      group_v(:) = 0.0
      do i = 1, nnod
        i_grp = nod_d_grp%IGROUP(i)
        group_v(i_grp) = group_v(i_grp) + node_volume(i)
      end do
      do i = 1, num
        write(*,*) 'group id', i, 'volume: ', group_v(i)
      end do
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine equaly_volume_bisection
!
!   --------------------------------------------------------------------
!
      subroutine proportionally_bisection                               &
     &         (nnod, inter_nod, xx, part_tbl, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
!
      real(kind = kreal), intent(inout) :: part_tbl(num_domain)
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: i, node_grp_cnt(num_domain)
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod) =      0
      nod_d_grp%IGROUP(1:inter_nod) = 1

      call s_sort_by_position_with_ratio                                &
     &   (inter_nod, part_p1%ndivide_eb, part_tbl,                      &
     &    nod_d_grp%IGROUP(1), xx(1,1), xx(1,2), xx(1,3),               &
     &    part_comm%VAL, part_comm%IS1)
! verify partition
      node_grp_cnt(:) = 0
      do i = 1, nnod
        node_grp_cnt(nod_d_grp%IGROUP(i))                               &
     &     = node_grp_cnt(nod_d_grp%IGROUP(i)) + 1
      end do
      write(*,*) 'num of node in group ', node_grp_cnt(:)
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine proportionally_bisection
!
!   --------------------------------------------------------------------
!
      subroutine proportion_volume_bisection(nnod, inter_nod, xx,       &
     &         part_volume, n_volume, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(inout) :: part_volume(num_domain)
      real(kind = kreal), intent(in) :: n_volume(nnod)
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: i, node_grp_cnt(num_domain)
      real(kind = kreal) :: volume_grp_cnt(num_domain)
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod)= 0
      nod_d_grp%IGROUP(1:inter_nod)= 1

      call s_sort_by_position_with_ratio_volume                         &
     &   (inter_nod, part_p1%ndivide_eb, part_volume,                   &
     &    n_volume, nod_d_grp%IGROUP(1), xx(1,1),                       &
     &    xx(1,2), xx(1,3), part_comm%VAL, part_comm%IS1)
! verify partition
      node_grp_cnt(:) = 0
      volume_grp_cnt(:) = 0
      do i = 1, nnod
        node_grp_cnt(nod_d_grp%IGROUP(i))                               &
     &      = node_grp_cnt(nod_d_grp%IGROUP(i)) + 1
        volume_grp_cnt(nod_d_grp%IGROUP(i))                             &
     &      = volume_grp_cnt(nod_d_grp%IGROUP(i)) + n_volume(i)
      end do
      write(*,*) 'num of node in group ', node_grp_cnt(:)
      write(*,*) 'volume of node in group ', volume_grp_cnt(:)
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine proportion_volume_bisection
!
!   --------------------------------------------------------------------
!
      subroutine eb_spherical(nnod, inter_nod,                          &
     &          radius, colatitude, longitude, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod)= 0
      nod_d_grp%IGROUP(1:inter_nod)= 1

      call s_sort_by_position_4_eb3d(inter_nod, part_p1%ndivide_eb,     &
     &    nod_d_grp%IGROUP(1), radius(1), colatitude(1), longitude(1),  &
     &    part_comm%VAL, part_comm%IS1)
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine eb_spherical
!
!   --------------------------------------------------------------------
!
      subroutine eb_spherical_w_egrp(nnod, inter_nod, num_mat,          &
     &          mat_name, ntot_node_ele_grp, inod_stack_ele_grp,        &
     &          inod_ele_grp, radius, colatitude, longitude,            &
     &          nod_d_grp)
!
      integer(kind = kint), intent(in) :: nnod, inter_nod
!
      integer(kind = kint), intent(in) :: num_mat, ntot_node_ele_grp
      integer(kind = kint), intent(in) :: inod_stack_ele_grp(0:num_mat)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_ele_grp(ntot_node_ele_grp)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!C
!C +-----+
!C | EB  |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod)= 0
      nod_d_grp%IGROUP(1:inter_nod)= 1

      call s_sort_by_position_w_grp                                     &
     &   (inter_nod, part_p1%ndivide_eb, num_mat,                       &
     &    mat_name, ntot_node_ele_grp, inod_stack_ele_grp,              &
     &    inod_ele_grp, part_p1%num_egrp_layer, part_p1%grp_layer_name, &
     &    nod_d_grp%IGROUP, radius(1), colatitude(1), longitude(1),     &
     &    part_comm%VAL, part_comm%IS1)
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine eb_spherical_w_egrp
!
!   --------------------------------------------------------------------
!
      end module node_equaly_sectioning
