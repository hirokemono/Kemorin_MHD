!set_internals_by_group_tbl.f90
!      module set_internals_by_group_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine count_internal_nod_by_tbl                            &
!!     &         (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!!      subroutine set_internal_nod_by_tbl                              &
!!     &         (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_nod_part
!!
!!      subroutine count_internal_ele_by_tbl                            &
!!     &         (n_domain, ele_d_grp, itl_ele_part)
!!      subroutine set_internal_ele_by_tbl                              &
!!     &         (n_domain, ele_d_grp, itl_ele_part)
!!        type(domain_group_4_partition), intent(in) :: ele_d_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_ele_part
!!
!!      subroutine count_internal_surf_by_tbl                           &
!!     &         (n_domain, surf_d_grp, itl_surf_part)
!!      subroutine set_internal_surf_by_tbl                             &
!!     &         (n_domain, surf_d_grp, itl_surf_part)
!!        type(domain_group_4_partition), intent(in) :: surf_d_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_surf_part
!!
!!      subroutine count_internal_edge_by_tbl                           &
!!     &         (n_domain, edge_d_grp, itl_edge_part)
!!      subroutine set_internal_edge_by_tbl                             &
!!     &         (n_domain, edge_d_grp, itl_edge_part)
!!        type(domain_group_4_partition), intent(in) :: edge_d_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_edge_part
!
      module set_internals_by_group_tbl
!
      use m_precision
      use t_domain_group_4_partition
      use t_internal_4_partitioner
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_internal_nod_by_tbl                              &
     &         (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: intnod_s_domin
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
!
      integer(kind= kint) :: inod, ig
!
      itl_nod_part%num_inter_sub(1:n_domain) = 0
      do inod = 1, intnod_s_domin
        ig = nod_d_grp%IGROUP(inod)
        itl_nod_part%num_inter_sub(ig)                                  &
     &     = itl_nod_part%num_inter_sub(ig) + 1
      end do
!
      end subroutine count_internal_nod_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_nod_by_tbl                                &
     &         (n_domain, intnod_s_domin, nod_d_grp, itl_nod_part)
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: intnod_s_domin
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
!
      integer(kind= kint) :: inod, ig, icou
!
      itl_nod_part%num_inter_sub(1:n_domain) = 0
      do inod = 1, intnod_s_domin
        ig = nod_d_grp%IGROUP(inod)
        itl_nod_part%num_inter_sub(ig)                                  &
     &     = itl_nod_part%num_inter_sub(ig) + 1
        icou = itl_nod_part%istack_inter_sub(ig-1)                      &
     &        + itl_nod_part%num_inter_sub(ig)
        itl_nod_part%id_inter_subdomain(icou) = inod
      end do
!
      end subroutine set_internal_nod_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_ele_by_tbl                              &
     &         (n_domain, ele_d_grp, itl_ele_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: ele_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_ele_part
!
      integer(kind = kint) :: ip, ist, ied, inum, iele
!
!
      itl_ele_part%num_inter_sub(1:n_domain) = 0
      do ip = 1, n_domain
        ist = itl_ele_part%istack_4_subdomain(ip-1) + 1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          if (ele_d_grp%IGROUP(iele) .eq. ip) then
            itl_ele_part%num_inter_sub(ip)                              &
     &         = itl_ele_part%num_inter_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_ele_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_ele_by_tbl                                &
     &         (n_domain, ele_d_grp, itl_ele_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: ele_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_ele_part
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, icou
!
!
      do ip= 1, n_domain
        icou = itl_ele_part%istack_inter_sub(ip-1)
        ist = itl_ele_part%istack_4_subdomain(ip-1) + 1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          if (ele_d_grp%IGROUP(iele) .eq. ip) then
            icou = icou + 1
            itl_ele_part%id_inter_subdomain(icou) = iele
          end if
        end do
      end do
!
      end subroutine set_internal_ele_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_surf_by_tbl                             &
     &         (n_domain, surf_d_grp, itl_surf_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: surf_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_surf_part
!
      integer(kind = kint) :: ip, ist, ied, inum, isurf
!
!
      itl_surf_part%num_inter_sub(1:n_domain) = 0
      do ip= 1, n_domain
        ist = itl_surf_part%istack_4_subdomain(ip-1) + 1
        ied = itl_surf_part%istack_4_subdomain(ip)
        do inum = ist, ied
          isurf = itl_surf_part%id_4_subdomain(inum)
          if(surf_d_grp%IGROUP(isurf) .eq. ip) then
            itl_surf_part%num_inter_sub(ip)                             &
     &          = itl_surf_part%num_inter_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_surf_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_surf_by_tbl                               &
     &         (n_domain, surf_d_grp, itl_surf_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: surf_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_surf_part
!
      integer(kind = kint) :: ip, ist, ied, inum, isurf, icou
!
!
      do ip= 1, n_domain
        icou = itl_surf_part%istack_inter_sub(ip-1)
        ist = itl_surf_part%istack_4_subdomain(ip-1) + 1
        ied = itl_surf_part%istack_4_subdomain(ip)
        do inum = ist, ied
          isurf = itl_surf_part%id_4_subdomain(inum)
          if(surf_d_grp%IGROUP(isurf) .eq. ip) then
            icou = icou + 1
            itl_surf_part%id_inter_subdomain(icou) = isurf
          end if
        end do
      end do
!
      end subroutine set_internal_surf_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_edge_by_tbl                             &
     &         (n_domain, edge_d_grp, itl_edge_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: edge_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_edge_part
!
      integer(kind = kint) :: ip, ist, ied, inum, iedge
!
!
      itl_edge_part%num_inter_sub(1:n_domain) = 0
      do ip= 1, n_domain
        ist = itl_edge_part%istack_4_subdomain(ip-1) + 1
        ied = itl_edge_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iedge = itl_edge_part%id_4_subdomain(inum)
          if (edge_d_grp%IGROUP(iedge) .eq. ip) then
            itl_edge_part%num_inter_sub(ip)                             &
     &                 = itl_edge_part%num_inter_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_edge_by_tbl                               &
     &         (n_domain, edge_d_grp, itl_edge_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: edge_d_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_edge_part
!
      integer(kind = kint) :: ip, ist, ied, inum, iedge, icou
!
!
      do ip= 1, n_domain
        icou = itl_edge_part%istack_inter_sub(ip-1)
        ist = itl_edge_part%istack_4_subdomain(ip-1) + 1
        ied = itl_edge_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iedge = itl_edge_part%id_4_subdomain(inum)
          if (edge_d_grp%IGROUP(iedge) .eq. ip) then
            icou = icou + 1
            itl_edge_part%id_inter_subdomain(icou) = iedge
          end if
        end do
      end do
!
      end subroutine set_internal_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_internals_by_group_tbl
