!set_internals_by_group_tbl.f90
!      module set_internals_by_group_tbl
!
      module set_internals_by_group_tbl
!
!      Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine count_internal_nod_by_tbl(n_domain)
!      subroutine set_internal_nod_by_tbl(n_domain)
!
!      subroutine count_internal_ele_by_tbl(n_domain)
!      subroutine set_internal_ele_by_tbl(n_domain)
!
!      subroutine count_internal_surf_by_tbl(n_domain)
!      subroutine set_internal_surf_by_tbl(n_domain)
!
!      subroutine count_internal_edge_by_tbl(n_domain)
!      subroutine set_internal_edge_by_tbl(n_domain)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_internal_nod_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind= kint) :: inod, ig
!
      num_intnod_sub(1:n_domain) = 0
      do inod = 1, intnod_s_domin
        ig = IGROUP_nod(inod)
        num_intnod_sub(ig) = num_intnod_sub(ig) + 1
      enddo
!
      end subroutine count_internal_nod_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_nod_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind= kint) :: inod, ig, icou
!
      num_intnod_sub(1:n_domain) = 0
      do inod = 1, intnod_s_domin
        ig = IGROUP_nod(inod)
        num_intnod_sub(ig) = num_intnod_sub(ig) + 1
        icou = istack_intnod_sub(ig-1) + num_intnod_sub(ig)
        inod_intnod_sub(icou) = inod
      end do
!
      end subroutine set_internal_nod_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_ele_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, iele
!
!
      num_intele_sub(1:n_domain) = 0
      do ip = 1, n_domain
        ist = istack_numele_sub(ip-1) + 1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          if (IGROUP_ele(iele) .eq. ip) then
            num_intele_sub(ip) = num_intele_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_ele_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_ele_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, icou
!
!
      do ip= 1, n_domain
        icou = istack_intele_sub(ip-1)
        ist = istack_numele_sub(ip-1) + 1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          if (IGROUP_ele(iele) .eq. ip) then
            icou = icou + 1
            iele_intele_sub(icou) = iele
          end if
        end do
      end do
!
      end subroutine set_internal_ele_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_surf_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, isurf
!
!
      num_intsurf_sub(1:n_domain) = 0
      do ip= 1, n_domain
        ist = istack_numsurf_sub(ip-1) + 1
        ied = istack_numsurf_sub(ip)
        do inum = ist, ied
          isurf = isurf_4_subdomain(inum)
          if(surf_d_grp1%IGROUP(isurf) .eq. ip) then
            num_intsurf_sub(ip) = num_intsurf_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_surf_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_surf_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, isurf, icou
!
!
      do ip= 1, n_domain
        icou = istack_intsurf_sub(ip-1)
        ist = istack_numsurf_sub(ip-1) + 1
        ied = istack_numsurf_sub(ip)
        do inum = ist, ied
          isurf = isurf_4_subdomain(inum)
          if(surf_d_grp1%IGROUP(isurf) .eq. ip) then
            icou = icou + 1
            isurf_intsurf_sub(icou) = isurf
          end if
        end do
      end do
!
      end subroutine set_internal_surf_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_internal_edge_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, iedge
!
!
      num_intedge_sub(1:n_domain) = 0
      do ip= 1, n_domain
        ist = istack_numedge_sub(ip-1) + 1
        ied = istack_numedge_sub(ip)
        do inum = ist, ied
          iedge = iedge_4_subdomain(inum)
          if (edge_d_grp1%IGROUP(iedge) .eq. ip) then
            num_intedge_sub(ip) = num_intedge_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_edge_by_tbl(n_domain)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint) :: ip, ist, ied, inum, iedge, icou
!
!
      do ip= 1, n_domain
        icou = istack_intedge_sub(ip-1)
        ist = istack_numedge_sub(ip-1) + 1
        ied = istack_numedge_sub(ip)
        do inum = ist, ied
          iedge = iedge_4_subdomain(inum)
          if (edge_d_grp1%IGROUP(iedge) .eq. ip) then
            icou = icou + 1
            iedge_intedge_sub(icou) = iedge
          end if
        end do
      end do
!
      end subroutine set_internal_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_internals_by_group_tbl
