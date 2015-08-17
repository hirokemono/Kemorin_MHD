!set_subdomain_by_group_tbl.f90
!      module set_subdomain_by_group_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine count_subdomain_nod_by_tbl(n_domain)
!      subroutine set_subdomain_nod_by_tbl(n_domain)
!
!      subroutine count_subdomain_surf_by_tbl(n_domain)
!      subroutine set_subdomain_surf_by_tbl(n_domain)
!
!      subroutine count_subdomain_edge_by_tbl(n_domain)
!      subroutine set_subdomain_edge_by_tbl(n_domain)
!
      module set_subdomain_by_group_tbl
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_nod(:)
      integer(kind=kint), allocatable :: imark_surf(:)
      integer(kind=kint), allocatable :: imark_edge(:)
      private :: imark_nod, imark_surf, imark_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_subdomain_nod_by_tbl(n_domain)
!
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind= kint) :: ip, ist, ied, inum, k, iele, inod
!
!
      allocate (imark_nod(nnod_s_domin))
!
      do ip = 1, n_domain
        imark_nod(1:nnod_s_domin)= 0
        numnod_4_subdomain(ip) = num_intnod_sub(ip)

        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          do k = 1, ele1%nodelm(iele)
            inod= ele1%ie(iele,k)
            if (IGROUP_nod(inod).ne.ip .and. imark_nod(inod).eq.0) then
              numnod_4_subdomain(ip) = numnod_4_subdomain(ip) + 1
              imark_nod(inod) = 1
            end if
          end do
        end do
      end do
!
      end subroutine count_subdomain_nod_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_subdomain_nod_by_tbl(n_domain)
!
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind= kint) :: ip, ist, ied, inum, k, iele, inod
      integer(kind= kint) :: jst, icou
!
!
      do ip = 1, n_domain
        imark_nod(1:nnod_s_domin)= 0

        ist = istack_intnod_sub(ip-1)
        jst = istack_numnod_sub(ip-1)
        do inum = 1, num_intnod_sub(ip)
          inod_4_subdomain(inum+jst)= inod_intnod_sub(inum+ist)
        end do

        icou = istack_numnod_sub(ip-1) + num_intnod_sub(ip)
        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          do k = 1, ele1%nodelm(iele)
            inod = ele1%ie(iele,k)
            if (IGROUP_nod(inod).ne.ip .and. imark_nod(inod).eq.0) then
              icou = icou + 1
              inod_4_subdomain(icou) = inod
              imark_nod(inod) = 1
            end if
          end do
        end do
      end do
!
      deallocate(imark_nod)
!
      end subroutine set_subdomain_nod_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_subdomain_surf_by_tbl(n_domain)
!
      use m_geometry_constants
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint) :: ip, ist, ied, inum, iele, isurf, k
!
!
      allocate (imark_surf(nsurf_s_domin))
!
      do ip = 1, n_domain
        imark_surf(1:nsurf_s_domin)= 0
!
        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          do k = 1, nsurf_4_ele
            isurf = abs( isf_4_ele(iele,k) )
            if (imark_surf(isurf).eq.0) then
              numsurf_4_subdomain(ip) = numsurf_4_subdomain(ip) + 1
              imark_surf(isurf) = 1
            end if
          end do
        end do
      end do
!
      end subroutine count_subdomain_surf_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_subdomain_surf_by_tbl(n_domain)
!
      use m_geometry_constants
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint) :: ip, ist, ied, inum, iele, isurf, k, icou
!
!
      do ip = 1, n_domain
        imark_surf(1:nsurf_s_domin)= 0

        icou = istack_numsurf_sub(ip-1)
        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele= iele_4_subdomain(inum)
          do k = 1, nsurf_4_ele
            isurf = abs( isf_4_ele(iele,k) )
            if (imark_surf(isurf).eq.0) then
              icou = icou + 1
              isurf_4_subdomain(icou) = isurf
              imark_surf(isurf) = 1
            end if
          end do
        end do
      end do
!
      deallocate (imark_surf)
!
      end subroutine set_subdomain_surf_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_subdomain_edge_by_tbl(n_domain)
!
      use m_geometry_constants
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint) :: ip, ist, ied, inum, iele, iedge, k
!
!
      allocate (imark_edge(nedge_s_domin))
!
      do ip = 1, n_domain
        imark_edge(1:nedge_s_domin)= 0

        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele = iele_4_subdomain(inum)
          do k = 1, nedge_4_ele
            iedge = abs( iedge_4_ele(iele,k) )
            if (imark_edge(iedge).eq.0) then
              numedge_4_subdomain(ip) = numedge_4_subdomain(ip) + 1
              imark_edge(iedge) = 1
            end if
          end do
        end do
      end do
!
      end subroutine count_subdomain_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_subdomain_edge_by_tbl(n_domain)
!
      use m_geometry_constants
      use m_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint) :: ip, ist, ied, inum, iele, iedge, k, icou
!
!
      do ip = 1, n_domain
        imark_edge(1:nedge_s_domin)= 0

        icou = istack_numedge_sub(ip-1)
        ist = istack_numele_sub(ip-1)+1
        ied = istack_numele_sub(ip)
        do inum = ist, ied
          iele= iele_4_subdomain(inum)
          do k = 1, nedge_4_ele
            iedge = abs( iedge_4_ele(iele,k) )
            if (imark_edge(iedge).eq.0) then
              icou = icou + 1
              iedge_4_subdomain(icou) = iedge
              imark_edge(iedge) = 1
            end if
          end do
        end do
      end do
!
      deallocate (imark_edge)
!
      end subroutine set_subdomain_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_subdomain_by_group_tbl
