!const_neighbour_domain.f90
!      module const_neighbour_domain
!
      module const_neighbour_domain
!
!      Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      use m_domain_group_4_partition
      use m_internal_4_partitioner
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_pe(:)
      private :: imark_pe
!
!      subroutine allocate_wk_neib_domain(nproc)
!      subroutine deallocate_wk_neib_domain
!
!      subroutine count_neib_domain_by_node(ip, nproc, num_neib)
!      subroutine set_neib_domain_by_node(ip, nproc, num_neib, id_neib)
!
!      subroutine count_neib_domain_by_ele(ip, nproc, num_neib)
!      subroutine set_neib_domain_by_ele(ip, nproc, num_neib, id_neib)
!
!      subroutine count_neib_domain_by_surf(ip, nproc, num_neib)
!      subroutine set_neib_domain_by_surf(ip, nproc, num_neib, id_neib)
!
!      subroutine count_neib_domain_by_edge(ip, nproc, num_neib)
!      subroutine set_neib_domain_by_edge(ip, nproc, num_neib, id_neib)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_wk_neib_domain(nproc)
!
      integer(kind= kint), intent(in) :: nproc
!
      allocate(imark_pe(0:nproc))
!
      end subroutine allocate_wk_neib_domain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_wk_neib_domain
!
      deallocate(imark_pe)
!
      end subroutine deallocate_wk_neib_domain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_neib_domain_by_node(ip, nproc, num_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, inod, jnod_org, jp
!
!
      imark_pe(0:nproc) = 0
      ist = istack_numnod_sub(ip-1) + num_intnod_sub(ip) + 1
      ied = istack_numnod_sub(ip)
      do inum = ist, ied
        inod = inod_4_subdomain(inum)
        jnod_org = id_glnode_org(inod)
        jp = IGROUP_nod(jnod_org)
        imark_pe(jp) = 1
      end do
!
      num_neib = 0
      do jp = 1, nproc
        num_neib = num_neib + imark_pe(jp)
      end do
!
      end subroutine count_neib_domain_by_node
!
!   --------------------------------------------------------------------
!
      subroutine set_neib_domain_by_node(ip, nproc, num_neib, id_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
      integer(kind= kint) :: ist, ied, inum, inod, jnod_org, jp, icou
!
!
      icou = 0
      imark_pe(0:nproc) = 0
      ist = istack_numnod_sub(ip-1) + num_intnod_sub(ip) + 1
      ied = istack_numnod_sub(ip)
      do inum = ist, ied
        inod = inod_4_subdomain(inum)
        jnod_org = id_glnode_org(inod)
        jp = IGROUP_nod(jnod_org)
        if (imark_pe(jp) .eq. 0) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        end if
      end do
!
      end subroutine set_neib_domain_by_node
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_neib_domain_by_ele(ip, nproc, num_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, iele, jele_org, jp
!
!
      imark_pe(0:nproc) = 0
      ist = istack_numele_sub(ip-1) + 1
      ied = istack_numele_sub(ip)
      do inum = ist, ied
        iele = iele_4_subdomain(inum)
        jele_org = id_glelem_org(iele)
        jp = IGROUP_ele(jele_org)
        if (jp .ne. ip) then
          imark_pe(jp) = 1
        else if (iele .ne. jele_org) then
          imark_pe(jp) = 1
        end if
      end do
!
      num_neib = 0
      do jp = 1, nproc
        num_neib = num_neib + imark_pe(jp)
      end do
!
      end subroutine count_neib_domain_by_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_neib_domain_by_ele(ip, nproc, num_neib, id_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
      integer(kind= kint) :: ist, ied, inum, iele, jele_org, jp, icou
!
!
      icou = 0
      imark_pe(0:nproc) = 0
      ist = istack_numele_sub(ip-1) + 1
      ied = istack_numele_sub(ip)
      do inum = ist, ied
        iele = iele_4_subdomain(inum)
        jele_org = id_glelem_org(iele)
        jp = IGROUP_ele(jele_org)
        if (jp .ne. ip) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        else if (iele .ne. jele_org) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        end if
      end do
!
      end subroutine set_neib_domain_by_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_neib_domain_by_surf(ip, nproc, num_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, isurf, jsurf_org, jp
!
!
      imark_pe(0:nproc) = 0
      ist = istack_numsurf_sub(ip-1) + 1
      ied = istack_numsurf_sub(ip)
      do inum = ist, ied
        isurf = isurf_4_subdomain(inum)
        jsurf_org = id_glsurf_org(isurf)
        jp = IGROUP_surf(jsurf_org)
        if (jp .ne. ip) then
          imark_pe(jp) = 1
        else if (isurf .ne. jsurf_org) then
          imark_pe(jp) = 1
        end if
      end do
!
      num_neib = 0
      do jp = 1, nproc
        num_neib = num_neib + imark_pe(jp)
      end do
!
      end subroutine count_neib_domain_by_surf
!
!   --------------------------------------------------------------------
!
      subroutine set_neib_domain_by_surf(ip, nproc, num_neib, id_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
      integer(kind= kint) :: ist, ied, inum, isurf, jsurf_org, jp, icou
!
!
      icou = 0
      imark_pe(0:nproc) = 0
      ist = istack_numsurf_sub(ip-1) + 1
      ied = istack_numsurf_sub(ip)
      do inum = ist, ied
        isurf = isurf_4_subdomain(inum)
        jsurf_org = id_glsurf_org(isurf)
        jp = IGROUP_surf(jsurf_org)
        if (jp .ne. ip) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        else if (isurf .ne. jsurf_org) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        end if
      end do
!
      end subroutine set_neib_domain_by_surf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_neib_domain_by_edge(ip, nproc, num_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, iedge, jedge_org, jp
!
!
      imark_pe(0:nproc) = 0
      ist = istack_numedge_sub(ip-1) + 1
      ied = istack_numedge_sub(ip)
      do inum = ist, ied
        iedge = iedge_4_subdomain(inum)
        jedge_org = id_gledge_org(iedge)
        jp = IGROUP_edge(jedge_org)
        if (jp .ne. ip) then
          imark_pe(jp) = 1
        else if (iedge .ne. jedge_org) then
          imark_pe(jp) = 1
        end if
      end do
!
      num_neib = 0
      do jp = 1, nproc
        num_neib = num_neib + imark_pe(jp)
      end do
!
      end subroutine count_neib_domain_by_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_neib_domain_by_edge(ip, nproc, num_neib, id_neib)
!
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
      integer(kind= kint) :: ist, ied, inum, iedge, jedge_org, jp, icou
!
!
      icou = 0
      imark_pe(0:nproc) = 0
      ist = istack_numedge_sub(ip-1) + 1
      ied = istack_numedge_sub(ip)
      do inum = ist, ied
        iedge = iedge_4_subdomain(inum)
        jedge_org = id_gledge_org(iedge)
        jp = IGROUP_edge(jedge_org)
        if (jp .ne. ip) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        else if (iedge .ne. jedge_org) then
          icou = icou + 1
          id_neib(icou) = jp
          imark_pe(jp) = 1
        end if
      end do
!
      end subroutine set_neib_domain_by_edge
!
!   --------------------------------------------------------------------
!
      end module const_neighbour_domain
