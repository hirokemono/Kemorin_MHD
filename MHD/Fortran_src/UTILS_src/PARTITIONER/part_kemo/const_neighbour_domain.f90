!const_neighbour_domain.f90
!      module const_neighbour_domain
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine allocate_wk_neib_domain(nproc)
!!      subroutine deallocate_wk_neib_domain
!!
!!      subroutine count_neib_domain_by_node                            &
!!     &         (nod_d_grp, ip, nproc, num_neib)
!!      subroutine set_neib_domain_by_node                              &
!!     &         (nod_d_grp, ip, nproc, num_neib, id_neib)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      module const_neighbour_domain
!
      use m_precision
!
      use t_domain_group_4_partition
      use m_internal_4_partitioner
!
      implicit none
!
      integer(kind=kint), allocatable :: imark_pe(:)
      private :: imark_pe
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
      subroutine count_neib_domain_by_node                              &
     &         (nod_d_grp, ip, nproc, num_neib)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, inod, jp
      integer(kind= kint_gl) :: jnod_org
!
!
      imark_pe(0:nproc) = 0
      ist = istack_numnod_sub(ip-1) + num_intnod_sub(ip) + 1
      ied = istack_numnod_sub(ip)
      do inum = ist, ied
        inod = inod_4_subdomain(inum)
        jnod_org = nod_d_grp%id_global_org(inod)
        jp = nod_d_grp%IGROUP(jnod_org)
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
      subroutine set_neib_domain_by_node                                &
     &         (nod_d_grp, ip, nproc, num_neib, id_neib)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind= kint), intent(in) :: ip, nproc
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
      integer(kind= kint) :: ist, ied, inum, inod, jp, icou
      integer(kind= kint_gl) :: jnod_org
!
!
      icou = 0
      imark_pe(0:nproc) = 0
      ist = istack_numnod_sub(ip-1) + num_intnod_sub(ip) + 1
      ied = istack_numnod_sub(ip)
      do inum = ist, ied
        inod = inod_4_subdomain(inum)
        jnod_org = nod_d_grp%id_global_org(inod)
        jp = nod_d_grp%IGROUP(jnod_org)
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
!
      end module const_neighbour_domain
