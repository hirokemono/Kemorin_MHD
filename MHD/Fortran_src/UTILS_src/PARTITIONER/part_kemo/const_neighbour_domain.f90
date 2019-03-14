!const_neighbour_domain.f90
!      module const_neighbour_domain
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine allocate_wk_neib_domain(num_pe)
!!      subroutine deallocate_wk_neib_domain
!!
!!      subroutine count_neib_domain_by_node                            &
!!     &        (nod_d_grp, itl_nod_part, ip, num_pe, num_neib)
!!      subroutine set_neib_domain_by_node                              &
!!     &        (nod_d_grp, itl_nod_part, ip, num_pe, num_neib, id_neib)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      module const_neighbour_domain
!
      use m_precision
!
      use t_domain_group_4_partition
      use t_internal_4_partitioner
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
      subroutine allocate_wk_neib_domain(num_pe)
!
      integer, intent(in) :: num_pe
!
      allocate(imark_pe(0:num_pe))
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
     &         (nod_d_grp, itl_nod_part, ip, num_pe, num_neib)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
      integer, intent(in) :: num_pe
      integer(kind= kint), intent(in) :: ip
!
      integer(kind= kint), intent(inout) :: num_neib
      integer(kind= kint) :: ist, ied, inum, inod, jp
      integer(kind= kint_gl) :: jnod_org
!
!
      imark_pe(0:num_pe) = 0
      ist = itl_nod_part%istack_4_subdomain(ip-1)                       &
     &     + itl_nod_part%num_inter_sub(ip) + 1
      ied = itl_nod_part%istack_4_subdomain(ip)
      do inum = ist, ied
        inod = itl_nod_part%id_4_subdomain(inum)
        jnod_org = nod_d_grp%id_global_org(inod)
        jp = nod_d_grp%IGROUP(jnod_org)
        imark_pe(jp) = 1
      end do
!
      num_neib = 0
      do jp = 1, num_pe
        num_neib = num_neib + imark_pe(jp)
      end do
!
      end subroutine count_neib_domain_by_node
!
!   --------------------------------------------------------------------
!
      subroutine set_neib_domain_by_node                                &
     &        (nod_d_grp, itl_nod_part, ip, num_pe, num_neib, id_neib)
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
      integer, intent(in) :: num_pe
      integer(kind= kint), intent(in) :: ip
      integer(kind= kint), intent(in) :: num_neib
      integer(kind= kint), intent(inout) :: id_neib(num_neib)
!
      integer(kind= kint) :: ist, ied, inum, inod, jp, icou
      integer(kind= kint_gl) :: jnod_org
!
!
      icou = 0
      imark_pe(0:num_pe) = 0
      ist = itl_nod_part%istack_4_subdomain(ip-1)                       &
     &     + itl_nod_part%num_inter_sub(ip) + 1
      ied = itl_nod_part%istack_4_subdomain(ip)
      do inum = ist, ied
        inod = itl_nod_part%id_4_subdomain(inum)
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
