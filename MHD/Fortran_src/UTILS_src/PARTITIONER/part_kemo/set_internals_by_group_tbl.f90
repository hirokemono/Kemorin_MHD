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
!!      subroutine count_internal_id_by_tbl                             &
!!     &         (n_domain, domain_grp, itl_part)
!!      subroutine set_internal_id_by_tbl                               &
!!     &         (n_domain, domain_grp, itl_part)
!!        type(domain_group_4_partition), intent(in) :: domain_grp
!!        type(internal_4_partitioner), intent(inout) :: itl_part
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
      subroutine count_internal_id_by_tbl                               &
     &         (n_domain, domain_grp, itl_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: domain_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      integer(kind = kint) :: ip, ist, ied, inum, i
!
!
      itl_part%num_inter_sub(1:n_domain) = 0
      do ip = 1, n_domain
        ist = itl_part%istack_4_subdomain(ip-1) + 1
        ied = itl_part%istack_4_subdomain(ip)
        do inum = ist, ied
          i = itl_part%id_4_subdomain(inum)
          if(domain_grp%IGROUP(i) .eq. ip) then
            itl_part%num_inter_sub(ip)                                  &
     &                 = itl_part%num_inter_sub(ip) + 1
          end if
        end do
      end do
!
      end subroutine count_internal_id_by_tbl
!
!   --------------------------------------------------------------------
!
      subroutine set_internal_id_by_tbl                                 &
     &         (n_domain, domain_grp, itl_part)
!
      integer(kind = kint), intent(in) :: n_domain
      type(domain_group_4_partition), intent(in) :: domain_grp
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      integer(kind = kint) :: ip, ist, ied, inum, i, icou
!
!
      do ip = 1, n_domain
        icou = itl_part%istack_inter_sub(ip-1)
        ist = itl_part%istack_4_subdomain(ip-1) + 1
        ied = itl_part%istack_4_subdomain(ip)
        do inum = ist, ied
          i = itl_part%id_4_subdomain(inum)
          if(domain_grp%IGROUP(i) .eq. ip) then
            icou = icou + 1
            itl_part%id_inter_subdomain(icou) = i
          end if
        end do
      end do
!
      end subroutine set_internal_id_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_internals_by_group_tbl
