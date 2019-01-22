!set_subdomain_by_group_tbl.f90
!      module set_subdomain_by_group_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine count_subdomain_nod_by_tbl                           &
!!     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod,          &
!!     &          itl_ele_part, itl_nod_part, imark_nod)
!!      subroutine set_subdomain_nod_by_tbl                             &
!!     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod,          &
!!     &          itl_ele_part, itl_nod_part, imark_nod)
!!        type(internal_4_partitioner), intent(inout) :: itl_nod_part
!!
!!      subroutine count_subdomain_id_by_tbl                            &
!!     &         (numele, nie_4_ele, ie_4_ele, n_domain, num_s_domin,   &
!!     &          itl_ele_part, itl_part, imark)
!!      subroutine set_subdomain_id_by_tbl(numele, nie_4_ele, ie_4_ele, &
!!     &          n_domain, num_s_domin, itl_ele_part, itl_part, imark)
!!        type(internal_4_partitioner), intent(in) :: itl_ele_part
!!        type(internal_4_partitioner), intent(inout) :: itl_part
!
      module set_subdomain_by_group_tbl
!
      use m_precision
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
      subroutine count_subdomain_nod_by_tbl                             &
     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod,            &
     &          itl_ele_part, itl_nod_part, imark_nod)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: nnod_4_subdomain
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_4_subdomain)
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      integer(kind = kint), intent(inout)                               &
     &              :: imark_nod(nnod_4_subdomain)
!
      integer(kind= kint) :: ip, ist, ied, inum, k, iele, inod
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_nod(1:nnod_4_subdomain)= 0
!$omp end parallel workshare
!
        itl_nod_part%num_4_subdomain(ip)                                &
     &      = itl_nod_part%num_inter_sub(ip)

        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          do k = 1, ele%nodelm(iele)
            inod= ele%ie(iele,k)
            if(IGROUP_nod(inod).ne.ip .and. imark_nod(inod).eq.0) then
              itl_nod_part%num_4_subdomain(ip)                          &
     &             = itl_nod_part%num_4_subdomain(ip) + 1
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
      subroutine set_subdomain_nod_by_tbl                               &
     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod,            &
     &          itl_ele_part, itl_nod_part, imark_nod)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: nnod_4_subdomain
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_4_subdomain)
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_nod_part
      integer(kind = kint), intent(inout)                               &
     &              :: imark_nod(nnod_4_subdomain)
!
      integer(kind= kint) :: ip, ist, ied, inum, k, iele, inod
      integer(kind= kint) :: jst, icou
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_nod(1:nnod_4_subdomain)= 0
!$omp end parallel workshare
!
        ist = itl_nod_part%istack_inter_sub(ip-1)
        jst = itl_nod_part%istack_4_subdomain(ip-1)
        do inum = 1, itl_nod_part%num_inter_sub(ip)
          itl_nod_part%id_4_subdomain(inum+jst)                         &
     &          = itl_nod_part%id_inter_subdomain(inum+ist)
        end do

        icou = itl_nod_part%istack_4_subdomain(ip-1)                    &
     &        + itl_nod_part%num_inter_sub(ip)
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          do k = 1, ele%nodelm(iele)
            inod = ele%ie(iele,k)
            if (IGROUP_nod(inod).ne.ip                                  &
     &              .and. imark_nod(inod).eq.0) then
              icou = icou + 1
              itl_nod_part%id_4_subdomain(icou) = inod
              imark_nod(inod) = 1
            end if
          end do
        end do
      end do
!
      end subroutine set_subdomain_nod_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_subdomain_id_by_tbl                              &
     &         (numele, nie_4_ele, ie_4_ele, n_domain, num_s_domin,     &
     &          itl_ele_part, itl_part, imark)
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele, nie_4_ele
      integer(kind = kint), intent(in) :: ie_4_ele(numele,nie_4_ele)
      integer(kind = kint), intent(in) :: num_s_domin
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_part
      integer(kind = kint), intent(inout) :: imark(num_s_domin)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, i, k
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark(1:num_s_domin)= 0
!$omp end parallel workshare
!
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          do k = 1, nie_4_ele
            i = abs( ie_4_ele(iele,k) )
            if (imark(i).eq.0) then
              itl_part%num_4_subdomain(ip)                              &
     &                 = itl_part%num_4_subdomain(ip) + 1
              imark(i) = 1
            end if
          end do
        end do
      end do
!
      end subroutine count_subdomain_id_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_subdomain_id_by_tbl(numele, nie_4_ele, ie_4_ele,   &
     &          n_domain, num_s_domin, itl_ele_part, itl_part, imark)
!
      use t_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele, nie_4_ele
      integer(kind = kint), intent(in) :: ie_4_ele(numele,nie_4_ele)
      integer(kind = kint), intent(in) :: num_s_domin
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(internal_4_partitioner), intent(inout) :: itl_part
      integer(kind = kint), intent(inout) :: imark(num_s_domin)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, i, k, icou
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark(1:num_s_domin) = 0
!$omp end parallel workshare
!
        icou = itl_part%istack_4_subdomain(ip-1)
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele= itl_ele_part%id_4_subdomain(inum)
          do k = 1, nie_4_ele
            i = abs(ie_4_ele(iele,k))
            if (imark(i).eq.0) then
              icou = icou + 1
              itl_part%id_4_subdomain(icou) = i
              imark(i) = 1
            end if
          end do
        end do
      end do
!
      end subroutine set_subdomain_id_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_subdomain_by_group_tbl
