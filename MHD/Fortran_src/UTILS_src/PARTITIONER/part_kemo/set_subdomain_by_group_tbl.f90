!set_subdomain_by_group_tbl.f90
!      module set_subdomain_by_group_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine count_subdomain_nod_by_tbl                           &
!!     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod, imark_nod)
!!      subroutine set_subdomain_nod_by_tbl                             &
!!     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod, imark_nod)
!!
!!      subroutine count_subdomain_surf_by_tbl                          &
!!     &         (numele, isf_4_ele, n_domain, nsurf_4_subdomain,       &
!!     &          imark_surf)
!!      subroutine set_subdomain_surf_by_tbl                            &
!!     &         (numele, isf_4_ele, n_domain, nsurf_4_subdomain.       &
!!     &          imark_surf)
!!
!!      subroutine count_subdomain_edge_by_tbl                          &
!!     &         (numele, iedge_4_ele, n_domain, nedge_4_subdomain,     &
!!     &          imark_edge)
!!      subroutine set_subdomain_edge_by_tbl                            &
!!     &         (numele, iedge_4_ele, n_domain, nedge_4_subdomain,     &
!!     &          imark_edge)
!
      module set_subdomain_by_group_tbl
!
      use m_precision
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
     &         (ele, n_domain, nnod_4_subdomain, IGROUP_nod, imark_nod)
!
      use t_geometry_data
      use m_internal_4_partitioner
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: nnod_4_subdomain
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_4_subdomain)
!
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
     &          imark_nod)
!
      use t_geometry_data
      use m_internal_4_partitioner
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: nnod_4_subdomain
      integer(kind = kint), intent(in) :: IGROUP_nod(nnod_4_subdomain)
!
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
      subroutine count_subdomain_surf_by_tbl                            &
     &         (numele, isf_4_ele, n_domain, nsurf_4_subdomain,         &
     &          imark_surf)
!
      use m_geometry_constants
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: nsurf_4_subdomain
!
      integer(kind = kint), intent(inout)                               &
     &              :: imark_surf(nsurf_4_subdomain)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, isurf, k
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_surf(1:nsurf_4_subdomain)= 0
!$omp end parallel workshare
!
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          do k = 1, nsurf_4_ele
            isurf = abs( isf_4_ele(iele,k) )
            if (imark_surf(isurf).eq.0) then
              itl_surf_part%num_4_subdomain(ip)                         &
     &            = itl_surf_part%num_4_subdomain(ip) + 1
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
      subroutine set_subdomain_surf_by_tbl                              &
     &         (numele, isf_4_ele, n_domain, nsurf_4_subdomain,         &
     &          imark_surf)
!
      use m_geometry_constants
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: nsurf_4_subdomain
!
      integer(kind = kint), intent(inout)                               &
     &              :: imark_surf(nsurf_4_subdomain)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, isurf, k, icou
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_surf(1:nsurf_4_subdomain)= 0
!$omp end parallel workshare
!
        icou = itl_surf_part%istack_4_subdomain(ip-1)
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele= itl_ele_part%id_4_subdomain(inum)
          do k = 1, nsurf_4_ele
            isurf = abs( isf_4_ele(iele,k) )
            if (imark_surf(isurf).eq.0) then
              icou = icou + 1
              itl_surf_part%id_4_subdomain(icou) = isurf
              imark_surf(isurf) = 1
            end if
          end do
        end do
      end do
!
      end subroutine set_subdomain_surf_by_tbl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_subdomain_edge_by_tbl                            &
     &         (numele, iedge_4_ele, n_domain, nedge_4_subdomain,       &
     &          imark_edge)
!
      use m_geometry_constants
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in)                                  &
     &      :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind = kint), intent(in) :: nedge_4_subdomain
!
      integer(kind = kint), intent(inout)                               &
     &              :: imark_edge(nedge_4_subdomain)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, iedge, k
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_edge(1:nedge_4_subdomain)= 0
!$omp end parallel workshare
!
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele = itl_ele_part%id_4_subdomain(inum)
          do k = 1, nedge_4_ele
            iedge = abs( iedge_4_ele(iele,k) )
            if (imark_edge(iedge).eq.0) then
              itl_edge_part%num_4_subdomain(ip)                         &
     &            = itl_edge_part%num_4_subdomain(ip) + 1
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
      subroutine set_subdomain_edge_by_tbl                              &
     &         (numele, iedge_4_ele, n_domain, nedge_4_subdomain,       &
     &          imark_edge)
!
      use m_geometry_constants
      use m_internal_4_partitioner
      use t_domain_group_4_partition
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in)                                  &
     &      :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind = kint), intent(in) :: nedge_4_subdomain
!
      integer(kind = kint), intent(inout)                               &
     &              :: imark_edge(nedge_4_subdomain)
!
      integer(kind = kint) :: ip, ist, ied, inum, iele, iedge, k, icou
!
!
      do ip = 1, n_domain
!$omp parallel workshare
        imark_edge(1:nedge_4_subdomain)= 0
!$omp end parallel workshare
!
        icou = itl_edge_part%istack_4_subdomain(ip-1)
        ist = itl_ele_part%istack_4_subdomain(ip-1)+1
        ied = itl_ele_part%istack_4_subdomain(ip)
        do inum = ist, ied
          iele= itl_ele_part%id_4_subdomain(inum)
          do k = 1, nedge_4_ele
            iedge = abs(iedge_4_ele(iele,k) )
            if (imark_edge(iedge).eq.0) then
              icou = icou + 1
              itl_edge_part%id_4_subdomain(icou) = iedge
              imark_edge(iedge) = 1
            end if
          end do
        end do
      end do
!
      end subroutine set_subdomain_edge_by_tbl
!
!   --------------------------------------------------------------------
!
      end module set_subdomain_by_group_tbl
