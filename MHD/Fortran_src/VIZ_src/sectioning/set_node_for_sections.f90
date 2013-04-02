!set_node_for_sections.f90
!      module set_node_for_sections
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine count_node_at_node_psf(numnod, c_ref, nnod_search,    &
!     &          istack_n_search_s, inod_search, istack_n_on_n_smp)
!      subroutine set_node_at_node_psf(numnod, c_ref, nnod_search,      &
!     &      istack_n_search_s, inod_search, nnod_on_nod,               &
!     &      istack_n_on_n_smp, inod_4_nod, coef_on_nod, iflag_n_on_n,  &
!     &      id_n_on_n)
!
!      subroutine count_node_on_edge_4_psf(numnod, numedge, nnod_4_edge,&
!     &          ie_edge, c_ref, nedge_search, istack_e_search_s,       &
!     &          iedge_search, istack_n_on_e_smp)
!      subroutine set_node_on_edge_4_psf(numnod, numedge, nnod_4_edge,  &
!     &          ie_edge, c_ref, nedge_search, istack_e_search_s,       &
!     &          iedge_search, istack_n_on_n_smp, nnod_on_edge,         &
!     &          istack_n_on_e_smp, istack_nod_smp, iedge_4_nod,        &
!     &          coef_on_edge, iflag_n_on_e, id_n_on_e)
!
!      subroutine set_nod_on_nod_4_edge_psf(numnod, numedge,            &
!     &          nnod_4_edge, ie_edge, nedge_search,                    &
!     &          istack_e_search_s, iedge_search, istack_nod_smp,       &
!     &          istack_n_on_n_smp, id_n_on_n, id_n_on_e)
!
      module set_node_for_sections
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_at_node_psf(numnod, c_ref, nnod_search,     &
     &          istack_n_search_s, inod_search, istack_n_on_n_smp)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nnod_search
      integer(kind = kint), intent(in) :: istack_n_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: inod_search(nnod_search)
      real(kind= kreal), intent(in) :: c_ref(numnod)
!
      integer(kind=kint), intent(inout) :: istack_n_on_n_smp(0:np_smp)
!
      integer(kind = kint) :: nnod_on_nod_smp(np_smp)
      integer(kind = kint) :: ip, inum, inod, ist, ied
      real(kind= kreal), parameter :: zero = 0.0d0
!
!
      nnod_on_nod_smp(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,inod)
      do ip = 1, np_smp
        ist = istack_n_search_s(ip-1) + 1
        ied = istack_n_search_s(ip)
        do inum = ist, ied
          inod = inod_search(inum)
          if (c_ref(inod) .eq. zero)                                    &
     &                nnod_on_nod_smp(ip) = nnod_on_nod_smp(ip) + 1
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_n_on_n_smp(ip)                                           &
     &             = istack_n_on_n_smp(ip-1) + nnod_on_nod_smp(ip)
      end do
!
      end subroutine count_node_at_node_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_psf(numnod, c_ref, nnod_search,       &
     &      istack_n_search_s, inod_search, nnod_on_nod,                &
     &      istack_n_on_n_smp, inod_4_nod, coef_on_nod, iflag_n_on_n,   &
     &      id_n_on_n)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nnod_search, nnod_on_nod
      integer(kind = kint), intent(in) :: istack_n_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: inod_search(nnod_search)
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      real(kind= kreal), intent(in) :: c_ref(numnod)
!
      integer(kind = kint), intent(inout) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint), intent(inout) :: iflag_n_on_n(numnod)
      integer(kind = kint), intent(inout) :: id_n_on_n(numnod)
      real(kind= kreal), intent(inout) :: coef_on_nod(nnod_on_nod)
!
!
      integer(kind = kint) :: ip, ist,ied,inum,inod,icou
!
!
      iflag_n_on_n(1:numnod) = izero
      id_n_on_n(1:numnod) = izero
!
!$omp parallel do private(ist,ied,inum,inod,icou)
      do ip = 1, np_smp
        icou = istack_n_on_n_smp(ip-1)
        ist = istack_n_search_s(ip-1) + 1
        ied = istack_n_search_s(ip)
        do inum = ist, ied
          inod = inod_search(inum)
          if (c_ref(inod) .eq. zero) then
            icou = icou + 1
            inod_4_nod(icou) = inod
            coef_on_nod(icou) = one
            iflag_n_on_n(inod) = ione
            id_n_on_n(inod) = icou
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_node_at_node_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_node_on_edge_4_psf(numnod, numedge, nnod_4_edge, &
     &          ie_edge, c_ref, nedge_search, istack_e_search_s,        &
     &          iedge_search, istack_n_on_e_smp)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: nedge_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_search(nedge_search)
      real(kind= kreal), intent(in) :: c_ref(numnod)
!
      integer(kind=kint), intent(inout) :: istack_n_on_e_smp(0:np_smp)
!
      integer(kind = kint) :: nnod_on_edge_smp(np_smp)
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum
      real(kind= kreal) :: c0
!
!
      nnod_on_edge_smp(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,iedge,inod1,inod2,c0)
      do ip = 1, np_smp
        ist = istack_e_search_s(ip-1) + 1
        ied = istack_e_search_s(ip)
        do inum = ist, ied
          iedge = iedge_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          c0 = c_ref(inod1) * c_ref(inod2)
          if ( c0 .lt. zero) then
            nnod_on_edge_smp(ip) = nnod_on_edge_smp(ip) + 1
          end if
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_n_on_e_smp(ip)                                           &
     &             = istack_n_on_e_smp(ip-1) + nnod_on_edge_smp(ip)
      end do
!
      end subroutine count_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_on_edge_4_psf(numnod, numedge, nnod_4_edge,   &
     &          ie_edge, c_ref, nedge_search, istack_e_search_s,        &
     &          iedge_search, istack_n_on_n_smp, nnod_on_edge,          &
     &          istack_n_on_e_smp, istack_nod_smp, iedge_4_nod,         &
     &          coef_on_edge, iflag_n_on_e, id_n_on_e)
!
      use m_machine_parameter
      use m_constants
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: nedge_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_search(nedge_search)
      integer(kind = kint), intent(in) :: nnod_on_edge
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: istack_n_on_e_smp(0:np_smp)
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      real(kind= kreal), intent(in) :: c_ref(numnod)
!
      integer(kind = kint), intent(inout) :: iedge_4_nod(nnod_on_edge)
      integer(kind = kint), intent(inout) :: iflag_n_on_e(numedge)
      integer(kind = kint), intent(inout) :: id_n_on_e(numedge)
      real(kind= kreal), intent(inout) :: coef_on_edge(nnod_on_edge,2)
!
      integer(kind = kint) :: ip, icou, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum, num
      real(kind= kreal) :: c0, diff
!
!
      iflag_n_on_e(1:numedge) = izero
      id_n_on_e(1:numedge) =    izero
!
!$omp parallel do private(ist,ied,inum,icou,iedge,inod1,inod2,c0,diff)
      do ip = 1, np_smp
        icou = istack_n_on_e_smp(ip-1)
        ist = istack_e_search_s(ip-1) + 1
        ied = istack_e_search_s(ip)
        do inum = ist, ied
          iedge = iedge_search(inum)
          inod1 = abs( ie_edge(iedge,1) )
          inod2 = abs( ie_edge(iedge,2) )
          c0 = c_ref(inod1) * c_ref(inod2)
          if ( c0 .lt. zero) then
            icou = icou + 1
            iedge_4_nod(icou) = iedge
            diff = c_ref(inod2)-c_ref(inod1)
!
            coef_on_edge(icou,1) =  c_ref(inod2) / diff
            coef_on_edge(icou,2) = -c_ref(inod1) / diff
!
            iflag_n_on_e(iedge) = ione
          end if
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(num,inum,icou,iedge)
      do ip = 1, np_smp
        num = istack_n_on_e_smp(ip) - istack_n_on_e_smp(ip-1)
        do inum = 1, num
          icou = inum + istack_n_on_e_smp(ip-1)
          iedge = iedge_4_nod(icou)
          id_n_on_e(iedge) = inum                                       &
     &                      + istack_n_on_n_smp(ip)                     &
     &                      - istack_n_on_n_smp(ip-1)                   &
     &                      + istack_nod_smp(ip-1)
        end do
      end do
!$omp end parallel do
!
!       write(40+my_rank,*) 'inum_e, id_n_on_e(inum)'
!      do inum = 1, numedge
!         if ( id_n_on_e(inum).ne.0 )                                   &
!     &     write(40+my_rank,*) inum, id_n_on_e(inum)
!      end do
!
      end subroutine set_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_on_nod_4_edge_psf(numnod, numedge,             &
     &          nnod_4_edge, ie_edge, nedge_search,                     &
     &          istack_e_search_s, iedge_search, istack_nod_smp,        &
     &          istack_n_on_n_smp, id_n_on_n, id_n_on_e)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: nedge_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_search(nedge_search)
      integer(kind = kint), intent(in) :: id_n_on_n(numnod)
!
      integer(kind = kint), intent(inout) :: id_n_on_e(numedge)
!
      integer(kind = kint) :: ip, ist, ied, iedge, jp
      integer(kind = kint) :: inod1, inod2, inum
!
!
!$omp parallel do private(ist,ied,inum,iedge,inod1,inod2,jp)
      do ip = 1, np_smp
        ist = istack_e_search_s(ip-1) + 1
        ied = istack_e_search_s(ip)
        do inum = ist, ied
          iedge = iedge_search(inum)
          inod1 = ie_edge(iedge,1)
!
          do jp = 1, np_smp
            if (    id_n_on_n(inod1) .gt. istack_n_on_n_smp(jp-1)       &
     &        .and. id_n_on_n(inod1) .le. istack_n_on_n_smp(jp  )) then
              id_n_on_e(iedge) = id_n_on_n(inod1)                       &
     &                          - istack_n_on_n_smp(jp-1)               &
     &                          + istack_nod_smp(jp-1)
!              write(40+my_rank,*) ip, jp, iedge, id_n_on_e(iedge),     &
!     &              id_n_on_n(inod1), istack_nod_smp(jp-1)
            end if
          end do
!
        end do
!
        do inum = ist, ied
          iedge = iedge_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
!
          do jp = 1, np_smp
            if (    id_n_on_n(inod1).eq.izero                           &
     &        .and. id_n_on_n(inod2) .gt. istack_n_on_n_smp(jp-1)       &
     &        .and. id_n_on_n(inod2) .le. istack_n_on_n_smp(jp  )) then
              id_n_on_e(iedge) = id_n_on_n(inod2)                       &
     &                          - istack_n_on_n_smp(jp-1)               &
     &                          + istack_nod_smp(jp-1)
!              write(40+my_rank,*) ip, jp, iedge, id_n_on_e(iedge),     &
!     &              id_n_on_n(inod2), istack_nod_smp(jp-1)
            end if
          end do
!
        end do
!
      end do
!$omp end parallel do
!
!       write(40+my_rank,*) 'inum_n, id_n_on_e(inum)'
!      do inum = 1, numedge
!         if ( id_n_on_e(inum).ne.0 ) write(40+my_rank,*)               &
!     &                                inum, id_n_on_e(inum)
!      end do
!
      end subroutine set_nod_on_nod_4_edge_psf
!
!  ---------------------------------------------------------------------
!
      end module set_node_for_sections
