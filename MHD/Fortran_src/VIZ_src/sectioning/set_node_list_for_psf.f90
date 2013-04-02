!set_node_list_for_psf.f90
!      module set_node_list_for_psf
!
!        programmed by H.Matsui on june, 2006
!
!      subroutine allocate_work_4_mark_node_psf(numnod)
!      subroutine deallocate_work_4_mark_node_psf
!
!      subroutine mark_node_list_4_psf(numnod, numedge, nnod_4_edge,    &
!     &          ie_edge, nedge_search, istack_e_search_s, iedge_search)
!
!      subroutine count_node_list_4_psf(inod_smp_stack,                 &
!     &          istack_nod_search_s)
!      subroutine set_node_list_4_psf(inod_smp_stack,                   &
!                nnod_search, istack_nod_search_s, inod_search)
!
      module set_node_list_for_psf
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), allocatable:: imark_nod(:)
      integer(kind = kint), allocatable:: imark_nod_smp(:,:)
!
      integer(kind = kint), allocatable:: nnod_search_smp(:)
!
      private :: imark_nod, imark_nod_smp
      private :: nnod_search_smp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_mark_node_psf(numnod)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(imark_nod(numnod))
      allocate(imark_nod_smp(numnod,np_smp))
      allocate(nnod_search_smp(np_smp))
!
      imark_nod(1:numnod) = 0
      nnod_search_smp(1:np_smp) = 0
!
      end subroutine allocate_work_4_mark_node_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_mark_node_psf
!
      deallocate(imark_nod)
      deallocate(imark_nod_smp)
      deallocate(nnod_search_smp)
!
      end subroutine deallocate_work_4_mark_node_psf
!
!  ---------------------------------------------------------------------
!
      subroutine mark_node_list_4_psf(numnod, numedge, nnod_4_edge,     &
     &          ie_edge, nedge_search, istack_e_search_s, iedge_search)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: nedge_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_search(nedge_search)
!
      integer(kind = kint) :: ip, inum, iedge, inod, inod1, inod2
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do
      do inod = 1, numnod
        imark_nod(inod) = 1
      end do
!$omp end parallel do
!$omp parallel do
      do ip = 1, np_smp
        imark_nod_smp(1:numnod,ip) = 1
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,inod1,inod2,iedge,ist,ied)
      do ip = 1, np_smp
        ist = istack_e_search_s(ip-1) + 1
        ied = istack_e_search_s(ip)
        do inum = ist, ied
          iedge = iedge_search(inum)
            inod1 = ie_edge(iedge,1)
            inod2 = ie_edge(iedge,2)
            imark_nod_smp(inod1,ip) = 0
            imark_nod_smp(inod2,ip) = 0
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
!$omp parallel do
        do inod = 1, numnod
          imark_nod(inod) = imark_nod(inod) * imark_nod_smp(inod,ip)
        end do
!$omp end parallel do
      end do
!
!$omp parallel do
      do inod = 1, numnod
        imark_nod(inod) =  (1 - imark_nod(inod))
      end do
!$omp end parallel do
!
      end subroutine mark_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_list_4_psf(inod_smp_stack,                  &
     &          istack_nod_search_s)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(inout)                               &
     &             :: istack_nod_search_s(0:np_smp)
!
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!
      nnod_search_smp(1:np_smp) = 0
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          nnod_search_smp(ip) = nnod_search_smp(ip) + imark_nod(inod)
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_nod_search_s(ip) = istack_nod_search_s(ip-1)             &
     &                            + nnod_search_smp(ip)
      end do
!
      end subroutine count_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_list_4_psf(inod_smp_stack,                    &
                nnod_search, istack_nod_search_s, inod_search)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: nnod_search
      integer(kind = kint), intent(in) :: istack_nod_search_s(0:np_smp)
!
      integer(kind = kint), intent(inout) :: inod_search(nnod_search)
!
      integer(kind = kint) :: ip, inod, ist, ied, icou
!
!$omp parallel do private(inod,ist,ied,icou)
      do ip = 1, np_smp
        icou = istack_nod_search_s(ip-1)
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          if( imark_nod(inod) .gt. 0) then
            icou = icou + 1
            inod_search(icou) = inod
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_node_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_node_list_for_psf
