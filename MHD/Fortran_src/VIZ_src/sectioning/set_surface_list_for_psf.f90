!set_surface_list_for_psf.f90
!      module set_surface_list_for_psf
!
!        programmed by H.Matsui on june, 2006
!
!      subroutine allocate_work_4_mark_surf_psf(numsurf)
!      subroutine deallocate_work_4_mark_surf_psf
!
!      subroutine mark_surface_list_4_psf(numele, numsurf, isf_4_ele,   &
!     &          nele_search, istack_e_search_s, iele_search)
!
!      subroutine count_surf_list_4_psf(isurf_smp_stack,                &
!     &          istack_s_search_s)
!      subroutine set_surface_list_4_psf(isurf_smp_stack, nsurf_search, &
!     &          istack_s_search_s, isurf_search)
!
      module set_surface_list_for_psf
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), allocatable:: imark_surf(:)
      integer(kind = kint), allocatable:: imark_surf_smp(:,:)
!
      integer(kind = kint), allocatable:: nsurf_search_smp(:)
!
      private :: imark_surf, nsurf_search_smp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_mark_surf_psf(numsurf)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numsurf
!
!
      allocate(imark_surf(numsurf))
      allocate(imark_surf_smp(numsurf,np_smp))
      allocate(nsurf_search_smp(np_smp))
!
      imark_surf(1:numsurf) = 0
      imark_surf_smp(1:numsurf,1:np_smp) = 0
      nsurf_search_smp(1:np_smp) = 0
!
      end subroutine allocate_work_4_mark_surf_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_mark_surf_psf
!
      deallocate(imark_surf, imark_surf_smp)
      deallocate(nsurf_search_smp)
!
      end subroutine deallocate_work_4_mark_surf_psf
!
!  ---------------------------------------------------------------------
!
      subroutine mark_surface_list_4_psf(numele, numsurf, isf_4_ele,    &
     &          nele_search, istack_e_search_s, iele_search)
!
      use m_machine_parameter
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: nele_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(in) :: iele_search(nele_search)
!
      integer(kind = kint) :: ip, inum, iele, is, isurf, ist, ied
!
!
!$omp parallel do
      do isurf = 1, numsurf
        imark_surf(isurf) = 1
      end do
!$omp end parallel do
!
!$omp parallel do
      do ip = 1, np_smp
        imark_surf_smp(1:numsurf,ip) = 1
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,iele,is,isurf,ist,ied)
      do ip = 1, np_smp
        ist = istack_e_search_s(ip-1) + 1
        ied = istack_e_search_s(ip)
        do inum = ist, ied
          iele = iele_search(inum)
          do is = 1, nsurf_4_ele
            isurf = abs(isf_4_ele(iele,is))
            imark_surf_smp(isurf,ip) = 0
          end do
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
!$omp parallel do
        do isurf = 1, numsurf
          imark_surf(isurf) = imark_surf(isurf)                         &
     &                       * imark_surf_smp(isurf,ip)
        end do
!$omp end parallel do
      end do
!
!$omp parallel do
      do isurf = 1, numsurf
        imark_surf(isurf) = (1 - imark_surf(isurf))
      end do
!$omp end parallel do
!
      end subroutine mark_surface_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_surf_list_4_psf(isurf_smp_stack,                 &
     &          istack_s_search_s)
!
      use m_machine_parameter
!
      integer(kind=kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind=kint), intent(inout) :: istack_s_search_s(0:np_smp)
!
      integer(kind=kint) :: ip, isurf, ist, ied
!
!
      nsurf_search_smp(1:np_smp) = 0
!$omp parallel do private(isurf,ist,ied)
      do ip = 1, np_smp
        ist = isurf_smp_stack(ip-1) + 1
        ied = isurf_smp_stack(ip)
        do isurf = ist, ied
          nsurf_search_smp(ip)                                          &
     &            = nsurf_search_smp(ip) + imark_surf(isurf)
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_s_search_s(ip) = istack_s_search_s(ip-1)                 &
     &                            + nsurf_search_smp(ip)
      end do
!
      end subroutine count_surf_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_list_4_psf(isurf_smp_stack, nsurf_search,  &
     &          istack_s_search_s, isurf_search)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: nsurf_search
      integer(kind = kint), intent(in) :: istack_s_search_s(0:np_smp)
      integer(kind = kint), intent(inout) :: isurf_search(nsurf_search)
!
      integer(kind = kint) :: ip, isurf, ist, ied, icou
!
!$omp parallel do private(isurf,ist,ied,icou)
      do ip = 1, np_smp
        icou = istack_s_search_s(ip-1)
        ist = isurf_smp_stack(ip-1) + 1
        ied = isurf_smp_stack(ip)
        do isurf = ist, ied
          if (imark_surf(isurf) .gt. 0) then
            icou = icou + 1
            isurf_search(icou) = isurf
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_surface_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_surface_list_for_psf
