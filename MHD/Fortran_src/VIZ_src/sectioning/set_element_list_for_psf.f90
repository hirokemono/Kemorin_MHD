!set_element_list_for_psf.f90
!      module set_element_list_for_psf
!
!        programmed by H.Matsui on june, 2006
!
!      subroutine allocate_work_4_mark_psf
!      subroutine deallocate_work_4_mark_psf
!      subroutine mark_element_list_4_psf(numele, interior_ele,         &
!     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp, &
!     &        ngrp_area, id_ele_grp_psf)
!      subroutine count_element_list_4_psf(iele_smp_stack,              &
!     &          istack_e_search_s)
!
!      subroutine set_element_list_4_psf(iele_smp_stack, nele_search,   &
!     &          istack_e_search_s, iele_search)
!
!
      module set_element_list_for_psf
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), allocatable:: imark_ele(:)
      integer(kind = kint), allocatable:: nele_search_smp(:)
!
      private :: imark_ele, nele_search_smp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_mark_psf(numele)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate(imark_ele(numele))
      allocate(nele_search_smp(np_smp))
!
      imark_ele(1:numele) =       0
      nele_search_smp(1:np_smp) = 0
!
      end subroutine allocate_work_4_mark_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_mark_psf
!
      deallocate(imark_ele)
      deallocate(nele_search_smp)
!
      end subroutine deallocate_work_4_mark_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_element_list_4_psf(numele, interior_ele,          &
     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,  &
     &        ngrp_area, id_ele_grp_psf)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind = kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind = kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
      integer(kind = kint), intent(in) :: ngrp_area
      integer(kind = kint), intent(in) :: id_ele_grp_psf(ngrp_area)
!
      integer(kind = kint) :: i, inum, iele, igrp, ist, ied
!
!
!$omp parallel do
      do iele = 1, numele
        imark_ele(iele) = 0
      end do
!$omp end parallel do
!
      if (id_ele_grp_psf(1) .eq. 0) then
!$omp parallel do private(iele)
        do iele = 1, numele
          imark_ele(iele) = interior_ele(iele)
        end do
!$omp end parallel do
      else
!
        do i = 1, ngrp_area
          igrp = id_ele_grp_psf(i)
          ist = istack_ele_grp(igrp-1) + 1
          ied = istack_ele_grp(igrp)
!$omp parallel do private(iele)
          do inum = ist, ied
            iele = item_ele_grp(inum)
            imark_ele(iele) = interior_ele(iele)
          end do
!$omp end parallel do
        end do
!
      end if
!
      end subroutine mark_element_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_element_list_4_psf(iele_smp_stack,               &
     &          istack_e_search_s)
!
      use m_machine_parameter
!
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind=kint), intent(inout) :: istack_e_search_s(0:np_smp)
      integer(kind=kint) :: ip, ist, ied, iele
!
!
      nele_search_smp(1:np_smp) = 0
!$omp parallel do private(iele,ist,ied)
      do ip = 1, np_smp
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
        do iele = ist, ied
          nele_search_smp(ip) = nele_search_smp(ip) + imark_ele(iele)
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_e_search_s(ip) = istack_e_search_s(ip-1)                 &
     &                           + nele_search_smp(ip)
      end do
!
      end subroutine count_element_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_element_list_4_psf(iele_smp_stack, nele_search,    &
     &          istack_e_search_s, iele_search)
!
      use m_machine_parameter
!
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: nele_search
      integer(kind = kint), intent(in) :: istack_e_search_s(0:np_smp)
      integer(kind = kint), intent(inout) :: iele_search(nele_search)
!
      integer(kind = kint) :: ip, iele, ist, ied, icou
!
!$omp parallel do private(iele,ist,ied,icou)
      do ip = 1, np_smp
        icou = istack_e_search_s(ip-1)
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
        do iele = ist, ied
          if (imark_ele(iele) .gt. 0) then
            icou = icou + 1
            iele_search(icou) = iele
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_element_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module set_element_list_for_psf
