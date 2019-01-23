!ordering_by_element_group.f90
!      module ordering_by_element_group
!
!      Written by H. Matsui on Oct., 2007
!
!!      subroutine set_local_element_table                              &
!!     &         (numnod, numele, ele_grp, n_domain,                    &
!!     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,&
!!     &          ntot_ele_subdomain, iele_4_subdomain)
!
      module ordering_by_element_group
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable, private :: imark_ele(:)
      private :: s_ordering_by_element_group
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element_table                                &
     &         (numnod, numele, ele_grp, n_domain,                      &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,  &
     &          ntot_ele_subdomain, iele_4_subdomain)
!
      use t_group_data
      use m_ctl_param_partitioner
!
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(inout)                               &
     &                      :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint), intent(in) :: ntot_ele_subdomain
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_4_subdomain(ntot_ele_subdomain)
!
!
      if (nele_grp_ordering .gt. 0) then
        call s_ordering_by_element_group                                &
     &     (numnod, numele, ele_grp, n_domain,                          &
     &      ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,      &
     &      ntot_ele_subdomain, iele_4_subdomain)
      else
        iele_4_subdomain(1:ntot_ele_subdomain)                          &
     &        = iele_near_nod(1:ntot_ele_subdomain)
      end if
!
      end subroutine set_local_element_table
!
!   --------------------------------------------------------------------
!
      subroutine s_ordering_by_element_group                            &
     &         (numnod, numele, ele_grp, n_domain,                      &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod,  &
     &          ntot_ele_subdomain, iele_4_subdomain)
!
      use t_group_data
      use m_ctl_param_partitioner
!
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint), intent(in) :: ntot_ele_subdomain
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_4_subdomain(ntot_ele_subdomain)
!
      integer(kind = kint) :: igrp, ip, ist, ied, inum, iele, icou
      integer(kind = kint) :: jgrp, jst, jed, jnum
!
!
!
      allocate(imark_ele(numele))
      imark_ele(1:numele) = 0
!
!
      do igrp = 1, nele_grp_ordering
        do jgrp = 1, ele_grp%num_grp
          if (ele_grp_ordering(igrp) .eq. ele_grp%grp_name(jgrp)) then
            igrp_ele_ordering(igrp) = jgrp
          end if
        end do
      end do
!
!
      do ip = 1, n_domain
        imark_ele(1:numele) = 0
!
        ist = iele_stack_near_nod(ip-1) + 1
        ied = iele_stack_near_nod(ip)
        do inum = ist, ied
          iele = iele_near_nod(inum)
          imark_ele(iele) = inum
        end do
!
        icou = iele_stack_near_nod(ip-1)
        do igrp = 1, nele_grp_ordering
          jgrp = igrp_ele_ordering(igrp)
          if (jgrp .gt. 0) then
            jst = ele_grp%istack_grp(jgrp-1) + 1
            jed = ele_grp%istack_grp(jgrp)
            do jnum = jst, jed
              iele = ele_grp%item_grp(jnum)
              if(imark_ele(iele) .gt. 0) then
                icou = icou + 1
                iele_4_subdomain(icou) = iele
                inum = imark_ele(iele)
                imark_ele(iele) =     0
                iele_near_nod(inum) = 0
              end if
            end do
          end if
        end do
!
        do inum = ist, ied
          if ( iele_near_nod(inum) .gt. 0) then
            icou = icou + 1
            iele_4_subdomain(icou) = iele_near_nod(inum)
            iele_near_nod(inum) = 0
          end if
        end do
!
      end do
!
      deallocate(imark_ele)
!
      end subroutine s_ordering_by_element_group
!
!   --------------------------------------------------------------------
!
      end module ordering_by_element_group
