!ordering_by_element_group.f90
!      module ordering_by_element_group
!
!      Written by H. Matsui on Oct., 2007
!
!!      subroutine set_local_element_table(n_domain,                    &
!!     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
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
      subroutine set_local_element_table(n_domain,                      &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
!
      use m_geometry_parameter
      use m_ctl_param_partitioner
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(inout)                               &
     &                      :: iele_near_nod(ntot_ele_near_nod)
!
!
      if (nele_grp_ordering .gt. 0) then
        call s_ordering_by_element_group(n_domain,                      &
     &      ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
      else
        iele_4_subdomain(1:ntot_numele_sub)                             &
     &        = iele_near_nod(1:ntot_numele_sub)
      end if
!
      end subroutine set_local_element_table
!
!   --------------------------------------------------------------------
!
      subroutine s_ordering_by_element_group(n_domain,                  &
     &          ntot_ele_near_nod, iele_stack_near_nod, iele_near_nod)
!
      use m_ctl_param_partitioner
      use m_geometry_parameter
      use m_element_group
      use m_internal_4_partitioner
!
      integer(kind = kint), intent(in) :: n_domain
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(inout)                               &
     &                      :: iele_near_nod(ntot_ele_near_nod)
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
        do jgrp = 1, ele_grp1%num_grp
          if (ele_grp_ordering(igrp) .eq. ele_grp1%grp_name(jgrp)) then
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
            jst = mat_istack(jgrp-1) + 1
            jed = mat_istack(jgrp)
            do jnum = jst, jed
              iele = mat_item(jnum)
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
