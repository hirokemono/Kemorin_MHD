!
!      module set_element_list_4_filter
!
      module set_element_list_4_filter
!
!     Written by H. Matsui on Oct., 2006
!
      use m_precision
!
      use m_element_list_4_filter
      use m_geometry_parameter
!
      implicit none
!
      integer(kind = kint), allocatable :: imark_ele_filter(:)
!
      private :: imark_ele_filter
!
      private :: allocate_mark_list_4_filter
      private :: deallocate_mark_list_4_filter
      private :: mark_ele_list_4_filter
      private :: count_ele_list_4_filter, set_ele_list_4_filter
!
!      subroutine s_set_element_list_4_filter
!      subroutine set_ele_id_4_filter_grp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_element_list_4_filter
!
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer(kind = kint), parameter :: ione = 1
!
      call allocate_mark_list_4_filter
!
      call mark_ele_list_4_filter
!
      call count_ele_list_4_filter
!
      call allocate_ele_list_4_filter
!
      call set_ele_list_4_filter
!
      call deallocate_mark_list_4_filter
!
!
      call allocate_ele_smp_stk_filter(np_smp)
!
      call count_number_4_smp( np_smp, ione, nele_4_filter,             &
     &    iele_filter_smp_stack, maxele_filter_4_smp)

!
      end subroutine s_set_element_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_id_4_filter_grp
!
      use m_element_id_4_node
!
!
      call set_grouped_ele_id_4_node(nele_4_filter, iele_4_filter)
!
      end subroutine set_ele_id_4_filter_grp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_mark_list_4_filter
!
      allocate(imark_ele_filter(numele) )
      imark_ele_filter(1:numele) = 0
!
      end subroutine allocate_mark_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_mark_list_4_filter
!
      deallocate(imark_ele_filter)
!
      end subroutine deallocate_mark_list_4_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mark_ele_list_4_filter
!
      use m_element_group
      use m_ctl_params_4_gen_filter
!
      integer(kind = kint) :: i, igrp, inum, ist, ied, iele
!
!
      if (     filter_area_name(1) .eq. 'all'                           &
     &    .or. filter_area_name(1) .eq. 'All'                           &
     &    .or. filter_area_name(1) .eq. 'ALL') then
        id_filter_area_grp(1) = -1
        imark_ele_filter(1:numele) = 1
      else
!
        do igrp = 1, num_filtering_grp
          do i = 1, ele_grp1%num_grp
            if ( filter_area_name(igrp) .eq. mat_name(i) ) then
              id_filter_area_grp(igrp) = i
              ist = mat_istack(i-1) + 1
              ied = mat_istack(i)
              do inum = ist, ied
                iele = mat_item(inum)
                imark_ele_filter(iele) = 1
              end do
            end if
          end do
        end do
!
      end if
!
      end subroutine mark_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine count_ele_list_4_filter
!
!
      integer(kind = kint) :: iele
!
      nele_4_filter = 0
      do iele = 1, numele
        nele_4_filter = nele_4_filter + imark_ele_filter(iele)
      end do
!
      end subroutine count_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_list_4_filter
!
      integer(kind = kint) :: inum, iele
!
!
      inum = 0
      do iele = 1, numele
        if (imark_ele_filter(iele) .eq. 1) then
          inum = inum + 1
          iele_4_filter(inum) = iele
        end if
      end do
!
      end subroutine set_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      end module set_element_list_4_filter
