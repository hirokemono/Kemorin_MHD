!
!      module t_element_list_4_filter
!
!     Written by H. Matsui on Oct., 2006
!
!!      subroutine s_set_element_list_4_filter                          &
!!     &         (ele, ele_grp, gfil_p, fil_elist)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(element_list_4_filter), intent(inout) :: fil_elist
!!      subroutine dealloc_ele_liset_4_filter(fil_elist)
!!      subroutine dealloc_ele_smp_stk_filter(fil_elist)
!!        type(element_list_4_filter), intent(inout) :: fil_elist
!
      module t_element_list_4_filter
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_ctl_params_4_gen_filter
!
      implicit none
!
!
      type element_list_4_filter
        integer(kind = kint) :: nele_4_filter
        integer(kind = kint), allocatable :: iele_4_filter(:)
!
        integer(kind = kint), allocatable :: iele_filter_smp_stack(:)
!>        number of element on this PE
        integer(kind = kint)  ::  maxele_filter_4_smp = 0
      end type element_list_4_filter
!
      integer(kind = kint), allocatable, private :: imark_ele_filter(:)
!
      private :: alloc_ele_list_4_filter, alloc_ele_smp_stk_filter
      private :: allocate_mark_list_4_filter
      private :: deallocate_mark_list_4_filter
      private :: mark_ele_list_4_filter
      private :: count_ele_list_4_filter, set_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_element_list_4_filter                            &
     &         (ele, ele_grp, gfil_p, fil_elist)
!
      use t_group_data
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(element_list_4_filter), intent(inout) :: fil_elist
!
!
      call allocate_mark_list_4_filter(ele)
!
      call mark_ele_list_4_filter(ele, ele_grp, gfil_p)
!
      call count_ele_list_4_filter(ele, fil_elist%nele_4_filter)
!
      call alloc_ele_list_4_filter(fil_elist)
!
      call set_ele_list_4_filter                                        &
     &   (ele, fil_elist%nele_4_filter, fil_elist%iele_4_filter)
!
      call deallocate_mark_list_4_filter
!
!
      call alloc_ele_smp_stk_filter(np_smp, fil_elist)
!
      call count_number_4_smp(np_smp, ione, fil_elist%nele_4_filter,    &
     &    fil_elist%iele_filter_smp_stack,                              &
     &    fil_elist%maxele_filter_4_smp)
!
      end subroutine s_set_element_list_4_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_liset_4_filter(fil_elist)
!
      type(element_list_4_filter), intent(inout) :: fil_elist
!
      deallocate(fil_elist%iele_4_filter)
!
      end subroutine dealloc_ele_liset_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_ele_smp_stk_filter(fil_elist)
!
      type(element_list_4_filter), intent(inout) :: fil_elist
!
      deallocate(fil_elist%iele_filter_smp_stack)
!
      end subroutine dealloc_ele_smp_stk_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_ele_list_4_filter(fil_elist)
!
      type(element_list_4_filter), intent(inout) :: fil_elist
!
      allocate(fil_elist%iele_4_filter(fil_elist%nele_4_filter))
      fil_elist%iele_4_filter = 0
!
      end subroutine alloc_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine alloc_ele_smp_stk_filter(np_smp, fil_elist)
!
      integer(kind = kint), intent(in) :: np_smp
      type(element_list_4_filter), intent(inout) :: fil_elist
!
!
      allocate(fil_elist%iele_filter_smp_stack(0:np_smp) )
      fil_elist%iele_filter_smp_stack = 0
!
      end subroutine alloc_ele_smp_stk_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_mark_list_4_filter(ele)
!
      type(element_data), intent(in) :: ele
!
!
      allocate(imark_ele_filter(ele%numele) )
      imark_ele_filter(1:ele%numele) = 0
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
      subroutine mark_ele_list_4_filter(ele, ele_grp, gfil_p)
!
      use t_group_data
      use skip_comment_f
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      integer(kind = kint) :: i, igrp, inum, ist, ied, iele
!
!
      if (cmp_no_case(gfil_p%filter_area_name(1), 'all')) then
        gfil_p%id_filter_area_grp(1) = -1
        imark_ele_filter(1:ele%numele) = 1
      else
!
        do igrp = 1, gfil_p%num_filtering_grp
          do i = 1, ele_grp%num_grp
            if(gfil_p%filter_area_name(igrp)                            &
     &                 .eq. ele_grp%grp_name(i)) then
              gfil_p%id_filter_area_grp(igrp) = i
              ist = ele_grp%istack_grp(i-1) + 1
              ied = ele_grp%istack_grp(i)
              do inum = ist, ied
                iele = ele_grp%item_grp(inum)
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
      subroutine count_ele_list_4_filter(ele, nele_4_filter)
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(inout) :: nele_4_filter
      integer(kind = kint) :: iele
!
!
      nele_4_filter = 0
      do iele = 1, ele%numele
        nele_4_filter = nele_4_filter + imark_ele_filter(iele)
      end do
!
      end subroutine count_ele_list_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_list_4_filter                                  &
     &         (ele, nele_4_filter, iele_4_filter)
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: nele_4_filter
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_4_filter(nele_4_filter)
!
      integer(kind = kint) :: inum, iele
!
!
      inum = 0
      do iele = 1, ele%numele
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
      end module t_element_list_4_filter
