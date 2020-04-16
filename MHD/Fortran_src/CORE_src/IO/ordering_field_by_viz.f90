!>@file   ordering_field_by_viz.f90
!!@brief  module ordering_field_by_viz
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  Ordering field data by visualization flag
!!
!!@verbatim
!!      subroutine s_ordering_field_by_viz(field_ctl, fld)
!!      subroutine ordering_field_by_comp_viz(field_ctl, fld)
!!      subroutine set_istack_4_nodal_field(fld)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine count_field_4_monitor                                &
!!     &         (fld, num_field_monitor, ntot_comp_monitor)
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module ordering_field_by_viz
!
      use m_precision
      use m_constants
!
      use t_phys_data
!
      implicit  none
!
      private :: mark_vis_checked_fields
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_ordering_field_by_viz(field_ctl, fld)
!
      use t_control_array_character3
      use set_nodal_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i, i0
      logical :: flag
!
!
      fld%num_component = 0
!
      i0 = 0
      do i = 1, fld%num_phys
        if(check_vis_control_flag(field_ctl%c2_tbl(i))) then
          flag = .FALSE.
          call set_vector_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
          call set_scalar_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
          call set_tensor_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
          if(flag) then
            fld%flag_monitor(i0+1)                                      &
     &         = check_monitor_control_flag(field_ctl%c3_tbl(i))
            i0 = i0 + 1
          end if
!
        end if
      end do
      fld%num_phys_viz = i0
!
      do i = 1, fld%num_phys
        flag = mark_vis_checked_fields(fld%num_phys, fld%num_phys_viz,  &
     &        fld%phys_name, field_ctl%c1_tbl(i))
        if(flag) cycle
!
        call set_vector_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
        call set_scalar_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
        call set_tensor_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
        if(flag) then
          fld%flag_monitor(i0+1)                                        &
     &       = check_monitor_control_flag(field_ctl%c3_tbl(i))
          i0 = i0 + 1
        end if
      end do
!
      end subroutine s_ordering_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_comp_viz(field_ctl, fld)
!
      use t_control_array_character3
      use set_nodal_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i, i0
      logical :: flag
!
!
      fld%num_component = 0
!
      i0 = 0
      do i = 1, fld%num_phys
        if(check_vis_control_flag(field_ctl%c2_tbl(i))) then
          flag = .FALSE.
          call set_vector_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
          if(flag) then
            fld%flag_monitor(i0+1)                                      &
     &         = check_monitor_control_flag(field_ctl%c3_tbl(i))
            i0 = i0 + 1
          end if
        end if
      end do
!
      do i = 1, fld%num_phys
        if(check_vis_control_flag(field_ctl%c2_tbl(i))) then
          flag = .FALSE.
          call set_scalar_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
          if(flag) then
            fld%flag_monitor(i0+1)                                      &
     &         = check_monitor_control_flag(field_ctl%c3_tbl(i))
            i0 = i0 + 1
          end if
        end if
      end do
!
      do i = 1, fld%num_phys
        if(check_vis_control_flag(field_ctl%c2_tbl(i))) then
          flag = .FALSE.
          call set_tensor_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
          if(flag) then
            fld%flag_monitor(i0+1)                                      &
     &         = check_monitor_control_flag(field_ctl%c3_tbl(i))
            i0 = i0 + 1
          end if
        end if
      end do
      fld%num_phys_viz = i0
!
      do i = 1, fld%num_phys
        flag = mark_vis_checked_fields(fld%num_phys, fld%num_phys_viz,  &
     &        fld%phys_name, field_ctl%c1_tbl(i))
        if(flag) cycle
!
          call set_vector_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
        if(flag) then
          fld%flag_monitor(i0+1)                                        &
     &       = check_monitor_control_flag(field_ctl%c3_tbl(i))
          i0 = i0 + 1
        end if
      end do
!
      do i = 1, fld%num_phys
        flag = mark_vis_checked_fields(fld%num_phys, fld%num_phys_viz,  &
     &        fld%phys_name, field_ctl%c1_tbl(i))
        if(flag) cycle
!
        call set_scalar_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
        if(flag) then
          fld%flag_monitor(i0+1)                                        &
     &       = check_monitor_control_flag(field_ctl%c3_tbl(i))
          i0 = i0 + 1
        end if
      end do
!
      do i = 1, fld%num_phys
        flag = mark_vis_checked_fields(fld%num_phys, fld%num_phys_viz,  &
     &        fld%phys_name, field_ctl%c1_tbl(i))
        if(flag) cycle
!
        call set_tensor_field_name(i0, field_ctl%c1_tbl(i), fld, flag)
!
        if(flag) then
          fld%flag_monitor(i0+1)                                        &
     &       = check_monitor_control_flag(field_ctl%c3_tbl(i))
          i0 = i0 + 1
        end if
      end do
!
      end subroutine ordering_field_by_comp_viz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_4_nodal_field(fld)
!
      use cal_minmax_and_stacks
!
      type(phys_data), intent(inout) :: fld
!
!
      call s_cal_total_and_stacks(fld%num_phys, fld%num_component,      &
     &    izero, fld%istack_component, fld%ntot_phys)
      fld%ntot_phys_viz = fld%istack_component(fld%num_phys_viz)
!
      end subroutine set_istack_4_nodal_field
!
! -----------------------------------------------------------------------
!
      subroutine count_field_4_monitor                                  &
     &         (fld, num_field_monitor, ntot_comp_monitor)
!
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint), intent(inout) :: num_field_monitor
      integer(kind = kint), intent(inout) :: ntot_comp_monitor
!
      integer(kind = kint) :: i
!
!    count number of components for monitoring
!
      num_field_monitor = 0
      ntot_comp_monitor = 0
      do i = 1, fld%num_phys
        if(fld%flag_monitor(i)) then
          num_field_monitor = num_field_monitor + 1
          ntot_comp_monitor = ntot_comp_monitor + fld%num_component(i)
        end if
      end do
!
      end subroutine count_field_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function mark_vis_checked_fields                          &
     &      (num_phys, num_phys_viz,  phys_name, phys_nod_name_ctl)
!
      integer(kind = kint), intent(in) :: num_phys
      integer(kind = kint), intent(in) :: num_phys_viz
      character (len=kchara), intent(in) :: phys_name(num_phys)
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint) :: i1
!
      mark_vis_checked_fields = .FALSE.
      do i1 = 1, num_phys_viz
        if (phys_nod_name_ctl .eq. phys_name(i1) ) then
          mark_vis_checked_fields = .TRUE.
          exit
        end if
      end do
!
      end function mark_vis_checked_fields
!
! -----------------------------------------------------------------------
!
      end module ordering_field_by_viz
