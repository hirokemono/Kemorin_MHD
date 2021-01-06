!>@file   set_field_name_w_SGS.f90
!!@brief  module set_field_name_w_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!!@n        modified by H.Matsui on Oct.,  2009
!!@n        modified by H.Matsui on June., 2012
!
!>@brief  Set field names from control data
!!
!!@verbatim
!!      subroutine ordering_fld_w_SGS_by_viz(field_ctl, fld)
!!      subroutine ordering_fld_w_SGS_by_comp_viz(field_ctl, fld)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!@endverbatim
!
      module set_field_name_w_SGS
!
      use m_precision
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine ordering_fld_w_SGS_by_viz(field_ctl, fld)
!
      use add_nodal_fields_ctl
      use set_each_field_name_w_SGS
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
!
      do i = 1, field_ctl%icou
        flag = .FALSE.
        if(flag) cycle
        call set_vector_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_scalar_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_tensor_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine ordering_fld_w_SGS_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_fld_w_SGS_by_comp_viz(field_ctl, fld)
!
      use add_nodal_fields_ctl
      use set_each_field_name_w_SGS
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
!
      flag = .FALSE.
      do i = 1, field_ctl%icou
        if(flag) cycle
        call set_vector_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        if(flag) cycle
        call set_scalar_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        if(flag) cycle
        call set_tensor_field_name_w_SGS(field_ctl%c1_tbl(i),           &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine ordering_fld_w_SGS_by_comp_viz
!
! -----------------------------------------------------------------------
!
      end module set_field_name_w_SGS
