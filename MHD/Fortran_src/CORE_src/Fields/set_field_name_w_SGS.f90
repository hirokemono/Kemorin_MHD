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
      use m_phys_labels
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
      private :: set_vector_field_name_w_SGS
      private :: set_scalar_field_name_w_SGS
      private :: set_tensor_field_name_w_SGS
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine ordering_fld_w_SGS_by_viz(field_ctl, fld)
!
      use set_nodal_field_name
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
      use set_nodal_field_name
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
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name_w_SGS                            &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use m_field_product_labels
!
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_vector_fields(phys_name_ctl)                        &
     &   .or. check_SGS_vector_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_vector,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      if(phys_name_ctl .eq. geostrophic_balance%name) then
        flag = .TRUE.
        call append_field_name_list(rest_of_geostrophic%name, n_vector, &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_vector_field_name_w_SGS
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name_w_SGS                            &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use m_energy_flux_labels
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_scalar_fields(phys_name_ctl)                        &
     &   .or. check_SGS_scalar_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_scalar,            &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
!   Old field label... Should be deleted later!!
      if(phys_name_ctl .eq. buoyancy_work%name) then
        flag = .TRUE.
        call append_field_name_list(buoyancy_flux%name, n_scalar,       &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_scalar_field_name_w_SGS
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name_w_SGS                            &
     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical, intent(in) :: flag_viz, flag_monitor
      type(phys_data), intent(inout) :: fld
      logical, intent(inout) :: flag
!
!
      flag =  check_sym_tensor_fields(phys_name_ctl)                    &
     &   .or. check_SGS_sym_tensor_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, n_sym_tensor,        &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      flag =  check_asym_tensor_fields(phys_name_ctl)                   &
     &  .or. check_SGS_asym_tensor_fields(phys_name_ctl)
      if(flag) then
        call append_field_name_list(phys_name_ctl, ithree,              &
     &      flag_viz, flag_monitor, ione, fld)
        return
      end if
!
      end subroutine set_tensor_field_name_w_SGS
!
! -----------------------------------------------------------------------
!
      end module set_field_name_w_SGS
