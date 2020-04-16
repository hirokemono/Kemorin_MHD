!>@file   set_nodal_field_name.f90
!!@brief  module set_nodal_field_name
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!!@n        modified by H.Matsui on Oct.,  2009
!!@n        modified by H.Matsui on June., 2012
!
!>@brief  Set field names from control data
!!
!!@verbatim
!!      subroutine set_vector_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!      subroutine set_scalar_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!      subroutine set_tensor_field_name                                &
!!     &         (phys_name_ctl, flag_viz, flag_monitor, fld, flag)
!!        type(phys_data), intent(inout) :: fld
!!
!!      logical function check_vis_control_flag(visualize_ctl)
!!      logical function check_monitor_control_flag(monitor_ctl)
!!
!!      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!!      subroutine set_monitor_control_flag                             &
!!     &         (iflag_fld_monitor, monitor_ctl)
!!@endverbatim
!
      module set_nodal_field_name
!
      use m_precision
      use m_phys_labels
      use t_phys_data
!
      implicit  none
!
      character(len = kchara), parameter :: cflag_viz_on =  'Viz_On'
      character(len = kchara), parameter :: cflag_viz_off = 'Viz_Off'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_on =  'Monitor_On'
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_off = 'Monitor_Off'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name                                  &
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
!   Find vector field name
      if(flag) return
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
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name                                  &
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
!  set number of components ( vector and scalar )
!
      if(flag) return
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
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name                                  &
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
!  set number of components ( vector and scalar )
!
      if(flag) return
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
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_vis_control_flag(visualize_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
!
      check_vis_control_flag = cmp_no_case(visualize_ctl, cflag_viz_on)
!
      end function check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      logical function check_monitor_control_flag(monitor_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
!
      check_monitor_control_flag                                        &
     &      = cmp_no_case(monitor_ctl, cflag_monitor_on)
!
      end function check_monitor_control_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!
      integer (kind = kint), intent(in) :: iflag_viz
      character(len = kchara), intent(inout) :: visualize_ctl
!
      if(iflag_viz .gt. 0) then
        visualize_ctl = cflag_viz_on
      else
        visualize_ctl = cflag_viz_off
      end if
!
      end subroutine set_vis_control_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_monitor_control_flag                               &
     &         (iflag_fld_monitor, monitor_ctl)
!
      integer (kind = kint), intent(in) :: iflag_fld_monitor
      character(len = kchara), intent(inout) :: monitor_ctl
!
      if(iflag_fld_monitor .gt. 0) then
        monitor_ctl = cflag_monitor_on
      else
        monitor_ctl = cflag_monitor_off
      end if
!
      end subroutine set_monitor_control_flag
!
! -----------------------------------------------------------------------
!
      end module set_nodal_field_name
