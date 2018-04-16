!>@file   set_control_ele_layering.f90
!!@brief  module set_control_ele_layering
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  Set parameters for radial grouping
!!
!!@verbatim
!!      subroutine s_set_control_ele_layering(elayer_ctl)
!!        type(layering_control), intent(in) :: elayer_ctl
!!@endverbatim
!
      module set_control_ele_layering
!
      use m_precision
!
      use m_machine_parameter
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_control_ele_layering(elayer_ctl)
!
      use t_ctl_data_ele_layering
      use set_layer_list_by_table
      use skip_comment_f
!
      type(layering_control), intent(in) :: elayer_ctl
!
      character(len=kchara) :: tmpchara
!
!
      tmpchara = elayer_ctl%layering_grp_type_ctl%charavalue
      if     (cmp_no_case(tmpchara,'explicit')) then
        iflag_layering_type = 0
      else if(cmp_no_case(tmpchara,'ele_group_list')) then
        iflag_layering_type = 2
      else if(cmp_no_case(tmpchara,'start_end')) then
        iflag_layering_type = 1
      else
        iflag_layering_type = 0
      end if
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'iflag_layering_type', iflag_layering_type
!
      if (iflag_layering_type .eq. 0) then
!
        ntotal_layer_grp = elayer_ctl%igrp_stack_layer_ctl%num
        num_layer_grp =    elayer_ctl%igrp_stack_layer_ctl%num
        call allocate_layering_ele_grp
!
        if(num_layer_grp.gt. 0) then
          igrp_stack_each_layer(1:num_layer_grp)                        &
     &        = elayer_ctl%igrp_stack_layer_ctl%ivec(1:num_layer_grp)
        end if
        if(ntotal_layer_grp .gt. 0) then
          dynamic_layer_grp_name(1:ntotal_layer_grp)                    &
     &        = elayer_ctl%layer_grp_name_ctl%c_tbl(1:ntotal_layer_grp)
        end if
      else if (iflag_layering_type .eq. 2) then
        num_layering_grp = elayer_ctl%num_layering_grp_ctl%intvalue
        num_fluid_layering_grp                                          &
     &     = elayer_ctl%num_fl_layer_grp_ctl%intvalue
        start_layering_grp_name                                         &
     &     = elayer_ctl%start_layering_grp_name_ctl%charavalue
        start_fluid_layering_grp_name                                   &
     &     = elayer_ctl%start_fl_layer_grp_name_ctl%charavalue
      end if
!
      if(iflag_debug .gt. 0) then
        if (iflag_layering_type .eq. 0) then
          write(*,*) 'ntotal_layer_grp', ntotal_layer_grp
          write(*,*) 'num_layer_grp', num_layer_grp
          write(*,*) 'igrp_stack_each_layer', igrp_stack_each_layer
        end if
        if (iflag_layering_type .eq. 2) then
          write(*,*) 'num_layering_grp: ', num_layering_grp
          write(*,*) 'num_fluid_layering_grp: ', num_fluid_layering_grp
          write(*,*) 'start_layering_grp_name: ',                       &
     &              trim(start_layering_grp_name)
          write(*,*) 'start_fluid_layering_grp_name: ',                 &
     &              trim(start_fluid_layering_grp_name)
        end if
      end if
!
      end subroutine s_set_control_ele_layering
!
! ----------------------------------------------------------------------
!
      end module set_control_ele_layering
