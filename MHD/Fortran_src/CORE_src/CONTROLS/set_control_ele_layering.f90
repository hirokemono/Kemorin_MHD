!set_control_ele_layering.f90
!      module set_control_ele_layering
!
!        programmed by H.Matsui on Nov., 2009
!
!      subroutine s_set_control_ele_layering
!
!      subroutine count_layering_ele_grp_list
!      subroutine set_layering_ele_grp_list
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
      subroutine s_set_control_ele_layering
!
      use m_ctl_data_ele_layering
      use set_layer_list_by_table
!
!
      if    (layering_grp_type_ctl .eq. 'explicit'                      &
     &  .or. layering_grp_type_ctl .eq. 'Explicit'                      &
     &  .or. layering_grp_type_ctl .eq. 'EXPLICIT') then
        iflag_layering_type = 0
      else if( layering_grp_type_ctl .eq. 'ele_group_list'              &
     &    .or. layering_grp_type_ctl .eq. 'Ele_group_list'              &
     &    .or. layering_grp_type_ctl .eq. 'ELE_GROUP_LIST') then
        iflag_layering_type = 2
      else if( layering_grp_type_ctl .eq. 'start_end'                   &
     &    .or. layering_grp_type_ctl .eq. 'Start_end'                   &
     &    .or. layering_grp_type_ctl .eq. 'START_END') then
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
        ntotal_layer_grp = ntotal_layer_grp_ctl
        num_layer_grp = num_layer_grp_ctl
        call allocate_layering_ele_grp
!
        if(num_layer_grp.gt. 0) then
          igrp_stack_each_layer(1:num_layer_grp)                        &
     &        = igrp_stack_each_layer_ctl(1:num_layer_grp)
        end if
        if(ntotal_layer_grp .gt. 0) then
          dynamic_layer_grp_name(1:ntotal_layer_grp)                    &
     &        = layer_grp_name_ctl(1:ntotal_layer_grp)
        end if
!
        call deallocate_layer_grp_ctl
!
      else if (iflag_layering_type .eq. 2) then
        num_layering_grp =              num_layering_grp_ctl
        num_fluid_layering_grp =        num_fl_layer_grp_ctl
        start_layering_grp_name =       start_layering_grp_name_ctl
        start_fluid_layering_grp_name = start_fl_layer_grp_name_ctl
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
