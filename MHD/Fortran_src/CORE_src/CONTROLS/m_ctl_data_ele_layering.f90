!>@file   m_ctl_data_ele_layering.f90
!!@brief  module m_ctl_data_ele_layering
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  Structure for reading parameters for radial grouping
!!
!!@verbatim
!!      subroutine read_ele_layers_grp_ctl
!!
!! -- example of parameters -----------------------------------------
!!
!!  Define by number and starting group of element group list
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     ele_group_list
!!        num_layering_grp_ctl     8
!!        start_layering_grp_name_ctl  fluid_layer_1
!!        num_fl_layer_grp_ctl     8
!!        start_fl_layer_grp_name_ctl  fluid_layer_1
!!      end dynamic_model_layer_ctl
!!
!!  Define by explicit element group list
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     explicit
!!        array grp_stack_each_layer_ctl    4
!!          grp_stack_each_layer_ctl  2
!!          grp_stack_each_layer_ctl  4
!!          grp_stack_each_layer_ctl  6
!!          grp_stack_each_layer_ctl  8
!!        end array grp_stack_each_layer_ctl
!!
!!        array layer_grp_name_ctl    8
!!          layer_grp_name_ctl    fluid_layer_1   end
!!          layer_grp_name_ctl    fluid_layer_2   end
!!          layer_grp_name_ctl    fluid_layer_3   end
!!          layer_grp_name_ctl    fluid_layer_4   end
!!          layer_grp_name_ctl    fluid_layer_5   end
!!          layer_grp_name_ctl    fluid_layer_6   end
!!          layer_grp_name_ctl    fluid_layer_7   end
!!          layer_grp_name_ctl    fluid_layer_8   end
!!        end array layer_grp_name_ctl
!!      end dynamic_model_layer_ctl
!!
!!  Define by start and end element address
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     start_end
!!      end dynamic_model_layer_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_ele_layering
!
      use m_precision
      use t_control_elements
      use t_read_control_arrays
!
      implicit  none
!
!>      Structure for group type
      type(read_character_item), save :: layering_grp_type_ctl
!
!>      Structure for layering group names
!!@n      layer_grp_name_ctl%num:   Number of layering group
!!@n      layer_grp_name_ctl%c_tbl: layering group names
        type(ctl_array_chara), save :: layer_grp_name_ctl
!
!>      Structure for layering stacks
!!@n      igrp_stack_layer_ctl%ivec: layering stack array
        type(ctl_array_int), save :: igrp_stack_layer_ctl
!
!>      Structure for number of group to start
      type(read_integer_item), save :: num_layering_grp_ctl
!>      Structure for number of fluid group to start
      type(read_integer_item), save :: num_fl_layer_grp_ctl
!>      Structure for group to start
      type(read_character_item), save :: start_layering_grp_name_ctl
!>      Structure for fluid group to start
      type(read_character_item), save :: start_fl_layer_grp_name_ctl
!
!
!>      Structure for number of groups on sphere
      type(read_integer_item), save :: ngrp_SGS_on_sphere_ctl
!
!   labels for entry
!
      character(len=kchara), parameter :: hd_dynamic_layers             &
     &                        = 'dynamic_model_layer_ctl'
      integer (kind=kint) :: i_dynamic_layers = 0
!
!    labels for layering parameteres
!
      character(len=kchara), parameter :: hd_layering_data_ctl          &
     &                        = 'layering_data_ctl'
      character(len=kchara), parameter :: hd_ntotal_layer_grp_ctl       &
     &                        = 'layer_grp_name_ctl'
      character(len=kchara), parameter :: hd_num_layer_grp_ctl          &
     &                        = 'grp_stack_each_layer_ctl'
!
      character(len=kchara), parameter :: hd_num_SGS_ele_grp            &
     &                        = 'num_layering_grp_ctl'
      character(len=kchara), parameter :: hd_start_SGS_ele_grp_name     &
     &                        = 'start_layering_grp_name_ctl'
      character(len=kchara), parameter :: hd_num_SGS_fluid_grp          &
     &                        = 'num_fl_layer_grp_ctl'
      character(len=kchara), parameter :: hd_start_SGS_fluid_grp_name   &
     &                        = 'start_fl_layer_grp_name_ctl'
!
      character(len=kchara), parameter :: hd_ngrp_SGS_on_sphere         &
     &                        = 'ngrp_SGS_on_sphere_ctl'
!
      private :: hd_dynamic_layers, i_dynamic_layers
      private :: hd_layering_data_ctl, hd_ntotal_layer_grp_ctl
      private :: hd_num_layer_grp_ctl
!
      private :: hd_num_SGS_ele_grp, hd_start_SGS_ele_grp_name
      private :: hd_num_SGS_fluid_grp, hd_start_SGS_fluid_grp_name
      private :: hd_ngrp_SGS_on_sphere
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ele_layers_grp_ctl
!
      use m_machine_parameter
      use skip_comment_f
!
!
      if(right_begin_flag(hd_dynamic_layers) .eq. 0) return
      if (i_dynamic_layers .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_dynamic_layers, i_dynamic_layers)
        if(i_dynamic_layers .gt. 0) exit
!
!
        call read_control_array_c1                                      &
     &     (hd_ntotal_layer_grp_ctl, layer_grp_name_ctl)
!
        call read_control_array_i1                                      &
     &     (hd_num_layer_grp_ctl, igrp_stack_layer_ctl)
!
!
        call read_integer_ctl_type(hd_num_SGS_ele_grp,                  &
     &      num_layering_grp_ctl)
        call read_integer_ctl_type(hd_num_SGS_fluid_grp,                &
     &      num_fl_layer_grp_ctl)
!
        call read_integer_ctl_type(hd_ngrp_SGS_on_sphere,               &
     &      ngrp_SGS_on_sphere_ctl)
!
!
        call read_chara_ctl_type                                        &
     &     (hd_layering_data_ctl, layering_grp_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_start_SGS_ele_grp_name, start_layering_grp_name_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_start_SGS_fluid_grp_name, start_fl_layer_grp_name_ctl)
      end do
!
      end subroutine read_ele_layers_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_ele_layering
