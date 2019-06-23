!>@file   t_ctl_data_ele_layering.f90
!!@brief  module t_ctl_data_ele_layering
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  Structure for reading parameters for radial grouping
!!
!!@verbatim
!!      subroutine read_ele_layers_control                              &
!!     &         (id_control, hd_block, iflag, elayer_ctl, c_buf)
!!      subroutine dealloc_ctl_data_ele_layering(elayer_ctl)
!!        type(layering_control), intent(inout) :: elayer_ctl
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
!!        array grp_stack_each_layer_ctl
!!          grp_stack_each_layer_ctl  2
!!          grp_stack_each_layer_ctl  4
!!          grp_stack_each_layer_ctl  6
!!          grp_stack_each_layer_ctl  8
!!        end array grp_stack_each_layer_ctl
!!
!!        array layer_grp_name_ctl
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
      module t_ctl_data_ele_layering
!
      use m_precision
      use t_read_control_elements
      use t_control_elements
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!
!>     Structure for element layering
      type layering_control
!>        Structure for group type
        type(read_character_item) :: layering_grp_type_ctl
!
!>        Structure for layering group names
!!@n        layer_grp_name_ctl%num:   Number of layering group
!!@n        layer_grp_name_ctl%c_tbl: layering group names
        type(ctl_array_chara) :: layer_grp_name_ctl
!
!>        Structure for layering stacks
!!@n        igrp_stack_layer_ctl%ivec: layering stack array
        type(ctl_array_int) :: igrp_stack_layer_ctl
!
!>        Structure for number of group to start
        type(read_integer_item) :: num_layering_grp_ctl
!>        Structure for number of fluid group to start
        type(read_integer_item) :: num_fl_layer_grp_ctl
!>        Structure for group to start
        type(read_character_item) :: start_layering_grp_name_ctl
!>        Structure for fluid group to start
        type(read_character_item) :: start_fl_layer_grp_name_ctl
!
!>        Structure for number of groups on sphere
        type(read_integer_item) :: ngrp_SGS_on_sphere_ctl
!
        integer (kind=kint) :: i_dynamic_layers = 0
      end type layering_control
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
      subroutine read_ele_layers_control                                &
     &         (id_control, hd_block, elayer_ctl, c_buf)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(layering_control), intent(inout) :: elayer_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(elayer_ctl%i_dynamic_layers .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control, hd_ntotal_layer_grp_ctl, &
     &      elayer_ctl%layer_grp_name_ctl, c_buf)
!
        call read_control_array_i1(id_control, hd_num_layer_grp_ctl,    &
     &      elayer_ctl%igrp_stack_layer_ctl, c_buf)
!
!
        call read_integer_ctl_type(c_buf, hd_num_SGS_ele_grp,           &
     &      elayer_ctl%num_layering_grp_ctl)
        call read_integer_ctl_type(c_buf, hd_num_SGS_fluid_grp,         &
     &      elayer_ctl%num_fl_layer_grp_ctl)
!
        call read_integer_ctl_type(c_buf, hd_ngrp_SGS_on_sphere,        &
     &      elayer_ctl%ngrp_SGS_on_sphere_ctl)
!
!
        call read_chara_ctl_type(c_buf, hd_layering_data_ctl,           &
     &      elayer_ctl%layering_grp_type_ctl)
        call read_chara_ctl_type(c_buf, hd_start_SGS_ele_grp_name,      &
     &      elayer_ctl%start_layering_grp_name_ctl)
        call read_chara_ctl_type(c_buf, hd_start_SGS_fluid_grp_name,    &
     &      elayer_ctl%start_fl_layer_grp_name_ctl)
      end do
      elayer_ctl%i_dynamic_layers = 1
!
      end subroutine read_ele_layers_control
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_ele_layering(elayer_ctl)
!
      type(layering_control), intent(inout) :: elayer_ctl
!
!
      call dealloc_control_array_int(elayer_ctl%igrp_stack_layer_ctl)
      call dealloc_control_array_chara(elayer_ctl%layer_grp_name_ctl)
!
      elayer_ctl%i_dynamic_layers = 0
!
      end subroutine dealloc_ctl_data_ele_layering
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_ele_layering
