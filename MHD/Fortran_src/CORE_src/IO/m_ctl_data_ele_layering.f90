!
!      module m_ctl_data_ele_layering
!
!        programmed by H.Matsui on Mov., 2006
!
!
!      subroutine allocate_layer_grp_name_ctl
!      subroutine allocate_layer_grp_stack_ctl
!
!      subroutine deallocate_layer_grp_ctl
!
!      subroutine read_ele_layers_grp_ctl
!
! -- example of parameters -----------------------------------------
!
!  Define by number and starting group of element group list
!
!      begin dynamic_model_layer_ctl
!        layering_data_ctl     ele_group_list
!        num_layering_grp_ctl     8
!        start_layering_grp_name_ctl  fluid_layer_1
!        num_fl_layer_grp_ctl     8
!        start_fl_layer_grp_name_ctl  fluid_layer_1
!      end dynamic_model_layer_ctl
!
!  Define by explicit element group list
!
!      begin dynamic_model_layer_ctl
!        layering_data_ctl     explicit
!        array grp_stack_each_layer_ctl    4
!          grp_stack_each_layer_ctl  2
!          grp_stack_each_layer_ctl  4
!          grp_stack_each_layer_ctl  6
!          grp_stack_each_layer_ctl  8
!        end array grp_stack_each_layer_ctl
!
!        array layer_grp_name_ctl    8
!          layer_grp_name_ctl    fluid_layer_1   end
!          layer_grp_name_ctl    fluid_layer_2   end
!          layer_grp_name_ctl    fluid_layer_3   end
!          layer_grp_name_ctl    fluid_layer_4   end
!          layer_grp_name_ctl    fluid_layer_5   end
!          layer_grp_name_ctl    fluid_layer_6   end
!          layer_grp_name_ctl    fluid_layer_7   end
!          layer_grp_name_ctl    fluid_layer_8   end
!        end array layer_grp_name_ctl
!      end dynamic_model_layer_ctl
!
!  Define by start and end element address
!
!      begin dynamic_model_layer_ctl
!        layering_data_ctl     start_end
!      end dynamic_model_layer_ctl
!
! -----------------------------------------------------------------------
!
      module m_ctl_data_ele_layering
!
      use m_precision
!
      implicit  none
!
      character (len=kchara) :: layering_grp_type_ctl
!
      integer (kind=kint)   :: ntotal_layer_grp_ctl
      integer (kind=kint)   :: num_layer_grp_ctl
      integer (kind=kint), allocatable :: igrp_stack_each_layer_ctl(:)
      character (len=kchara), allocatable :: layer_grp_name_ctl(:)
!
      integer (kind=kint) :: num_layering_grp_ctl
      integer (kind=kint) :: num_fl_layer_grp_ctl
      character (len=kchara) :: start_layering_grp_name_ctl
      character (len=kchara) :: start_fl_layer_grp_name_ctl
!
      integer(kind = kint) :: ngrp_SGS_on_sphere_ctl
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
      integer (kind=kint) :: i_layering_data_ctl =    0
      integer (kind=kint) :: i_ntotal_layer_grp_ctl = 0
      integer (kind=kint) :: i_num_layer_grp_ctl =    0
!
      integer (kind=kint) :: i_num_SGS_ele_grp =          0
      integer (kind=kint) :: i_start_SGS_ele_grp_name =   0
      integer (kind=kint) :: i_num_SGS_fluid_grp =        0
      integer (kind=kint) :: i_start_SGS_fluid_grp_name = 0
      integer (kind=kint) :: i_ngrp_SGS_on_sphere =       0
!
      private :: hd_dynamic_layers, i_dynamic_layers
      private :: hd_layering_data_ctl, hd_ntotal_layer_grp_ctl
      private :: hd_num_layer_grp_ctl
!
      private :: hd_num_SGS_ele_grp, hd_start_SGS_ele_grp_name
      private :: hd_num_SGS_fluid_grp, hd_start_SGS_fluid_grp_name
      private :: hd_ngrp_SGS_on_sphere
!
      private :: allocate_layer_grp_name_ctl
      private :: allocate_layer_grp_stack_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_layer_grp_name_ctl
!
      allocate(layer_grp_name_ctl(ntotal_layer_grp_ctl))
!
      end subroutine allocate_layer_grp_name_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_layer_grp_stack_ctl
!
      allocate(igrp_stack_each_layer_ctl(0:num_layer_grp_ctl))
      igrp_stack_each_layer_ctl = 0
!
      end subroutine allocate_layer_grp_stack_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_layer_grp_ctl
!
      deallocate(layer_grp_name_ctl)
      deallocate(igrp_stack_each_layer_ctl)
!
      end subroutine deallocate_layer_grp_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ele_layers_grp_ctl
!
      use m_machine_parameter
      use m_read_control_elements
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
        call find_control_array_flag(hd_ntotal_layer_grp_ctl,           &
     &      ntotal_layer_grp_ctl)
        if(ntotal_layer_grp_ctl.gt.0                                    &
     &       .and. i_ntotal_layer_grp_ctl.eq.0) then
          call allocate_layer_grp_name_ctl
          call read_control_array_chara_list(hd_ntotal_layer_grp_ctl,   &
     &        ntotal_layer_grp_ctl, i_ntotal_layer_grp_ctl,             &
     &        layer_grp_name_ctl)
        end if
!
        call find_control_array_flag(hd_num_layer_grp_ctl,              &
     &      num_layer_grp_ctl)
        if(num_layer_grp_ctl.gt.0 .and. i_num_layer_grp_ctl.eq.0) then
          call allocate_layer_grp_stack_ctl
          call read_control_array_int_list(hd_num_layer_grp_ctl,        &
     &        num_layer_grp_ctl, i_num_layer_grp_ctl,                   &
     &        igrp_stack_each_layer_ctl(1) )
        end if
!
!
        call read_integer_ctl_item(hd_num_SGS_ele_grp,                  &
     &        i_num_SGS_ele_grp, num_layering_grp_ctl)
        call read_integer_ctl_item(hd_num_SGS_fluid_grp,                &
     &        i_num_SGS_fluid_grp, num_fl_layer_grp_ctl)
!
        call read_integer_ctl_item(hd_ngrp_SGS_on_sphere,               &
     &        i_ngrp_SGS_on_sphere, ngrp_SGS_on_sphere_ctl)
!
!
        call read_character_ctl_item(hd_layering_data_ctl,              &
     &        i_layering_data_ctl, layering_grp_type_ctl)
        call read_character_ctl_item(hd_start_SGS_ele_grp_name,         &
     &        i_start_SGS_ele_grp_name, start_layering_grp_name_ctl)
        call read_character_ctl_item(hd_start_SGS_fluid_grp_name,       &
     &        i_start_SGS_fluid_grp_name, start_fl_layer_grp_name_ctl)
      end do
!
      end subroutine read_ele_layers_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_ele_layering
