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
      use t_ctl_data_ele_layering
!
      implicit  none
!
!>     Structure for element layering
      type(layering_control), save :: elayer_ctl1
!
!   labels for entry
!
      character(len=kchara), parameter :: hd_dynamic_layers             &
     &                        = 'dynamic_model_layer_ctl'
      integer (kind=kint) :: i_dynamic_layers = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ele_layers_grp_ctl
!
!
      call read_ele_layers_control                                      &
     &   (hd_dynamic_layers, i_dynamic_layers, elayer_ctl1)
!
      end subroutine read_ele_layers_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_ele_layering
