!>@file   m_ctl_data_mhd_evolution.f90
!!@brief  module m_ctl_data_mhd_evolution
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine dealloc_t_evo_name_ctl
!!      subroutine dealloc_ele_fl_grp_ctl
!!      subroutine dealloc_ele_cd_grp_ctl
!!
!!      subroutine read_mhd_time_evo_control
!!      subroutine read_mhd_layer_control
!!
!! ----------------------------------------------------------------------
!!
!!!!!!  physical values for time evolution !!!!!!!!!!!!!!!!!!
!! aviable valuables: velocity, temperature, magnetic_field
!!                    vector_potential, composition
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin time_evolution_ctl
!!      array time_evo_ctl   4
!!        time_evo_ctl  temperature
!!        time_evo_ctl  velocity
!!        time_evo_ctl  vector_potential
!!        time_evo_ctl  composition
!!      end array time_evo_ctl
!!    end  time_evolution_ctl
!!
!! !!!  setting for layers
!!
!!    begin layers_ctl
!!      array fluid_ele_grp    1
!!        fluid_ele_grp    outer_core
!!      end array fluid_ele_grp
!!
!!      array conduct_ele_grp    2
!!         conduct_ele_grp    inner_core
!!         conduct_ele_grp    outer_core
!!      end array conduct_ele_grp
!!    end  layers_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_ctl_data_mhd_evolution
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_mhd_evolution
!
      implicit  none
!
      type(mhd_evolution_control), save :: evo_ctl1
      type(mhd_evo_area_control), save :: earea_ctl1
!
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_evo =     'time_evolution_ctl'
      integer (kind=kint) :: i_time_evo =      0
!
      character(len=kchara), parameter :: hd_layers_ctl = 'layers_ctl'
      integer (kind=kint) :: i_layers_ctl =    0
!
      private :: hd_time_evo, hd_layers_ctl
      private :: i_time_evo,  i_layers_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_mhd_time_evo_control
!
!
      call read_mhd_time_evo_ctl(hd_time_evo, i_time_evo, evo_ctl1)
!
      end subroutine read_mhd_time_evo_control
!
!   --------------------------------------------------------------------
!
      subroutine read_mhd_layer_control
!
!
      call read_mhd_layer_ctl(hd_layers_ctl, i_layers_ctl, earea_ctl1)
!
      end subroutine read_mhd_layer_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evolution
