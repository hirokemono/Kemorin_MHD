!>@file   set_control_evo_layers.f90
!!@brief  module set_control_evo_layers
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for simulation areas from control data
!!
!!@verbatim
!!     subroutine s_set_control_evo_layers
!!@endverbatim
!
!
      module set_control_evo_layers
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      implicit  none
!
      private :: set_fluid_layer_egrp_name
      private :: set_conduct_layer_egrp_name
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_evo_layers
!
      use m_ctl_data_mhd_evolution
!
!
      if       (iflag_t_evo_4_velo .eq. id_no_evolution                 &
     &    .and. iflag_t_evo_4_temp .eq. id_no_evolution                 &
     &    .and. iflag_t_evo_4_composit .eq. id_no_evolution) then
!
          num_fl_ele_grp =  1
          call allocate_fluid_ele_grp_name
          fl_ele_grp_name = 'none'
!
          call set_conduct_layer_egrp_name
!
      else
        call set_fluid_layer_egrp_name
!
        if     (iflag_t_evo_4_magne .eq. id_no_evolution                &
     &    .and. iflag_t_evo_4_vect_p .eq. id_no_evolution) then
          num_cd_ele_grp =  1
          call allocate_conduct_ele_grp_name
          cd_ele_grp_name = 'none'
!
        else
          call set_conduct_layer_egrp_name
        end if
      end if
!
      num_in_core_ele_grp = 0
!         =  num_ele_in_core_grp_ctl
!      if (num_in_core_ele_grp .ne. 0 ) then
!        allocate(in_core_ele_grp_name(num_in_core_ele_grp))
!        in_core_ele_grp_name = num_ele_in_core_grp_ctl
!      end if
!
      end subroutine s_set_control_evo_layers
!
! -----------------------------------------------------------------------
!
      subroutine set_fluid_layer_egrp_name
!
      use m_ctl_data_mhd_evolution
!
!
      if (i_fluid_grp.eq.0) then
        num_fl_ele_grp = 1
        call allocate_fluid_ele_grp_name
        fl_ele_grp_name = 'all'
      else
        num_fl_ele_grp =  num_ele_fl_grp_ctl
        if (num_fl_ele_grp .ne. 0 ) then
          call allocate_fluid_ele_grp_name
          fl_ele_grp_name =  ele_fl_grp_ctl
          call dealloc_ele_fl_grp_ctl
        end if
      end if
!
      end subroutine set_fluid_layer_egrp_name
!
! -----------------------------------------------------------------------
!
      subroutine set_conduct_layer_egrp_name
!
      use m_ctl_data_mhd_evolution
!
!
      if (i_conduct_grp.eq.0) then
        num_cd_ele_grp = 1
        call allocate_conduct_ele_grp_name
        cd_ele_grp_name =  'all'
      else
        num_cd_ele_grp =  num_ele_cd_grp_ctl
        if (num_cd_ele_grp .ne. 0 ) then
          call allocate_conduct_ele_grp_name
          cd_ele_grp_name =  ele_cd_grp_ctl
          call dealloc_ele_cd_grp_ctl
        end if
      end if
!
      end subroutine set_conduct_layer_egrp_name
!
! -----------------------------------------------------------------------
!
      end module set_control_evo_layers
