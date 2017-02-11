!>@file   set_control_evo_layers.f90
!!@brief  module set_control_evo_layers
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for simulation areas from control data
!!
!!@verbatim
!!     subroutine s_set_control_evo_layers(earea_ctl)
!!       type(mhd_evo_area_control), intent(inout) :: earea_ctl
!!@endverbatim
!
!
      module set_control_evo_layers
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use t_ctl_data_mhd_evolution
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
      subroutine s_set_control_evo_layers(earea_ctl)
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
!
      if       (evo_velo%iflag_scheme .eq. id_no_evolution              &
     &    .and. evo_temp%iflag_scheme .eq. id_no_evolution              &
     &    .and. evo_comp%iflag_scheme .eq. id_no_evolution) then
!
          FEM_prm1%fluid_group%num_group =  1
          call allocate_fluid_ele_grp_name
          FEM_prm1%fluid_group%group_name = 'none'
!
          call set_conduct_layer_egrp_name(earea_ctl)
!
      else
        call set_fluid_layer_egrp_name(earea_ctl)
!
        if     (evo_magne%iflag_scheme .eq. id_no_evolution             &
     &    .and. evo_vect_p%iflag_scheme .eq. id_no_evolution) then
          FEM_prm1%condutive_group%num_group =  1
          call allocate_conduct_ele_grp_name
          FEM_prm1%condutive_group%group_name = 'none'
!
        else
          call set_conduct_layer_egrp_name(earea_ctl)
        end if
      end if
!
      FEM_prm1%inner_core_group%num_group = 0
!         =  num_ele_in_core_grp_ctl
!      if (FEM_prm1%inner_core_group%num_group .ne. 0 ) then
!        allocate(FEM_prm1%inner_core_group%group_name(FEM_prm1%inner_core_group%num_group))
!        FEM_prm1%inner_core_group%group_name = num_ele_in_core_grp_ctl
!      end if
!
      end subroutine s_set_control_evo_layers
!
! -----------------------------------------------------------------------
!
      subroutine set_fluid_layer_egrp_name(earea_ctl)
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
!
      if (earea_ctl%evo_fluid_group_ctl%icou .eq. 0) then
        FEM_prm1%fluid_group%num_group = 1
        call allocate_fluid_ele_grp_name
        FEM_prm1%fluid_group%group_name = 'all'
      else
        FEM_prm1%fluid_group%num_group                                  &
     &        =  earea_ctl%evo_fluid_group_ctl%num
        if(FEM_prm1%fluid_group%num_group .ne. 0 ) then
          call allocate_fluid_ele_grp_name
          FEM_prm1%fluid_group%group_name                               &
     &        =  earea_ctl%evo_fluid_group_ctl%c_tbl
          call dealloc_ele_fl_grp_ctl(earea_ctl)
        end if
      end if
!
      end subroutine set_fluid_layer_egrp_name
!
! -----------------------------------------------------------------------
!
      subroutine set_conduct_layer_egrp_name(earea_ctl)
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
!
      if (earea_ctl%evo_conduct_group_ctl%icou .eq. 0) then
        FEM_prm1%condutive_group%num_group = 1
        call allocate_conduct_ele_grp_name
        FEM_prm1%condutive_group%group_name =  'all'
      else
        FEM_prm1%condutive_group%num_group                              &
     &       =  earea_ctl%evo_conduct_group_ctl%num
        if(FEM_prm1%condutive_group%num_group .ne. 0 ) then
          call allocate_conduct_ele_grp_name
          FEM_prm1%condutive_group%group_name                           &
     &            =  earea_ctl%evo_conduct_group_ctl%c_tbl
          call dealloc_ele_cd_grp_ctl(earea_ctl)
        end if
      end if
!
      end subroutine set_conduct_layer_egrp_name
!
! -----------------------------------------------------------------------
!
      end module set_control_evo_layers
