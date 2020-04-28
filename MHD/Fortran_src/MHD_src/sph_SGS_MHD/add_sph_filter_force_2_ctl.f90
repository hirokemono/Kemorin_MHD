!>@file   add_sph_filter_force_2_ctl.f90
!!@brief  module add_sph_filter_force_2_ctl
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Add fields in control list for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_filter_force_4_sph_mhd(fl_prop, field_ctl)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_sph_filter_force_2_ctl
!
      use m_precision
!
      use m_phys_labels
      use t_control_array_character3
      use t_physical_property
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_filter_force_4_sph_mhd(fl_prop, field_ctl)
!
      use add_nodal_fields_ctl
      use m_filtered_force_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
!   filtered thermal buoyancy flag
        if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
          call add_phys_name_ctl(div_filtered_buoyancy%name, field_ctl)
          call add_phys_name_ctl(rot_filtered_buoyancy%name, field_ctl)
        end if
!   filtered compositional buoyancy flag
        if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl                                        &
     &       (filtered_comp_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (div_filtered_comp_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (rot_filtered_comp_buoyancy%name, field_ctl)
        end if
      end if
!
      end subroutine add_filter_force_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module add_sph_filter_force_2_ctl
