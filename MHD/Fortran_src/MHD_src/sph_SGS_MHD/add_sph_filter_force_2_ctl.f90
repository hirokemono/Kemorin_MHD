!>@file   add_sph_filter_force_2_ctl.f90
!!@brief  module add_sph_filter_force_2_ctl
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Add fields in control list for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_filter_force_4_sph_mhd                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
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
      subroutine add_filter_force_4_sph_mhd                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!
      use add_nodal_fields_ctl
      use m_filtered_force_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
!
!   filtered advection flag
        if(fl_prop%iflag_4_filter_inertia) then
          call add_phys_name_ctl(inertia_by_filtered%name, field_ctl)
          call add_phys_name_ctl(rot_inertia_by_filtered%name,          &
     &                           field_ctl)
          call add_phys_name_ctl(div_inertia_by_filtered%name,          &
     &                           field_ctl)
        end if  
!
!   Lorentz flag
        if(fl_prop%iflag_4_filter_lorentz) then
          call add_phys_name_ctl(Lorentz_force_by_filtered%name,        &
     &                           field_ctl)
          call add_phys_name_ctl(rot_Lorentz_force_by_filtered%name,    &
     &                           field_ctl)
          call add_phys_name_ctl(div_Lorentz_force_by_filtered%name,    &
     &                           field_ctl)
        end if
!
!   filtered thermal buoyancy flag
        if(fl_prop%iflag_4_filter_gravity) then
          call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
          call add_phys_name_ctl(div_filtered_buoyancy%name, field_ctl)
          call add_phys_name_ctl(rot_filtered_buoyancy%name, field_ctl)
        end if
!   filtered compositional buoyancy flag
        if(fl_prop%iflag_4_filter_comp_buo) then
          call add_phys_name_ctl                                        &
     &       (filtered_comp_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (div_filtered_comp_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (rot_filtered_comp_buoyancy%name, field_ctl)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_4_filter_induction) then
        call add_phys_name_ctl(magnetic_induction_by_filtered%name,     &
     &                         field_ctl)
        call add_phys_name_ctl(vecp_induction_by_filtered%name,         &
     &                         field_ctl)
      end if
!
!   divergence of heat flux flag
      if(ht_prop%iflag_4_filter_advection) then
        call add_phys_name_ctl(heat_flux_by_filtered%name, field_ctl)
        call add_phys_name_ctl(heat_advect_by_filtered%name, field_ctl)
      end if
!
!   divergence of dummy scalar flag
      if(cp_prop%iflag_4_filter_advection) then
        call add_phys_name_ctl(composite_flux_by_filtered%name,         &
     &                         field_ctl)
        call add_phys_name_ctl(comp_advect_by_filtered%name, field_ctl)
      end if
!
      end subroutine add_filter_force_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module add_sph_filter_force_2_ctl
