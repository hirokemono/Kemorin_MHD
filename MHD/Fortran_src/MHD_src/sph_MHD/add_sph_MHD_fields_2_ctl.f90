!
!      module add_sph_MHD_fields_2_ctl
!
!        programmed by H.Matsui on Sep., 2009
!
!      subroutine add_field_name_4_sph_mhd
!
      module add_sph_MHD_fields_2_ctl
!
      use m_precision
!
      use m_control_parameter
      use m_phys_labels
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_mhd
!
      use add_nodal_fields_ctl
!
!
!   velocity flag
      if(iflag_t_evo_4_velo.gt.0 .or. iflag_t_evo_4_magne.gt.0) then
        call add_phys_name_tmp(fhd_velo)
      end if
!   vorticity flag
      if(iflag_t_evo_4_velo.gt.0) then
        call add_phys_name_tmp(fhd_vort)
      end if
!   magnetic field flag
      if(iflag_t_evo_4_magne.gt.0 .or. iflag_4_lorentz.gt.0) then
        call add_phys_name_tmp(fhd_magne)
        call add_phys_name_tmp(fhd_current)
      end if
!
!   gradient of temperature flag
      if(iflag_t_evo_4_temp.gt.0) then
        call add_phys_name_tmp(fhd_grad_temp)
        call add_phys_name_tmp(fhd_part_temp)
        call add_phys_name_tmp(fhd_grad_par_temp)
      end if
!
!   gradient of dummy scalar flag
      if(iflag_t_evo_4_composit.gt.0) then
        call add_phys_name_tmp(fhd_grad_composit)
      end if
!
!
!
!   advection flag
      if(iflag_t_evo_4_velo.gt.0) then
        call add_phys_name_tmp(fhd_viscous)
        call add_phys_name_tmp(fhd_div_viscous)
        call add_phys_name_tmp(fhd_w_viscous)
!
        call add_phys_name_tmp(fhd_inertia)
        call add_phys_name_tmp(fhd_rot_inertia)
        call add_phys_name_tmp(fhd_div_inertia)
!
!   Coriolis flag
        if(iflag_4_coriolis.gt.0) then
          call add_phys_name_tmp(fhd_Coriolis)
          call add_phys_name_tmp(fhd_rot_Coriolis)
          call add_phys_name_tmp(fhd_div_Coriolis)
        end if
!   Lorentz flag
        if(iflag_4_lorentz.gt.0) then
          call add_phys_name_tmp(fhd_Lorentz)
          call add_phys_name_tmp(fhd_rot_Lorentz)
          call add_phys_name_tmp(fhd_div_Lorentz)
        end if
!   buoyancy flag
        if(iflag_4_gravity.gt.0) then
          call add_phys_name_tmp(fhd_buoyancy)
          call add_phys_name_tmp(fhd_rot_buoyancy)
          call add_phys_name_tmp(fhd_div_buoyancy)
        end if
!   compositional buoyancy flag
        if(iflag_4_composit_buo.gt.0) then
          call add_phys_name_tmp(fhd_comp_buo)
          call add_phys_name_tmp(fhd_div_comp_buo)
          call add_phys_name_tmp(fhd_rot_comp_buo)
        end if
!   filtered buoyancy flag
        if(iflag_4_filter_gravity.gt.0) then
          call add_phys_name_tmp(fhd_filter_buo)
          call add_phys_name_tmp(fhd_div_filter_buo)
          call add_phys_name_tmp(fhd_rot_filter_buo)
        end if
      end if
!
!   induction flag
      if(iflag_t_evo_4_magne.gt.0) then
        call add_phys_name_tmp(fhd_mag_diffuse)
!
        call add_phys_name_tmp(fhd_mag_induct)
        call add_phys_name_tmp(fhd_vp_induct)
      end if
!
!   divergence of heat flux flag
      if(iflag_t_evo_4_temp.gt.0) then
        call add_phys_name_tmp(fhd_thermal_diffusion)
        call add_phys_name_tmp(fhd_h_flux)
        call add_phys_name_tmp(fhd_heat_advect)
      end if
!
!   divergence of dummy scalar flag
      if(iflag_t_evo_4_composit.gt.0) then
        call add_phys_name_tmp(fhd_c_diffuse)
        call add_phys_name_tmp(fhd_c_flux)
        call add_phys_name_tmp(fhd_composit_advect)
      end if
!
      end subroutine add_field_name_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module add_sph_MHD_fields_2_ctl
