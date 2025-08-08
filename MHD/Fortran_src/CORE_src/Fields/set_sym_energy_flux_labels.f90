!>@file   set_sym_energy_flux_labels.f90
!!        module set_sym_energy_flux_labels
!!
!!@author T, Kera and H. Matsui
!!@date   Programmed in July, 2021 by T. Kera (Tohoku Univ.)
!!@date   Modified in Aug., 2025 by H. Matsui (Tohoku Univ.)
!!
!!
!> @brief Labels and addresses for energy flux
!!            decomposed by equatoreal symmetries
!!
!!@verbatim
!!      subroutine set_sym_eflx_address_by_sym_asm                      &
!!      &         (i_phys, field_name, eflux_s_sxa, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_s_sxa
!!        logical, intent(inout) :: flag
!!!!!! Symmetric energy fluxes by rot(F_asym) X F_asym !!!!!!!!!!!!!!!
!!
!!   mns_us_d_ws_x_ua  [eflux_s_sxa%i_m_advect_work]
!!       : Work of inertia:              -u_s \cdot (\omega_s \times u_a)
!!   us_d_js_x_ba      [eflux_s_sxa%i_ujb]
!!       :  Work of Lorentz force:        u_s \cdot (J_s \times B_a)
!!
!!   us_d_ws_x_ua      [eflux_s_sxa%i_uwu]
!!       : Work against of inertia:       u_s \cdot (\omega_s \times u_a)
!!   mns_us_d_js_x_ba  [eflux_s_sxa%i_nega_ujb]
!!       :  Work against Lorentz force:  -u_s \cdot (J_s \times B_a)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      subroutine set_sym_eflx_address_by_asm_sym                      &
!!      &         (i_phys, field_name, eflux_s_axs, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_s_axs
!!        logical, intent(inout) :: flag
!!!!!! Symmetric energy fluxes by rot(F_sym) X F_sym !!!!!!!!!!!!!!!
!!
!!   mns_us_d_wa_x_us  [eflux_s_axs%i_m_advect_work]
!!       : Work of inertia:              -u_s \cdot (\omega_a \times u_s)
!!   mns_us_d_z_x_us   [i_Coriolis_work]
!!       :  Work of Coriolis force:      -2u_s \cdot (\Omega \times u_s)
!!   us_d_ja_x_bs      [eflux_s_axs%i_ujb]
!!       :  Work of Lorentz force:          u_s \cdot (J_a \times B_s)
!!
!!   us_d_wa_x_us      [eflux_s_axs%i_uwu]
!!       : Work against of inertia:       u_s \cdot (\omega_a \times u_s)
!!   us_d_z_x_us       [eflux_s_axs%i_work_against_Coriolis]
!!       : Work against of Coriolis force: 2u_s \cdot (\Omega \times u_s)
!!   mns_us_d_ja_x_bs  [eflux_s_axs%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_s \cdot (J_a \times B_s)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      subroutine set_asm_eflx_address_by_sym_sym                      &
!!      &         (i_phys, field_name, eflux_a_sxs, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_a_sxs
!!        logical, intent(inout) :: flag
!!!!!! Antisymmetric energy fluxes by rot(F_asym) X F_sym !!!!!!!!!!!!!!!
!!
!!   mns_ua_d_ws_x_us  [eflux_a_sxs%i_m_advect_work]
!!       : Work of inertia:              -u_a \cdot (\omega_s \times u_s)
!!   ua_d_ws_x_us      [eflux_a_sxs%i_ujb]
!!       :  Work of Lorentz force:          u_a \cdot (J_s \times B_s)
!!
!!   ua_d_js_x_bs      [eflux_a_sxs%i_uwu]
!!       : Work against of inertia:       u_a \cdot (\omega_s \times u_s)
!!   mns_ua_d_js_x_bs  [eflux_a_sxs%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_a \cdot (J_s \times B_s)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      subroutine set_asm_eflx_address_by_asm_asm                      &
!!      &        (i_phys, field_name, eflux_a_axa, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_a_axa
!!        logical, intent(inout) :: flag
!!!!!! Antisymmetric energy fluxes by rot(F_sym) X F_asym !!!!!!!!!!!!!!!
!!
!!   mns_ua_d_wa_x_ua  [eflux_a_axa%i_m_advect_work]
!!       : Work of inertia:              -u_a \cdot (\omega_a \times u_a)
!!   mns_ua_d_z_x_ua   [i_Coriolis_work]
!!       :  Work of Coriolis force:       -2u_a \cdot (\Omega \times u_a)
!!   ua_d_ja_x_ba      [eflux_a_axa%i_ujb]
!!       :  Work of Lorentz force:          u_a \cdot (J_a \times B_a)
!!
!!   ua_d_wa_x_ua      [eflux_a_axa%i_uwu]
!!       : Work against of inertia:       u_a \cdot (\omega_a \times u_a)
!!   ua_d_z_x_ua       [eflux_a_axa%i_work_against_Coriolis]
!!       : Work against of Coriolis force: 2u_a \cdot (\Omega \times u_a)
!!   mns_ua_d_ja_x_ba  [eflux_a_axa%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_a \cdot (J_a \times B_a)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module set_sym_energy_flux_labels
!
      use m_precision
      use m_constants
      use t_base_force_labels
      use t_energy_flux_labels
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_eflx_address_by_sym_asm                        &
      &         (i_phys, field_name, eflux_s_sxa, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_s_sxa
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
        if (field_name .eq. us_d_js_x_ba%name) then
          eflux_s_sxa%i_ujb =           i_phys
        else if (field_name .eq. mns_us_d_ws_x_ua%name) then
          eflux_s_sxa%i_m_advect_work =       i_phys
!        else if (field_name .eq. mns_us_d_z_x_ua%name) then
!          eflux_s_sxa%i_Coriolis_work =       i_phys
!
        else if (field_name .eq. mns_us_d_js_x_ba%name) then
          eflux_s_sxa%i_nega_ujb =              i_phys
        else if (field_name .eq. us_d_ws_x_ua%name) then
          eflux_s_sxa%i_uwu =                   i_phys
!        else if (field_name .eq. us_d_z_x_ua%name) then
!          eflux_s_sxa%i_work_against_Coriolis = i_phys
!
!        else if(field_name .eq. sym_thermal_buoyancy_flux%name) then
!          eflux_s_sxa%i_t_buo_flux = i_phys
!        else if(field_name .eq. sym_composite_buoyancy_flux%name) then
!          eflux_s_sxa%i_c_buo_flux = i_phys
        else if(field_name .eq. sym_buoyancy_flux%name) then
          eflux_s_sxa%i_buo_flux =   i_phys
        end if
      end if
!
      end subroutine set_sym_eflx_address_by_sym_asm
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_eflx_address_by_asm_sym                        &
      &         (i_phys, field_name, eflux_s_axs, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_s_axs
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
        if (field_name .eq. us_d_ja_x_bs%name) then
          eflux_s_axs%i_ujb =           i_phys
        else if (field_name .eq. mns_us_d_wa_x_us%name) then
          eflux_s_axs%i_m_advect_work = i_phys
        else if (field_name .eq. mns_us_d_z_x_us%name) then
          eflux_s_axs%i_Coriolis_work = i_phys
!
        else if (field_name .eq. mns_us_d_ja_x_bs%name) then
          eflux_s_axs%i_nega_ujb =              i_phys
        else if (field_name .eq. us_d_wa_x_us%name) then
          eflux_s_axs%i_uwu =                   i_phys
        else if (field_name .eq. us_d_z_x_us%name) then
          eflux_s_axs%i_work_against_Coriolis = i_phys
        end if
      end if
!
      end subroutine set_sym_eflx_address_by_asm_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_asm_eflx_address_by_sym_sym                        &
      &         (i_phys, field_name, eflux_a_sxs, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_a_sxs
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
        if (field_name .eq. ua_d_js_x_bs%name) then
          eflux_a_sxs%i_ujb =           i_phys
        else if (field_name .eq. mns_ua_d_ws_x_us%name) then
          eflux_a_sxs%i_m_advect_work = i_phys
!        else if (field_name .eq. mns_ua_d_z_x_ua%name) then
!          eflux_a_sxs%i_Coriolis_work = i_phys
!
        else if (field_name .eq. mns_ua_d_js_x_bs%name) then
          eflux_a_sxs%i_nega_ujb =              i_phys
        else if (field_name .eq. ua_d_ws_x_us%name) then
          eflux_a_sxs%i_uwu =                   i_phys
!        else if (field_name .eq. ua_d_z_x_ua%name) then
!          eflux_a_sxs%i_work_against_Coriolis = i_phys
!
!        else if(field_name .eq. asym_thermal_buoyancy_flux%name) then
!          eflux_a_sxs%i_t_buo_flux = i_phys
!        else if(field_name .eq. asym_composite_buoyancy_flux%name) then
!          eflux_a_sxs%i_c_buo_flux = i_phys
!        else if(field_name .eq. asym_buoyancy_flux%name) then
!          eflux_a_sxs%i_buo_flux =   i_phys
        end if
      end if
!
      end subroutine set_asm_eflx_address_by_sym_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_asm_eflx_address_by_asm_asm                        &
      &        (i_phys, field_name, eflux_a_axa, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_a_axa
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. ua_d_ja_x_ba%name) then
          eflux_a_axa%i_ujb =           i_phys
        else if (field_name .eq. mns_ua_d_wa_x_ua%name) then
          eflux_a_axa%i_m_advect_work = i_phys
        else if (field_name .eq. mns_ua_d_z_x_ua%name) then
          eflux_a_axa%i_Coriolis_work = i_phys
!
        else if (field_name .eq. mns_ua_d_ja_x_ba%name) then
          eflux_a_axa%i_nega_ujb =              i_phys
        else if (field_name .eq. ua_d_wa_x_ua%name) then
          eflux_a_axa%i_uwu =                   i_phys
        else if (field_name .eq. ua_d_z_x_ua%name) then
          eflux_a_axa%i_work_against_Coriolis = i_phys
        end if
      end if
!
      end subroutine set_asm_eflx_address_by_asm_asm
!
! ----------------------------------------------------------------------
!
      end module set_sym_energy_flux_labels
