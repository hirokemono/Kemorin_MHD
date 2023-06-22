!
      module read_mhd_control_4_c
!
      use iso_c_binding
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_ctl_data_sph_MHD_w_psf
!
      implicit none
!
      type(mhd_simulation_control), save, target :: MHD_ctl_C
      type(add_sgs_sph_mhd_ctl), save, private :: add_SSMHD_ctl_C
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl_C
      integer(kind = kint), parameter :: id_ctl = 11
!
      private :: load_chara_from_cc
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      character(len = kchara) function load_chara_from_cc(c_in)
        character(C_char) :: c_in(kchara)
        integer :: i
        do i = 1, kchara
          load_chara_from_cc(i:i) = c_in(i)
          if(c_in(i) .eq. char(0)) then
            load_chara_from_cc(i:kchara) = char(32)
            exit
          end if
        end do
      end function load_chara_from_cc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_block_name(c_ctl)                      &
     &          bind(C, NAME = 'c_MHD_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_iflag(c_ctl)                           &
     &          bind(C, NAME = 'c_MHD_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_iflag = C_loc(f_ctl%i_mhd_ctl)
      end function c_MHD_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_plt(c_ctl)                             &
     &          bind(C, NAME = 'c_MHD_plt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_plt = C_loc(f_ctl%plt)
      end function c_MHD_plt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_org_plt(c_ctl)                         &
     &          bind(C, NAME = 'c_MHD_org_plt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_org_plt = C_loc(f_ctl%org_plt)
      end function c_MHD_org_plt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_new_plt(c_ctl)                         &
     &          bind(C, NAME = 'c_MHD_new_plt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_new_plt = C_loc(f_ctl%new_plt)
      end function c_MHD_new_plt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_fname_psph(c_ctl)                      &
     &          bind(C, NAME = 'c_MHD_fname_psph')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_fname_psph = C_loc(f_ctl%fname_psph)
      end function c_MHD_fname_psph
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_psph_ctl(c_ctl)                        &
     &          bind(C, NAME = 'c_MHD_psph_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_psph_ctl = C_loc(f_ctl%psph_ctl)
      end function c_MHD_psph_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_model_ctl(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_model_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_model_ctl = C_loc(f_ctl%model_ctl)
      end function c_MHD_model_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_smctl_ctl(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_smctl_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_smctl_ctl = C_loc(f_ctl%smctl_ctl)
      end function c_MHD_smctl_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_smonitor_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_smonitor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_smonitor_ctl = C_loc(f_ctl%smonitor_ctl)
      end function c_MHD_smonitor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_nmtr_ctl(c_ctl)                        &
     &          bind(C, NAME = 'c_MHD_nmtr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_simulation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_nmtr_ctl = C_loc(f_ctl%nmtr_ctl)
      end function c_MHD_nmtr_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_block_name(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_mdl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_mdl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_iflag(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_mdl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_iflag = C_loc(f_ctl%i_model)
      end function c_MHD_mdl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_fld_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_fld_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_fld_ctl = C_loc(f_ctl%fld_ctl)
      end function c_MHD_mdl_fld_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_evo_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_evo_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_evo_ctl = C_loc(f_ctl%evo_ctl)
      end function c_MHD_mdl_evo_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_earea_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_mdl_earea_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_earea_ctl = C_loc(f_ctl%earea_ctl)
      end function c_MHD_mdl_earea_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_nbc_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_nbc_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_nbc_ctl = C_loc(f_ctl%nbc_ctl)
      end function c_MHD_mdl_nbc_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_sbc_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_sbc_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_sbc_ctl = C_loc(f_ctl%sbc_ctl)
      end function c_MHD_mdl_sbc_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_dless_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_mdl_dless_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_dless_ctl = C_loc(f_ctl%dless_ctl)
      end function c_MHD_mdl_dless_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_eqs_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_eqs_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_eqs_ctl = C_loc(f_ctl%eqs_ctl)
      end function c_MHD_mdl_eqs_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_frc_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_frc_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_frc_ctl = C_loc(f_ctl%frc_ctl)
      end function c_MHD_mdl_frc_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_g_ctl(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_mdl_g_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_g_ctl = C_loc(f_ctl%g_ctl)
      end function c_MHD_mdl_g_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_cor_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_cor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_cor_ctl = C_loc(f_ctl%cor_ctl)
      end function c_MHD_mdl_cor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_mcv_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_mdl_mcv_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_mcv_ctl = C_loc(f_ctl%mcv_ctl)
      end function c_MHD_mdl_mcv_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_bscale_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_mdl_bscale_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_bscale_ctl = C_loc(f_ctl%bscale_ctl)
      end function c_MHD_mdl_bscale_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_reft_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_mdl_reft_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_reft_ctl = C_loc(f_ctl%reft_ctl)
      end function c_MHD_mdl_reft_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mdl_refc_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_mdl_refc_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mdl_refc_ctl = C_loc(f_ctl%refc_ctl)
      end function c_MHD_mdl_refc_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_forces_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_forces_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_forces_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_iflag = C_loc(f_ctl%i_forces_ctl)
      end function c_MHD_forces_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_array(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_forces_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_array = C_loc(f_ctl%force_names)
      end function c_MHD_forces_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_dimless_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_dimless_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_dimless_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_iflag = C_loc(f_ctl%i_dimless_ctl)
      end function c_MHD_dimless_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_array(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_dimless_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_array = C_loc(f_ctl%dimless)
      end function c_MHD_dimless_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_block_name(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_eqs_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_eqs_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_iflag(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_eqs_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_iflag = C_loc(f_ctl%i_coef_term_ctl)
      end function c_MHD_eqs_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_mom_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_eqs_mom_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_mom_ctl = C_loc(f_ctl%mom_ctl)
      end function c_MHD_eqs_mom_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_induct_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_eqs_induct_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_induct_ctl = C_loc(f_ctl%induct_ctl)
      end function c_MHD_eqs_induct_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_heat_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_eqs_heat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_heat_ctl = C_loc(f_ctl%heat_ctl)
      end function c_MHD_eqs_heat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_comp_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_eqs_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_comp_ctl = C_loc(f_ctl%comp_ctl)
      end function c_MHD_eqs_comp_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_momentum_eq_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_momentum_eq_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_iflag = C_loc(f_ctl%i_momentum)
      end function c_MHD_momentum_eq_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_viscous(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_viscous')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_viscous = C_loc(f_ctl%coef_4_viscous)
      end function c_MHD_momentum_eq_viscous
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_inertia(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_inertia')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_inertia = C_loc(f_ctl%coef_4_intertia)
      end function c_MHD_momentum_eq_inertia
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_grad_p(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_momentum_eq_grad_p')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_grad_p = C_loc(f_ctl%coef_4_grad_p)
      end function c_MHD_momentum_eq_grad_p
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_t_buoyancy(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_t_buoyancy')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_t_buoyancy = C_loc(f_ctl%coef_4_termal_buo)
      end function c_MHD_momentum_eq_t_buoyancy
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_c_buoyancy(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_c_buoyancy')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_c_buoyancy = C_loc(f_ctl%coef_4_comp_buo)
      end function c_MHD_momentum_eq_c_buoyancy
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_coriolis(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_momentum_eq_coriolis')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_coriolis = C_loc(f_ctl%coef_4_Coriolis)
      end function c_MHD_momentum_eq_coriolis
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_lorentz(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_lorentz')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_lorentz = C_loc(f_ctl%coef_4_Lorentz)
      end function c_MHD_momentum_eq_lorentz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_induction_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_induction_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_induction_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_iflag = C_loc(f_ctl%i_induct_ctl)
      end function c_MHD_induction_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_evo(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_induction_evo')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_evo = C_loc(f_ctl%coef_4_magne_evo)
      end function c_MHD_induction_evo
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_diffuse(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_induction_diffuse')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_diffuse = C_loc(f_ctl%coef_4_mag_diffuse)
      end function c_MHD_induction_diffuse
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_potential(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_induction_potential')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_potential = C_loc(f_ctl%coef_4_mag_potential)
      end function c_MHD_induction_potential
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_uxb(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_induction_uxb')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_uxb = C_loc(f_ctl%coef_4_induction)
      end function c_MHD_induction_uxb
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_block_name(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_heat_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_heat_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_iflag(c_ctl)                      &
     &          bind(C, NAME = 'c_MHD_heat_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_iflag = C_loc(f_ctl%i_diff_adv)
      end function c_MHD_heat_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_advect(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_heat_advect')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_advect = C_loc(f_ctl%coef_4_adv_flux)
      end function c_MHD_heat_advect
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_diffuse(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_heat_diffuse')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_diffuse = C_loc(f_ctl%coef_4_diffuse)
      end function c_MHD_heat_diffuse
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_source(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_heat_source')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_source = C_loc(f_ctl%coef_4_source)
      end function c_MHD_heat_source
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_read_control_sph_SGS_MHD(names_c)          &
     &          bind(C, NAME = 'c_read_control_sph_SGS_MHD')
!
      type(c_ptr), value, intent(in) :: names_c
!
      type(buffer_for_control) :: c_buf1
      integer(kind = kint) :: len
      character(len=kchara) :: MHD_ctl_name
!
      character(C_char), pointer ::  name_f(:)
!
      call c_f_pointer(names_c, name_f, [kchara+1])
      MHD_ctl_name = load_chara_from_cc(name_f)
!
      c_buf1%level = 0
      call read_control_file_sph_SGS_MHD(MHD_ctl_name,                  &
     &    MHD_ctl_C, add_SSMHD_ctl_C, c_buf1)
      if(c_buf1%iend .gt. 0) stop 'Error in control file'
!
      len = len_trim(MHD_ctl_C%block_name) + 1
      write(MHD_ctl_C%block_name(len:len),'(a1)') char(0)
      c_read_control_sph_SGS_MHD = C_loc(MHD_ctl_C)
!
      end function c_read_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_view_control_sph_SGS_MHD()                           &
     &          bind(C, NAME = 'c_view_control_sph_SGS_MHD')
!
      use write_control_elements
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
!
      integer(kind = kint) :: level
!
      level = 0
      call write_sph_mhd_control_data(id_monitor,                       &
    &     MHD_ctl_C, add_SSMHD_ctl_C, level)
!
      end subroutine c_view_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_write_control_sph_SGS_MHD()                          &
     &          bind(C, NAME = 'c_write_control_sph_SGS_MHD')
!
      use calypso_mpi
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
!
      character(len=kchara), parameter                                  &
     &                      :: MHD_ctl_name = 'control_MHD_dup'
!
!
      call write_control_file_sph_SGS_MHD(MHD_ctl_name, MHD_ctl_C,      &
     &                                    add_SSMHD_ctl_C)
!
      end subroutine c_write_control_sph_SGS_MHD
!
!  ---------------------------------------------------------------------
!
      subroutine c_read_control_sph_MHD()                               &
     &          bind(C, NAME = 'c_read_control_sph_MHD')
!
      use bcast_control_sph_MHD
!
      character(len=kchara), parameter :: MHD_ctl_name = 'control_MHD'
      type(buffer_for_control) :: c_buf1
!
      call read_control_4_sph_MHD_w_psf(MHD_ctl_name, MHD_ctl_C,        &
     &                                  add_SMHD_ctl_C, c_buf1)
!
      end subroutine c_read_control_sph_MHD
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
