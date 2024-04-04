!
      module read_mhd_control_4_c
!
      use iso_c_binding
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_ctl_data_sph_MHD_w_psf
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_mhd_evo_area
      use t_ctl_data_gravity
!
      implicit none
!
      type(mhd_simulation_control), save, target :: MHD_ctl_C
      type(add_sgs_sph_mhd_ctl), save, target, private :: add_SSMHD_ctl_C
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
      type(c_ptr) function c_add_sgs_sph_mhd_ctl()                      &
     &          bind(C, NAME = 'c_add_sgs_sph_mhd_ctl')
      c_add_sgs_sph_mhd_ctl = C_loc(add_SSMHD_ctl_C)
      end function c_add_sgs_sph_mhd_ctl
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
      type(c_ptr) function c_MHD_sgs_ctl(c_ctl)                         &
     &          bind(C, NAME = 'c_MHD_sgs_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_sgs_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_sgs_ctl = C_loc(f_ctl%sgs_ctl)
      end function c_MHD_sgs_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_viz_ctls(c_ctl)                        &
     &          bind(C, NAME = 'c_MHD_viz_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_sgs_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_viz_ctls = C_loc(f_ctl%viz_ctls)
      end function c_MHD_viz_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_zm_ctls(c_ctl)                         &
     &          bind(C, NAME = 'c_MHD_zm_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_sgs_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_zm_ctls = C_loc(f_ctl%zm_ctls)
      end function c_MHD_zm_ctls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_viz3_ctls(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_viz3_ctls')
      use t_ctl_data_sph_MHD_w_vizs
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_vizs_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_viz3_ctls = C_loc(f_ctl%viz3_ctls)
      end function c_MHD_viz3_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_zm_ctls3(c_ctl)                        &
     &          bind(C, NAME = 'c_MHD_zm_ctls3')
      use t_ctl_data_sph_MHD_w_vizs
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_vizs_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_zm_ctls3 = C_loc(f_ctl%zm_ctls)
      end function c_MHD_zm_ctls3
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surfacing_ctls(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_surfacing_ctls')
      use t_ctl_data_sph_MHD_w_psf
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_psf_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surfacing_ctls = C_loc(f_ctl%surfacing_ctls)
      end function c_MHD_surfacing_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_zm_sects(c_ctl)                        &
     &          bind(C, NAME = 'c_MHD_zm_sects')
      use t_ctl_data_sph_MHD_w_psf
      type(c_ptr), value, intent(in) :: c_ctl
      type(add_psf_sph_mhd_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_zm_sects = C_loc(f_ctl%zm_sects)
      end function c_MHD_zm_sects
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_visualizations_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_block_name = C_loc(f_ctl%block_name)
      end function c_visualizations_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_visualizations_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_iflag = C_loc(f_ctl%i_viz_control)
      end function c_visualizations_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_psf_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_visualizations_psf_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_psf_ctls = C_loc(f_ctl%psf_ctls)
      end function c_visualizations_psf_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_iso_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_visualizations_iso_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_iso_ctls = C_loc(f_ctl%iso_ctls)
      end function c_visualizations_iso_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_map_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_visualizations_map_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_map_ctls = C_loc(f_ctl%map_ctls)
      end function c_visualizations_map_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_pvr_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_visualizations_pvr_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_pvr_ctls = C_loc(f_ctl%pvr_ctls)
      end function c_visualizations_pvr_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_fline_ctls(c_ctl)           &
     &          bind(C, NAME = 'c_visualizations_fline_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_fline_ctls = C_loc(f_ctl%fline_ctls)
      end function c_visualizations_fline_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_lic_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_visualizations_lic_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_lic_ctls = C_loc(f_ctl%lic_ctls)
      end function c_visualizations_lic_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_repart_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_visualizations_repart_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_repart_ctl = C_loc(f_ctl%repart_ctl)
      end function c_visualizations_repart_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_visualizations_fname_vrepart(c_ctl)        &
     &          bind(C, NAME = 'c_visualizations_fname_vrepart')
      type(c_ptr), value, intent(in) :: c_ctl
      type(visualization_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_visualizations_fname_vrepart= C_loc(f_ctl%fname_vol_repart_ctl)
      end function c_visualizations_fname_vrepart
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_shell_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_sph_shell_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(parallel_sph_shell_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_shell_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_shell_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_shell_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_sph_shell_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(parallel_sph_shell_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_shell_ctl_iflag = C_loc(f_ctl%iflag_sph_shell)
      end function c_sph_shell_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_shell_Fmesh_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_sph_shell_Fmesh_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(parallel_sph_shell_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_shell_Fmesh_ctl = C_loc(f_ctl%Fmesh_ctl)
      end function c_sph_shell_Fmesh_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_shell_spctl(c_ctl)                     &
     &          bind(C, NAME = 'c_sph_shell_spctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(parallel_sph_shell_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_shell_spctl = C_loc(f_ctl%spctl)
      end function c_sph_shell_spctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_shell_sdctl(c_ctl)                     &
     &          bind(C, NAME = 'c_sph_shell_sdctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(parallel_sph_shell_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_shell_sdctl = C_loc(f_ctl%sdctl)
      end function c_sph_shell_sdctl
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
      type(c_ptr) function c_smctl_ctl_block_name(c_ctl)                &
     &          bind(C, NAME = 'c_smctl_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_mhd_control_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_smctl_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_smctl_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_smctl_ctl_iflag(c_ctl)                     &
     &          bind(C, NAME = 'c_smctl_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_mhd_control_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_smctl_ctl_iflag = C_loc(f_ctl%i_control)
      end function c_smctl_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_smctl_ctl_tctl(c_ctl)                      &
     &          bind(C, NAME = 'c_smctl_ctl_tctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_mhd_control_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_smctl_ctl_tctl = C_loc(f_ctl%tctl)
      end function c_smctl_ctl_tctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_smctl_mrst_ctl(c_ctl)                      &
     &          bind(C, NAME = 'c_smctl_mrst_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_mhd_control_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_smctl_mrst_ctl = C_loc(f_ctl%mrst_ctl)
      end function c_smctl_mrst_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_smctl_mevo_ctl(c_ctl)                      &
     &          bind(C, NAME = 'c_smctl_mevo_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_mhd_control_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_smctl_mevo_ctl = C_loc(f_ctl%mevo_ctl)
      end function c_smctl_mevo_ctl
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
      write(*,*) 'smonitor_ctl%pspec_ctl%pick_radius_ctl', &
      &  MHD_ctl_C%smonitor_ctl%pspec_ctl%pick_radius_ctl%num, &
      &  allocated(MHD_ctl_C%smonitor_ctl%pspec_ctl%pick_radius_ctl%vect)
      
      write(*,'(a,z16)') 'smonitor_ctl', c_loc(MHD_ctl_C%smonitor_ctl)
      write(*,'(a,z16)') 'pspec_ctl', c_loc(MHD_ctl_C%smonitor_ctl%pspec_ctl)
      write(*,'(a,z16)') 'pick_radius_ctl', c_loc(MHD_ctl_C%smonitor_ctl%pspec_ctl%pick_radius_ctl)

      len = len_trim(MHD_ctl_C%block_name) + 1
      write(MHD_ctl_C%block_name(len:len),'(a1)') char(0)
      c_read_control_sph_SGS_MHD = C_loc(MHD_ctl_C)
      write(*,'(a,z16)') 'c_read_control_sph_SGS_MHD in F', &
     &                  c_read_control_sph_SGS_MHD
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
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_pvr_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_pvr_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_ctl_iflag = C_loc(f_ctl%i_pvr_ctl)
      end function c_VIZ_pvr_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_fname_mat_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_pvr_fname_mat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_fname_mat_ctl = C_loc(f_ctl%fname_mat_ctl)
      end function c_VIZ_pvr_fname_mat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_viewmat_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_pvr_viewmat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_viewmat_ctl = C_loc(f_ctl%mat)
      end function c_VIZ_pvr_viewmat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_fname_pvr_light_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_pvr_fname_pvr_light_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_fname_pvr_light_ctl = C_loc(f_ctl%fname_pvr_light_c)
      end function c_VIZ_pvr_fname_pvr_light_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_light_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_light_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_light_ctl = C_loc(f_ctl%light)
      end function c_VIZ_pvr_light_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_fname_cmap_cbar_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_pvr_fname_cmap_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_fname_cmap_cbar_ctl = C_loc(f_ctl%fname_cmap_cbar_c)
      end function c_VIZ_pvr_fname_cmap_cbar_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_cmap_cbar_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_pvr_cmap_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_cmap_cbar_ctl = C_loc(f_ctl%cmap_cbar_c)
      end function c_VIZ_pvr_cmap_cbar_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_movie_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_movie_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_movie_ctl = C_loc(f_ctl%movie)
      end function c_VIZ_pvr_movie_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_quilt_c(c_ctl)                     &
     &          bind(C, NAME = 'c_VIZ_pvr_quilt_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_quilt_c = C_loc(f_ctl%quilt_c)
      end function c_VIZ_pvr_quilt_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_updated_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_pvr_updated_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_updated_ctl = C_loc(f_ctl%updated_ctl)
      end function c_VIZ_pvr_updated_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_file_head_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_pvr_file_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_file_head_ctl = C_loc(f_ctl%file_head_ctl)
      end function c_VIZ_pvr_file_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_file_fmt_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_pvr_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_file_fmt_ctl = C_loc(f_ctl%file_fmt_ctl)
      end function c_VIZ_pvr_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_monitoring_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_pvr_monitoring_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_monitoring_ctl = C_loc(f_ctl%monitoring_ctl)
      end function c_VIZ_pvr_monitoring_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_streo_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_streo_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_streo_ctl = C_loc(f_ctl%streo_ctl)
      end function c_VIZ_pvr_streo_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_anaglyph_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_pvr_anaglyph_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_anaglyph_ctl = C_loc(f_ctl%anaglyph_ctl)
      end function c_VIZ_pvr_anaglyph_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_quilt_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_quilt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_quilt_ctl = C_loc(f_ctl%quilt_ctl)
      end function c_VIZ_pvr_quilt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_render_area_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_pvr_render_area_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_render_area_ctl = C_loc(f_ctl%render_area_c)
      end function c_VIZ_pvr_render_area_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_field_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_pvr_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_field_ctl = C_loc(f_ctl%pvr_field_ctl)
      end function c_VIZ_pvr_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_comp_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_VIZ_pvr_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_comp_ctl = C_loc(f_ctl%pvr_comp_ctl)
      end function c_VIZ_pvr_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_section_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_pvr_section_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_section_ctl = C_loc(f_ctl%pvr_scts_c)
      end function c_VIZ_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_pvr_isosurf_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_pvr_isosurf_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_pvr_isosurf_ctl = C_loc(f_ctl%pvr_isos_c)
      end function c_VIZ_pvr_isosurf_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pvr_area_ctl_block_name(c_ctl)             &
     &          bind(C, NAME = 'c_pvr_area_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_render_area_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pvr_area_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_pvr_area_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pvr_area_ctl_iflag(c_ctl)                  &
     &          bind(C, NAME = 'c_pvr_area_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_render_area_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pvr_area_ctl_iflag = C_loc(f_ctl%i_plot_area)
      end function c_pvr_area_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pvr_area_pvr_area_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_pvr_area_pvr_area_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_render_area_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pvr_area_pvr_area_ctl = C_loc(f_ctl%pvr_area_ctl)
      end function c_pvr_area_pvr_area_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pvr_area_surf_enhanse_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_pvr_area_surf_enhanse_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_render_area_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pvr_area_surf_enhanse_ctl = C_loc(f_ctl%surf_enhanse_ctl)
      end function c_pvr_area_surf_enhanse_ctl
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
