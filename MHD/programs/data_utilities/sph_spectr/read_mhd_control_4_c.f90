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
      type(c_ptr) function c_dynamo_vizs_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_dynamo_vizs_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_block_name = C_loc(f_ctl%block_name)
      end function c_dynamo_vizs_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_dynamo_vizs_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_iflag = C_loc(f_ctl%i_viz_ctl)
      end function c_dynamo_vizs_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_crust_filter_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_dynamo_vizs_crust_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_crust_filter_ctl = C_loc(f_ctl%crust_filter_ctl)
      end function c_dynamo_vizs_crust_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_zm_psf_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_dynamo_vizs_zm_psf_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_zm_psf_ctls = C_loc(f_ctl%zm_psf_ctls)
      end function c_dynamo_vizs_zm_psf_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_zRMS_psf_ctls(c_ctl)           &
     &          bind(C, NAME = 'c_dynamo_vizs_zRMS_psf_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_zRMS_psf_ctls = C_loc(f_ctl%zRMS_psf_ctls)
      end function c_dynamo_vizs_zRMS_psf_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_zm_map_ctls(c_ctl)             &
     &          bind(C, NAME = 'c_dynamo_vizs_zm_map_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_zm_map_ctls = C_loc(f_ctl%zm_map_ctls)
      end function c_dynamo_vizs_zm_map_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamo_vizs_zRMS_map_ctls(c_ctl)           &
     &          bind(C, NAME = 'c_dynamo_vizs_zRMS_map_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dynamo_viz_controls), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamo_vizs_zRMS_map_ctls = C_loc(f_ctl%zRMS_map_ctls)
      end function c_dynamo_vizs_zRMS_map_ctls
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
      type(c_ptr) function c_node_monitor_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_node_monitor_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_node_monitor_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_node_monitor_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_ctl_iflag = C_loc(f_ctl%i_monitor_data)
      end function c_node_monitor_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_xx_monitor_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_node_monitor_xx_monitor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_xx_monitor_ctl = C_loc(f_ctl%xx_4_monitor_ctl)
      end function c_node_monitor_xx_monitor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_node_mntr_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_node_monitor_node_mntr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_node_mntr_ctl = C_loc(f_ctl%node_4_monitor_ctl)
      end function c_node_monitor_node_mntr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_group_mntr_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_node_monitor_group_mntr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_group_mntr_ctl = C_loc(f_ctl%group_4_monitor_ctl)
      end function c_node_monitor_group_mntr_ctl
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
      type(c_ptr) function c_MHD_field_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_field_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_field_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_field_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_iflag = C_loc(f_ctl%i_phys_values)
      end function c_MHD_field_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_ctl_field_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_field_ctl_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_field_ctl = C_loc(f_ctl%field_ctl)
      end function c_MHD_field_ctl_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_quad_phys_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_field_quad_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_quad_phys_ctl = C_loc(f_ctl%quad_phys)
      end function c_MHD_field_quad_phys_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_scalar_phys_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_scalar_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_scalar_phys_ctl = C_loc(f_ctl%scalar_phys)
      end function c_MHD_scalar_phys_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_vector_phys_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_vector_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_vector_phys_ctl = C_loc(f_ctl%vector_phys)
      end function c_MHD_vector_phys_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evolution_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_MHD_evolution_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evolution_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_evolution_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evolution_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_evolution_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evolution_ctl_iflag = C_loc(f_ctl%i_time_evo)
      end function c_MHD_evolution_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_t_evo_field_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_t_evo_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_t_evo_field_ctl = C_loc(f_ctl%t_evo_field_ctl)
      end function c_MHD_t_evo_field_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_area_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_evo_area_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_area_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_evo_area_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_area_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_evo_area_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_area_ctl_iflag = C_loc(f_ctl%i_layers_ctl)
      end function c_MHD_evo_area_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_fluid_group_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_evo_fluid_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_fluid_group_ctl = C_loc(f_ctl%evo_fluid_group_ctl)
      end function c_MHD_evo_fluid_group_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_conduct_group_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_evo_conduct_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_conduct_group_ctl = C_loc(f_ctl%evo_conduct_group_ctl)
      end function c_MHD_evo_conduct_group_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_gravity_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_iflag = C_loc(f_ctl%i_gravity_ctl)
      end function c_MHD_gravity_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_gravity_model(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_FEM_gravity_model')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_gravity_model = C_loc(f_ctl%FEM_gravity_model)
      end function c_MHD_FEM_gravity_model
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_gravity(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_gravity')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_gravity = C_loc(f_ctl%gravity)
      end function c_MHD_gravity_ctl_gravity
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_gravity_ctl_vector(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_gravity_ctl_vector')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gravity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_gravity_ctl_vector = C_loc(f_ctl%gravity_vector)
      end function c_MHD_gravity_ctl_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_coriolis_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_coriolis_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_coriolis_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_coriolis_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_coriolis_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_coriolis_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_coriolis_ctl_iflag = C_loc(f_ctl%i_coriolis_ctl)
      end function c_MHD_coriolis_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_coriolis_model(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_FEM_coriolis_model')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_coriolis_model = C_loc(f_ctl%FEM_coriolis_model)
      end function c_MHD_FEM_coriolis_model
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_FEM_coriolis_implicit(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_FEM_coriolis_implicit')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_FEM_coriolis_implicit = C_loc(f_ctl%FEM_coriolis_implicit)
      end function c_MHD_FEM_coriolis_implicit
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_system_rotation(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_system_rotation')
      type(c_ptr), value, intent(in) :: c_ctl
      type(coriolis_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_system_rotation = C_loc(f_ctl%system_rotation)
      end function c_MHD_system_rotation
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_mag_cv_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_iflag = C_loc(f_ctl%i_magneto_ctl)
      end function c_MHD_mag_cv_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_filterd_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_mag_cv_filterd_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_filterd_ctl = C_loc(f_ctl%filterd_induction_ctl)
      end function c_MHD_mag_cv_filterd_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_magneto_cv(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_magneto_cv')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_magneto_cv = C_loc(f_ctl%magneto_cv)
      end function c_MHD_mag_cv_ctl_magneto_cv
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_mag_cv_ctl_ext_magne(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_mag_cv_ctl_ext_magne')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magneto_convection_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_mag_cv_ctl_ext_magne = C_loc(f_ctl%ext_magne)
      end function c_MHD_mag_cv_ctl_ext_magne
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_B_scale_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_B_scale_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_B_scale_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_ctl_iflag = C_loc(f_ctl%i_bscale_ctl)
      end function c_MHD_B_scale_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_B_scale_mag_to_kin_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_B_scale_mag_to_kin_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(magnetic_field_scale_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_B_scale_mag_to_kin_ctl = C_loc(f_ctl%mag_to_kin_energy_ctl)
      end function c_MHD_B_scale_mag_to_kin_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_model_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_model_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_model_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_takepiro_model_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_model_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_takepiro_model_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_model_ctl_iflag = C_loc(f_ctl%i_takepiro_t_ctl)
      end function c_takepiro_model_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_sigma_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_stratified_sigma_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_sigma_ctl                                   &
     &                       = C_loc(f_ctl%stratified_sigma_ctl)
      end function c_takepiro_stratified_sigma_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_width_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_stratified_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_width_ctl                                   &
     &                       = C_loc(f_ctl%stratified_width_ctl)
      end function c_takepiro_stratified_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_rout_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_takepiro_stratified_rout_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_rout_ctl                                   &
     &                       = C_loc(f_ctl%stratified_outer_r_ctl)
      end function c_takepiro_stratified_rout_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_reftemp_point_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_reftemp_point_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_reftemp_point_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_ctl_iflag = C_loc(f_ctl%i_referenced)
      end function c_reftemp_point_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_value_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_reftemp_point_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_value_ctl = C_loc(f_ctl%value)
      end function c_reftemp_point_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_depth_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_reftemp_point_depth_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_depth_ctl = C_loc(f_ctl%depth)
      end function c_reftemp_point_depth_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_temp_model_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_temp_model_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_temp_model_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ctl_iflag = C_loc(f_ctl%i_temp_def)
      end function c_temp_model_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_filter_advect_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_temp_model_filter_advect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_filter_advect_ctl = C_loc(f_ctl%filterd_advect_ctl)
      end function c_temp_model_filter_advect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_reference_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_temp_model_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_reference_ctl = C_loc(f_ctl%reference_ctl)
      end function c_temp_model_reference_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_stratified_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_temp_model_stratified_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_stratified_ctl = C_loc(f_ctl%stratified_ctl)
      end function c_temp_model_stratified_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ref_file_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_temp_model_ref_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ref_file_ctl = C_loc(f_ctl%ref_file_ctl)
      end function c_temp_model_ref_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ICB_diffuse_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_temp_model_ICB_diffuse_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ICB_diffuse_ctl                                      &
     &             = C_loc(f_ctl%ICB_diffuse_reduction_ctl)
      end function c_temp_model_ICB_diffuse_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_low_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_temp_model_low_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_low_ctl = C_loc(f_ctl%low_ctl)
      end function c_temp_model_low_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_high_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_temp_model_high_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_high_ctl = C_loc(f_ctl%high_ctl)
      end function c_temp_model_high_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_takepiro_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_temp_model_takepiro_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_takepiro_ctl = C_loc(f_ctl%takepiro_ctl)
      end function c_temp_model_takepiro_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_filter_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_filter_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_3d_filter_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_filter_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_filter_ctl_iflag = C_loc(f_ctl%i_SGS_3d_filter_ctl)
      end function c_SGS_3d_filter_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_whole_filter_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_whole_filter_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_whole_filter_grp_ctl = C_loc(f_ctl%whole_filter_grp_ctl)
      end function c_SGS_3d_whole_filter_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_fluid_filter_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_fluid_filter_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_fluid_filter_grp_ctl = C_loc(f_ctl%fluid_filter_grp_ctl)
      end function c_SGS_3d_fluid_filter_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_momentum_filter_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_3d_momentum_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_momentum_filter_ctl = C_loc(f_ctl%momentum_filter_ctl)
      end function c_SGS_3d_momentum_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_heat_filter_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_3d_heat_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_heat_filter_ctl = C_loc(f_ctl%heat_filter_ctl)
      end function c_SGS_3d_heat_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_induction_filter_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_induction_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_induction_filter_ctl = C_loc(f_ctl%induction_filter_ctl)
      end function c_SGS_3d_induction_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_comp_filter_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_3d_comp_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_comp_filter_ctl = C_loc(f_ctl%compostion_filter_ctl)
      end function c_SGS_3d_comp_filter_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_file_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_filter_file_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_file_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_filter_file_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_file_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_SGS_filter_file_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_file_iflag = C_loc(f_ctl%i_filter_fnames)
      end function c_SGS_filter_file_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_head_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_SGS_filter_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_head_ctl = C_loc(f_ctl%filter_head_ctl)
      end function c_SGS_filter_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_coef_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_coef_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_coef_head_ctl = C_loc(f_ctl%filter_coef_head_ctl)
      end function c_SGS_filter_coef_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_elen_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_elen_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_elen_head_ctl = C_loc(f_ctl%filter_elen_head_ctl)
      end function c_SGS_filter_elen_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_moms_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_moms_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_moms_head_ctl = C_loc(f_ctl%filter_moms_head_ctl)
      end function c_SGS_filter_moms_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_wide_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_wide_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_wide_head_ctl = C_loc(f_ctl%filter_wide_head_ctl)
      end function c_SGS_filter_wide_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_coef_ini_head(c_ctl)             &
     &          bind(C, NAME = 'c_SGS_model_coef_ini_head')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_coef_ini_head = C_loc(f_ctl%model_coef_ini_head_ctl)
      end function c_SGS_model_coef_ini_head
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_commute_coef_ini_head(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_commute_coef_ini_head')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_commute_coef_ini_head                                       &
     &            = C_loc(f_ctl%commute_coef_ini_head_ctl)
      end function c_SGS_commute_coef_ini_head
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_elen_format(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_filter_elen_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_elen_format = C_loc(f_ctl%filter_elen_format)
      end function c_SGS_filter_elen_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_3d_format(c_ctl)                &
     &          bind(C, NAME = 'c_SGS_filter_3d_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_3d_format = C_loc(f_ctl%filter_3d_format)
      end function c_SGS_filter_3d_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_wide_format(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_filter_wide_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_wide_format = C_loc(f_ctl%filter_wide_format)
      end function c_SGS_filter_wide_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_coef_rst_format(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_coef_rst_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_coef_rst_format = C_loc(f_ctl%model_coef_rst_format)
      end function c_SGS_model_coef_rst_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_commute_coef_rst_format(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_commute_coef_rst_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_commute_coef_rst_format                                     &
     &              = C_loc(f_ctl%commute_coef_rst_format)
      end function c_SGS_commute_coef_rst_format
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_layering_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_layering_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_layering_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_ctl_iflag = C_loc(f_ctl%i_dynamic_layers)
      end function c_SGS_layering_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_grp_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_layering_grp_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_grp_type_ctl = C_loc(f_ctl%layering_grp_type_ctl)
      end function c_SGS_layering_grp_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layer_grp_name_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_layer_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layer_grp_name_ctl = C_loc(f_ctl%layer_grp_name_ctl)
      end function c_SGS_layer_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_igrp_stack_layer_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_igrp_stack_layer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_igrp_stack_layer_ctl = C_loc(f_ctl%igrp_stack_layer_ctl)
      end function c_SGS_igrp_stack_layer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_num_layering_grp_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_num_layering_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_num_layering_grp_ctl = C_loc(f_ctl%num_layering_grp_ctl)
      end function c_SGS_num_layering_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_num_fluid_layer_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_num_fluid_layer_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_num_fluid_layer_grp_ctl = C_loc(f_ctl%num_fl_layer_grp_ctl)
      end function c_SGS_num_fluid_layer_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_start_layer_grp_name_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_start_layer_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_start_layer_grp_name_ctl                                    &
     &          = C_loc(f_ctl%start_layering_grp_name_ctl)
      end function c_SGS_start_layer_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_start_fluid_grp_name_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_start_fluid_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_start_fluid_grp_name_ctl                                    &
     &          = C_loc(f_ctl%start_fl_layer_grp_name_ctl)
      end function c_SGS_start_fluid_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_ngrp_on_sphere_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_ngrp_on_sphere_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_ngrp_on_sphere_ctl = C_loc(f_ctl%ngrp_SGS_on_sphere_ctl)
      end function c_SGS_ngrp_on_sphere_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_sph_filter_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_sph_filter_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_sph_filter_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_sph_filter_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_sph_filter_ctl_iflag = C_loc(f_ctl%i_sph_filter_ctl)
      end function c_SGS_sph_filter_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_sph_filter_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_sph_filter_type_ctl = C_loc(f_ctl%sph_filter_type_ctl)
      end function c_SPH_SGS_sph_filter_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_r_filter_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SPH_SGS_r_filter_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_r_filter_type_ctl = C_loc(f_ctl%radial_filter_type_ctl)
      end function c_SPH_SGS_r_filter_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_maximum_moments_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_maximum_moments_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_maximum_moments_ctl = C_loc(f_ctl%maximum_moments_ctl)
      end function c_SPH_SGS_maximum_moments_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_sph_filter_width_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_sph_filter_width_ctl                                    &
     &            = C_loc(f_ctl%sphere_filter_width_ctl)
      end function c_SPH_SGS_sph_filter_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_r_filter_width_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SPH_SGS_r_filter_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_r_filter_width_ctl                                      &
     &            = C_loc(f_ctl%radial_filter_width_ctl)
      end function c_SPH_SGS_r_filter_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_first_reference_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_first_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_first_reference_ctl = C_loc(f_ctl%first_reference_ctl)
      end function c_SPH_SGS_first_reference_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_second_reference_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SPH_SGS_second_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_second_reference_ctl= C_loc(f_ctl%second_reference_ctl)
      end function c_SPH_SGS_second_reference_ctl
!
!  ---------------------------------------------------------------------
!
      end module read_mhd_control_4_c
