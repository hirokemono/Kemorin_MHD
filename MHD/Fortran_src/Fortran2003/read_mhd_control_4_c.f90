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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      character(len = kchara) function load_chara_from_c(c_in)
        character(C_char) :: c_in(kchara)
        integer :: i
        do i = 1, kchara
          load_chara_from_c(i:i) = c_in(i)
          if(c_in(i) .eq. char(0)) then
            load_chara_from_c(i:kchara) = char(32)
            exit
          end if
        end do
      end function load_chara_from_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_block_name(c_ctl)                      &
     &          bind(C, NAME = 'c_plt_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_block_name = C_loc(f_ctl%block_name)
      end function c_plt_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_iflag(c_ctl)                           &
     &          bind(C, NAME = 'c_plt_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_iflag = C_loc(f_ctl%ndomain_ctl)
      end function c_plt_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_ndomain_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_plt_ndomain_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_ndomain_ctl = C_loc(f_ctl%num_smp_ctl)
      end function c_plt_ndomain_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_num_smp_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_plt_num_smp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_num_smp_ctl = C_loc(f_ctl%debug_flag_ctl)
      end function c_plt_num_smp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_debug_flag_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_plt_debug_flag_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_debug_flag_ctl = C_loc(f_ctl%sph_file_prefix)
      end function c_plt_debug_flag_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_sph_file_prefix(c_ctl)                 &
     &          bind(C, NAME = 'c_plt_sph_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_sph_file_prefix = C_loc(f_ctl%mesh_file_prefix)
      end function c_plt_sph_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_mesh_file_prefix(c_ctl)                &
     &          bind(C, NAME = 'c_plt_mesh_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_mesh_file_prefix = C_loc(f_ctl%restart_file_prefix)
      end function c_plt_mesh_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_restart_file_prefix(c_ctl)             &
     &          bind(C, NAME = 'c_plt_restart_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_restart_file_prefix = C_loc(f_ctl%restart_file_prefix)
      end function c_plt_restart_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_field_file_prefix(c_ctl)               &
     &          bind(C, NAME = 'c_plt_field_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_field_file_prefix = C_loc(f_ctl%field_file_prefix)
      end function c_plt_field_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_spectr_field_file_prefix(c_ctl)        &
     &          bind(C, NAME = 'c_plt_spectr_field_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_spectr_field_file_prefix                                    &
     &      = C_loc(f_ctl%spectr_field_file_prefix)
      end function c_plt_spectr_field_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_coriolis_int_file_name(c_ctl)          &
     &          bind(C, NAME = 'c_plt_coriolis_int_file_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_coriolis_int_file_name= C_loc(f_ctl%coriolis_int_file_name)
      end function c_plt_coriolis_int_file_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_bc_data_file_name_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_plt_bc_data_file_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_bc_data_file_name_ctl = C_loc(f_ctl%bc_data_file_name_ctl)
      end function c_plt_bc_data_file_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_radial_data_file_name_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_plt_radial_data_file_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_radial_data_file_name_ctl                                   &
     &      = C_loc(f_ctl%radial_data_file_name_ctl)
      end function c_plt_radial_data_file_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_interpolate_sph_to_fem(c_ctl)          &
     &          bind(C, NAME = 'c_plt_interpolate_sph_to_fem')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_interpolate_sph_to_fem= C_loc(f_ctl%interpolate_sph_to_fem)
      end function c_plt_interpolate_sph_to_fem
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_interpolate_fem_to_sph(c_ctl)          &
     &          bind(C, NAME = 'c_plt_interpolate_fem_to_sph')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_interpolate_fem_to_sph= C_loc(f_ctl%interpolate_fem_to_sph)
      end function c_plt_interpolate_fem_to_sph
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_rayleigh_spectr_dir(c_ctl)             &
     &          bind(C, NAME = 'c_plt_rayleigh_spectr_dir')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_rayleigh_spectr_dir = C_loc(f_ctl%rayleigh_spectr_dir)
      end function c_plt_rayleigh_spectr_dir
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_rayleigh_field_dir(c_ctl)              &
     &          bind(C, NAME = 'c_plt_rayleigh_field_dir')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_rayleigh_field_dir = C_loc(f_ctl%rayleigh_field_dir)
      end function c_plt_rayleigh_field_dir
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_sph_file_fmt_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_plt_sph_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_sph_file_fmt_ctl = C_loc(f_ctl%sph_file_fmt_ctl)
      end function c_plt_sph_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_mesh_file_fmt_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_plt_mesh_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_mesh_file_fmt_ctl = C_loc(f_ctl%mesh_file_fmt_ctl)
      end function c_plt_mesh_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_restart_file_fmt_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_plt_restart_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_restart_file_fmt_ctl = C_loc(f_ctl%restart_file_fmt_ctl)
      end function c_plt_restart_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_field_file_fmt_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_plt_field_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_field_file_fmt_ctl = C_loc(f_ctl%field_file_fmt_ctl)
      end function c_plt_field_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_spectr_field_fmt_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_plt_spectr_field_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_spectr_field_fmt_ctl = C_loc(f_ctl%spectr_field_fmt_ctl)
      end function c_plt_spectr_field_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_itp_file_fmt_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_plt_itp_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_itp_file_fmt_ctl = C_loc(f_ctl%itp_file_fmt_ctl)
      end function c_plt_itp_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_coriolis_file_fmt_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_plt_coriolis_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_coriolis_file_fmt_ctl = C_loc(f_ctl%coriolis_file_fmt_ctl)
      end function c_plt_coriolis_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_del_org_data_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_plt_del_org_data_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_del_org_data_ctl = C_loc(f_ctl%del_org_data_ctl)
      end function c_plt_del_org_data_ctl
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
      call c_f_pointer(names_c, name_f, [len+1])
      MHD_ctl_name = load_chara_from_c(name_f)
!
      c_buf1%level = 0
      call read_control_4_sph_SGS_MHD(MHD_ctl_name,                     &
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
