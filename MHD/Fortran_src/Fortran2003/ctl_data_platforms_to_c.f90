!>@file   ctl_data_platforms_to_c.f90
!!        module ctl_data_platforms_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
      module ctl_data_platforms_to_c
!
      use m_precision
      use iso_c_binding
      use t_ctl_data_4_platforms
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine  load_chara_from_c(c_ctl)         &
     &          bind(C, NAME = 'load_chara_from_c')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), pointer :: c_in(:)
      integer :: i
!
      call c_f_pointer(c_ctl, c_in, [kchara])
!
      do i = 1, kchara
        if(c_in(i) .eq. char(0)) then
          c_in(i:kchara) = char(32)
          exit
        end if
      end do
      end subroutine load_chara_from_c
!
!  ---------------------------------------------------------------------
!
      subroutine c_chara_item_clength(c_ctl, length_c)                  &
     &          bind(C, NAME = 'c_chara_item_clength')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), intent(inout) :: length_c
      character(len=kchara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      length_c = len_trim(f_ctl)
      write(*,*) 'length_c', length_c
      end subroutine c_chara_item_clength
!
!  ---------------------------------------------------------------------
!
      integer(C_int) function num_file_fmt_items_f()                    &
     &         bind(C, NAME = 'num_file_fmt_items_f')
      use m_file_format_labels
      num_file_fmt_items_f = num_label_file_fmt()
      end function num_file_fmt_items_f
!
! ----------------------------------------------------------------------
!
      subroutine set_file_fmt_items_f(fmt_names_c)                      &
     &          bind(C, NAME = 'set_file_fmt_items_f')
      use m_file_format_labels
!
      type(C_ptr), value :: fmt_names_c
      character(len=1), pointer :: fmt_names(:)
!
      call c_f_pointer(fmt_names_c, fmt_names, [kchara*num_label_file_fmt()])
      call set_label_file_fmt(fmt_names)
      end subroutine set_file_fmt_items_f
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_chara_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_block_name = C_loc(f_ctl%item_name)
      end function c_chara_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_chara_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_iflag = C_loc(f_ctl%iflag)
      end function c_chara_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_charavalue(c_ctl)               &
     &          bind(C, NAME = 'c_chara_item_charavalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_charavalue = C_loc(f_ctl%charavalue)
      end function c_chara_item_charavalue
!
!  ---------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_block_name(c_ctl)                      &
     &          bind(C, NAME = 'c_plt_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
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
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_iflag = C_loc(f_ctl%i_platform)
      end function c_plt_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_ndomain_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_plt_ndomain_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_ndomain_ctl = C_loc(f_ctl%ndomain_ctl)
      end function c_plt_ndomain_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_num_smp_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_plt_num_smp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_num_smp_ctl = C_loc(f_ctl%num_smp_ctl)
      end function c_plt_num_smp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_debug_flag_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_plt_debug_flag_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_debug_flag_ctl = C_loc(f_ctl%debug_flag_ctl)
      end function c_plt_debug_flag_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_sph_file_prefix(c_ctl)                 &
     &          bind(C, NAME = 'c_plt_sph_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_sph_file_prefix = C_loc(f_ctl%sph_file_prefix)
      end function c_plt_sph_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_mesh_file_prefix(c_ctl)                &
     &          bind(C, NAME = 'c_plt_mesh_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_mesh_file_prefix = C_loc(f_ctl%mesh_file_prefix)
      end function c_plt_mesh_file_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_plt_restart_file_prefix(c_ctl)             &
     &          bind(C, NAME = 'c_plt_restart_file_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(platform_data_control), pointer :: f_ctl
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
      call c_f_pointer(c_ctl, f_ctl)
      c_plt_del_org_data_ctl = C_loc(f_ctl%del_org_data_ctl)
      end function c_plt_del_org_data_ctl
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_platforms_to_c
