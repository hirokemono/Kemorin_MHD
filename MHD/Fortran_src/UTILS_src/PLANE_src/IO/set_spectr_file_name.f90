!
!      module set_spectr_file_name
!
      module set_spectr_file_name
!
!     Written by H. Matsui
!
      use m_precision
!
      use set_parallel_file_name
!
      implicit none
!
      character(len=kchara) :: plane_udt_header
!
      character(len=kchara) :: spec_mode_file_name
      character(len=kchara) :: spec_header
      character(len=kchara) :: ene_header
      character(len=kchara) :: ene_h_header
!
!
      character(len=kchara), parameter                                  &
     &     :: spec_mode_def_name = 'spectr/spec_mode.dat'
      character(len=kchara), parameter                                  &
     &     :: spec_def_header =       'spectr/spectral'
      character(len=kchara), parameter                                  &
     &     :: ene_spec_def_header =   'spectr/ene_spec'
      character(len=kchara), parameter                                  &
     &     :: ene_h_spec_def_header = 'spectr/ene_horiz_spec'
!
      character(len=kchara), parameter                                  &
     &     :: ene_spec_x_head =  'ene_spec_x'
      character(len=kchara), parameter                                  &
     &     :: ene_spec_y_head =  'ene_spec_y'
      character(len=kchara), parameter                                  &
     &     :: ene_spec_xy_head = 'ene_spec_xy'
!
      character(len=kchara), parameter                                  &
     &      :: t_ene_spec_x_head =  'time_ene_spec_x'
      character(len=kchara), parameter                                  &
     &      :: t_ene_spec_y_head =  'time_ene_spec_y'
      character(len=kchara), parameter                                  &
     &      :: t_ene_spec_xy_head = 'time_ene_spec_xy'
!
!
      character(len=kchara), parameter                                  &
     &      :: horiz_ave_name =   'horiz_average.dat'
      character(len=kchara), parameter                                  &
     &      :: horiz_rms_name =   'horiz_rms.dat'
!
      character(len=kchara), parameter                                  &
     &      :: t_horiz_ave_name =  'time_horiz_average.dat'
      character(len=kchara), parameter                                  &
     &      :: t_horiz_rms_name =  'time_horiz_rms.dat'
!
!      subroutine s_set_spectr_file_name(i_step, output_data_name)
!      subroutine s_set_ene_spec_file_name(i_step, output_data_name)
!      subroutine s_set_horiz_ene_file_name(i_step, output_data_name)
!
!      subroutine set_ene_spec_plane_name(jz, yz_stacked_name,          &
!     &          xz_stacked_name, xyz_stacked_name)
!      subroutine set_ene_h_spec_plane_name(jz, istep, output_data_name)
!
!      subroutine set_ene_spec_plane_name(jz, istep, output_data_name)
!      subroutine set_mode_file_name(kx, ky, output_data_name)
!      subroutine set_picked_mode_file_name(kx, ky, output_data_name)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_spectr_file_name(i_step, output_data_name)
!
      use set_parallel_file_name
!
      character(len=kchara  ), intent(inout) ::  output_data_name
      integer(kind=kint ), intent(in)     ::  i_step
!
      character(len=kchara) :: fname_tmp1
!
!
      call add_int_suffix(i_step, spec_header, fname_tmp1)
      call add_dat_extension(fname_tmp1, output_data_name)
!
      end subroutine s_set_spectr_file_name
!
!-----------------------------------------------------------------------
!
      subroutine s_set_ene_spec_file_name(i_step, output_data_name)
!
      character(len=kchara  ), intent(inout) ::  output_data_name
      integer(kind=kint ), intent(in)     ::  i_step
!
      character(len=kchara) :: fname_tmp1
!
!
      call add_int_suffix(i_step, ene_header, fname_tmp1)
      call add_dat_extension(fname_tmp1, output_data_name)
!
      end subroutine s_set_ene_spec_file_name
!
!-----------------------------------------------------------------------
!
      subroutine s_set_horiz_ene_file_name(i_step, output_data_name)
!
      character(len=kchara  ), intent(inout) ::  output_data_name
      integer(kind=kint ), intent(in)     ::  i_step
!
      character(len=kchara) :: fname_tmp1
!
!
      call add_int_suffix(i_step, ene_h_header, fname_tmp1)
      call add_dat_extension(fname_tmp1, output_data_name)
!
      end subroutine s_set_horiz_ene_file_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_ene_spec_plane_name(jz, yz_stacked_name,           &
     &          xz_stacked_name, xyz_stacked_name)
!
      integer(kind=kint), intent(in) :: jz
      character(len=kchara), intent(inout) :: yz_stacked_name
      character(len=kchara), intent(inout) :: xz_stacked_name
      character(len=kchara), intent(inout) :: xyz_stacked_name
!
      character(len=kchara) :: fname_tmp1
!
!
       call add_int_suffix(jz, ene_spec_x_head, fname_tmp1)
       call add_dat_extension(fname_tmp1,yz_stacked_name)
!
       call add_int_suffix(jz, ene_spec_y_head, fname_tmp1)
       call add_dat_extension(fname_tmp1,xz_stacked_name)
!
       call add_int_suffix(jz, ene_spec_xy_head, fname_tmp1)
       call add_dat_extension(fname_tmp1,xyz_stacked_name)
!
      end subroutine set_ene_spec_plane_name
!
!-----------------------------------------------------------------------
!
      subroutine set_ene_h_spec_plane_name(jz, istep, output_data_name)
!
      integer(kind=kint), intent(in) :: jz, istep
      character(len=kchara), intent(inout) :: output_data_name
!
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(jz, ene_h_header, fname_tmp1)
      call add_int_suffix(istep, fname_tmp1, fname_tmp2)
      call add_dat_extension(fname_tmp2, output_data_name)
!
      end subroutine set_ene_h_spec_plane_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_mode_file_name(kx, ky, output_data_name)
!
      integer(kind=kint), intent(in) :: kx, ky
      character(len=kchara), intent(inout) :: output_data_name
      character(len=kchara), parameter :: file_header = 'spec_evo'
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(kx, file_header, fname_tmp1)
      call add_int_suffix(ky, fname_tmp1, fname_tmp2)
      call add_dat_extension(fname_tmp2, output_data_name)
!
      end subroutine set_mode_file_name
!
!-----------------------------------------------------------------------
!
      subroutine set_picked_mode_file_name(kx, ky, output_data_name)
!
      integer(kind=kint), intent(in) :: kx, ky
      character(len=kchara), intent(inout) :: output_data_name
      character(len=kchara), parameter :: file_header = 'picked_spectr'
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(kx, file_header, fname_tmp1)
      call add_int_suffix(ky, fname_tmp1, fname_tmp2)
      call add_dat_extension(fname_tmp2, output_data_name)
!
      end subroutine set_picked_mode_file_name
!
!-----------------------------------------------------------------------
!
      end module set_spectr_file_name
