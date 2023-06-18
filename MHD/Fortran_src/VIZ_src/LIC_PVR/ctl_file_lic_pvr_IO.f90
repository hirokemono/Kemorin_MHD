!>@file   ctl_file_lic_pvr_IO.f90
!!@brief  module ctl_file_lic_pvr_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine sel_read_control_lic_pvr(id_control, hd_lic_ctl,     &
!!     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, c_buf)
!!      subroutine read_control_lic_pvr_file(id_control, fname_lic_ctl, &
!!     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_lic_ctl
!!        character(len = kchara), intent(inout) :: fname_lic_ctl
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
!!        type(buffer_for_control), intent(inout) :: c_buf
!!
!!      subroutine sel_write_control_lic_pvr(id_control, hd_lic_ctl,    &
!!     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, level)
!!      subroutine write_control_lic_pvr_file(id_control, fname_lic_ctl,&
!!     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_lic_ctl
!!        character(len = kchara), intent(in) :: fname_lic_ctl
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl_type
!!        integer(kind = kint), intent(inout) :: level
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  lic_file_prefix      pvr_temp
!!  lic_output_format    PNG
!!  monitoring_mode      YES
!!
!!  streo_imaging        YES
!!  anaglyph_switch      NO
!!  quilt_3d_imaging     YES
!!!
!!  begin LIC_ctl
!!   ...
!!  end  LIC_ctl
!!!
!!  begin plot_area_ctl
!!   ...
!!  end  plot_area_ctl
!!!
!!  begin view_transform_ctl
!!   ...
!!  end view_transform_ctl
!!
!!  begin lighting_ctl
!!   ...
!!  end lighting_ctl
!!
!!  begin colormap_ctl
!!   ...
!!  end   colormap_ctl
!!!
!!  begin colorbar_ctl
!!   ...
!!  end colorbar_ctl
!!!
!!  begin quilt_image_ctl
!!   ...
!!  end quilt_image_ctl
!!
!!  begin snapshot_movie_ctl
!!   ...
!!  end snapshot_movie_ctl
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_lic_pvr_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_pvr
      use t_control_data_LIC
      use skip_comment_f
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_control_lic_pvr(id_control, hd_lic_ctl,       &
     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, c_buf)
!
      use ctl_data_lic_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(inout) :: fname_lic_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_file_flag(c_buf, hd_lic_ctl)) then
        fname_lic_ctl = third_word(c_buf)
        write(*,'(a)', ADVANCE='NO') ' is read from file: '
        call read_control_lic_pvr_file(id_control+2, fname_lic_ctl,     &
     &      hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
      else if(check_begin_flag(c_buf, hd_lic_ctl)) then
          fname_lic_ctl = 'NO_FILE'
!
        write(*,'(a)') ' is included'
        call read_lic_pvr_ctl(id_control, hd_lic_ctl,                   &
     &                        pvr_ctl_type, lic_ctl_type, c_buf)
      end if
!
      end subroutine sel_read_control_lic_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_lic_pvr_file(id_control, fname_lic_ctl,   &
     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!
      use ctl_data_lic_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(in) :: fname_lic_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
!
      type(buffer_for_control) :: c_buf1
!
!
      if(fname_lic_ctl .eq. 'NO_FILE') return
!
      c_buf1%level = 0
      write(*,*) 'LIC control file: ', trim(fname_lic_ctl)
      open(id_control, file=fname_lic_ctl, status='old')
      do
        call load_one_line_from_control(id_control, hd_lic_ctl, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_lic_pvr_ctl                                           &
     &     (id_control, hd_lic_ctl, pvr_ctl_type, lic_ctl_type, c_buf1)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_lic_pvr_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_control_lic_pvr(id_control, hd_lic_ctl,      &
     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, level)
!
      use ctl_data_lic_pvr_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(in) :: fname_lic_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(fname_lic_ctl, 'NO_FILE')) then
        write(*,'(a)') ' is included.'
        call write_lic_pvr_ctl(id_control, hd_lic_ctl,                  &
     &                         pvr_ctl_type, lic_ctl_type, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)') ' should be written to... ',                    &
     &                  trim(fname_lic_ctl)
        call write_lic_pvr_ctl(id_control, hd_lic_ctl,                  &
     &                         pvr_ctl_type, lic_ctl_type, level)
      else
        write(*,'(2a)') ' is written to... ', trim(fname_lic_ctl)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_lic_ctl, fname_lic_ctl)
        call write_control_lic_pvr_file(id_control+2, fname_lic_ctl,    &
     &      hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
      end if
!
      end subroutine sel_write_control_lic_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_lic_pvr_file(id_control, fname_lic_ctl,  &
     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!
      use ctl_data_lic_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(in) :: fname_lic_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type
!
      integer(kind = kint) :: level
!
      level = 0
      open(id_control, file=fname_lic_ctl, status='old')
      call write_lic_pvr_ctl(id_control, hd_lic_ctl,                    &
     &                       pvr_ctl_type, lic_ctl_type, level)
      close(id_control)
!
      end subroutine write_control_lic_pvr_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_lic_pvr_IO
