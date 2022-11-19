!>@file   write_sph_monitor_data.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine select_output_sph_pwr_head                           &
!!     &         (id_file, flag_vol_ave, sph_OUT)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, flag_spectr, flag_vol_ave, sph_OUT)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!@endverbatim
!
      module write_sph_monitor_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
!
      implicit none
!
      private :: write_vol_sph_data, write_vol_spectr_data
      private :: write_layer_sph_data, write_layer_spectr_data
      private :: write_sph_pwr_vol_head, write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_head                             &
     &         (id_file, flag_vol_ave, sph_OUT)
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      call sph_mean_squre_header_labels(sph_OUT)
!
      if(flag_vol_ave) then
        call write_sph_pwr_vol_head(id_file, sph_OUT)
      else
        call write_sph_pwr_layer_head(id_file, sph_OUT)
      end if
!
      end subroutine select_output_sph_pwr_head
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_series_data                          &
     &         (id_file, flag_spectr, flag_vol_ave, sph_OUT)
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          call write_vol_spectr_data(id_file, sph_OUT)
        else
          call write_layer_spectr_data(id_file, sph_OUT)
        end if
      else
        if(flag_vol_ave) then
          call write_vol_sph_data(id_file, sph_OUT)
        else
          call write_layer_sph_data(id_file, sph_OUT)
        end if
      end if
!
      end subroutine select_output_sph_series_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_volume_pwr_sph(id_file, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,         &
     &             sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,1)
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_volume_pwr_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_vol_sph_data(id_file, sph_OUT)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(i16,1pE25.15e3)', ADVANCE='NO')                   &
     &                          sph_OUT%i_step, sph_OUT%time
      do i = 1, sph_OUT%ntot_sph_spec
        write(id_file,'(1pE25.15e3)', ADVANCE='NO')                     &
     &                           sph_OUT%spectr_IO(i,0,1)
      end do
      write(id_file,'(a)')
!
      end subroutine write_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_vol_spectr_data(id_file, sph_OUT)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: lth
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i5,a16)')  '(i16,1pE25.15e3,i16,',            &
     &                     sph_OUT%ntot_sph_spec, '(1p255E25.15e3))'
      do lth = 0, sph_OUT%ltr_sph
        write(id_file,fmt_txt)                                          &
     &               sph_OUT%i_step, sph_OUT%time, sph_OUT%i_mode(lth), &
     &               sph_OUT%spectr_IO(1:sph_OUT%ntot_sph_spec,lth,1)
      end do
!
      end subroutine write_vol_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_layer_sph_data(id_file, sph_OUT)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: kr
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a31,i5,a16)')  '(i16,1pE25.15e3,i16,1pE25.15e3,', &
     &                     sph_OUT%ntot_sph_spec, '(1p255E25.15e3))'
      do kr = 1, sph_OUT%nri_sph
        write(id_file,fmt_txt) sph_OUT%i_step, sph_OUT%time,            &
     &         sph_OUT%kr_sph(kr), sph_OUT%r_sph(kr),                   &
     &         sph_OUT%spectr_IO(1:sph_OUT%ntot_sph_spec,0,kr)
      end do
!
      end subroutine write_layer_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_layer_spectr_data(id_file, sph_OUT)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: kr, lth
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i5,a16)')                                     &
     &            '(i16,1pE25.15e3,i16,1pE25.15e3,i16,',                &
     &              sph_OUT%ntot_sph_spec, '(1p255E25.15e3))'
      do kr = 1, sph_OUT%nri_sph
        do lth = 0, sph_OUT%ltr_sph
          write(id_file,fmt_txt) sph_OUT%i_step, sph_OUT%time,          &
     &      sph_OUT%kr_sph(kr), sph_OUT%r_sph(kr), sph_OUT%i_mode(lth), &
     &      sph_OUT%spectr_IO(1:sph_OUT%ntot_sph_spec,lth,kr)
        end do
      end do
!
      end subroutine write_layer_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_vol_head(id_file, sph_OUT)
!
      use sph_power_spectr_data_text
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call len_sph_vol_spectr_header(sph_OUT, len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &       sph_vol_spectr_header_text(len_tot, len_each, sph_OUT)
!
      end subroutine write_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(id_file, sph_OUT)
!
      use sph_power_spectr_data_text
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call len_sph_layer_spectr_header(sph_OUT, len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &      sph_layer_spectr_header_text(len_tot, len_each, sph_OUT)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      end module write_sph_monitor_data
