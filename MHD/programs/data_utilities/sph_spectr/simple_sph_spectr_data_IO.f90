!>@file   simple_sph_spectr_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time spectrum data IO for utilities
!!
!!@verbatim
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, flag_spectr, flag_vol_ave, sph_IN)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
!
      module simple_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
      private :: write_vol_sph_data, write_vol_spectr_data
      private :: write_layer_sph_data, write_layer_spectr_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_series_data                          &
     &         (id_file, flag_spectr, flag_vol_ave, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          call write_vol_spectr_data(id_file, sph_IN)
        else
          call write_layer_spectr_data(id_file, sph_IN)
        end if
      else
        if(flag_vol_ave) then
          call write_vol_sph_data(id_file, sph_IN)
        else
          call write_layer_sph_data(id_file, sph_IN)
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
      subroutine write_vol_sph_data(id_file, sph_IN)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a16,i5,a15)') '(i16,1pE25.15e3,',                 &
     &                  sph_IN%ntot_sph_spec, '(1p255E25.15e3))'
      write(id_file,fmt_txt) sph_IN%i_step, sph_IN%time,                &
     &                  sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,1)
!
      end subroutine write_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_vol_spectr_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: lth
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i5,a15)')  '(i16,1pE25.15e3,i16,',            &
     &                     sph_IN%ntot_sph_spec, '(1p255E25.15e3))'
      do lth = 0, sph_IN%ltr_sph
        write(id_file,fmt_txt)                                          &
     &               sph_IN%i_step, sph_IN%time, sph_IN%i_mode(lth),    &
     &               sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,1)
      end do
!
      end subroutine write_vol_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_layer_sph_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: kr
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a31,i5,a15)')  '(i16,1pE25.15e3,i16,1pE25.15e3,', &
     &                     sph_IN%ntot_sph_spec, '(1p255E25.15e3))'
      do kr = 1, sph_IN%nri_sph
        write(id_file,fmt_txt) sph_IN%i_step, sph_IN%time,              &
     &         sph_IN%kr_sph(kr), sph_IN%r_sph(kr),                     &
     &         sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
!
      end subroutine write_layer_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_layer_spectr_data(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: kr, lth
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i5,a15)')                                     &
     &            '(i16,1pE25.15e3,i16,1pE25.15e3,i16,',                &
     &              sph_IN%ntot_sph_spec, '(1p255E25.15e3))'
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          write(id_file,fmt_txt) sph_IN%i_step, sph_IN%time,            &
     &         sph_IN%kr_sph(kr), sph_IN%r_sph(kr), sph_IN%i_mode(lth), &
     &         sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
!
      end subroutine write_layer_spectr_data
!
!   --------------------------------------------------------------------
!
      end module simple_sph_spectr_data_IO
