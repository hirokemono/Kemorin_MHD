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
!!     &         (flag_gzip, id_file, flag_vol_ave, sph_OUT, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_vol_sph_data(id_file, sph_OUT, spectr_IO)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        real(kind = kreal), intent(in)                                &
!!     &           :: spectr_IO(sph_OUT%ntot_sph_spec)
!!      subroutine write_vol_spectr_data(id_file, sph_OUT)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        real(kind = kreal), intent(in)                                &
!!     &           :: spectr_IO(sph_OUT%ntot_sph_spec,0:sph_OUT%ltr_sph)
!!
!!      subroutine write_layer_sph_data(id_file, sph_OUT, spectr_IO)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        real(kind = kreal), intent(in)                                &
!!     &           :: spectr_IO(sph_OUT%ntot_sph_spec,sph_OUT%nri_sph)
!!
!!      subroutine write_layer_spectr_data(id_file, sph_OUT, spectr_IO)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        real(kind = kreal), intent(in)                                &
!!     &           :: spectr_IO(sph_OUT%ntot_sph_spec,                  &
!!     &                        0:sph_OUT%ltr_sph,sph_OUT%nri_sph)
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
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_output_sph_pwr_head                             &
     &         (flag_gzip, id_file, flag_vol_ave, sph_OUT, zbuf)
!
      use t_buffer_4_gzip
      use gz_open_sph_monitor_file
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_vol_ave) then
        call write_sph_pwr_vol_head(flag_gzip, id_file,                 &
     &                              sph_pwr_labels, sph_OUT, zbuf)
      else
        call write_sph_pwr_layer_head(flag_gzip, id_file,               &
     &                                sph_pwr_labels, sph_OUT, zbuf)
      end if
!
      end subroutine select_output_sph_pwr_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_vol_sph_data(id_file, sph_OUT, spectr_IO)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      real(kind = kreal), intent(in)                                    &
     &           :: spectr_IO(sph_OUT%ntot_sph_spec)
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(i16,1pE25.15e3)', ADVANCE='NO')                   &
     &                          sph_OUT%i_step, sph_OUT%time
      do i = 1, sph_OUT%ntot_sph_spec
        write(id_file,'(1pE25.15e3)', ADVANCE='NO') spectr_IO(i)
      end do
      write(id_file,'(a)')
!
      end subroutine write_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_vol_spectr_data(id_file, sph_OUT, spectr_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      real(kind = kreal), intent(in)                                    &
     &           :: spectr_IO(sph_OUT%ntot_sph_spec,0:sph_OUT%ltr_sph)
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
     &               spectr_IO(1:sph_OUT%ntot_sph_spec,lth)
      end do
!
      end subroutine write_vol_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_layer_sph_data(id_file, sph_OUT, spectr_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      real(kind = kreal), intent(in)                                    &
     &           :: spectr_IO(sph_OUT%ntot_sph_spec,sph_OUT%nri_sph)
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
     &         spectr_IO(1:sph_OUT%ntot_sph_spec,kr)
      end do
!
      end subroutine write_layer_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_layer_spectr_data(id_file, sph_OUT, spectr_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      real(kind = kreal), intent(in)                                    &
     &           :: spectr_IO(sph_OUT%ntot_sph_spec,                    &
     &                        0:sph_OUT%ltr_sph,sph_OUT%nri_sph)
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
     &      spectr_IO(1:sph_OUT%ntot_sph_spec,lth,kr)
        end do
      end do
!
      end subroutine write_layer_spectr_data
!
!   --------------------------------------------------------------------
!
      end module write_sph_monitor_data
