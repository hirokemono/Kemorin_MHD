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
!!     &         (id_file, flag_vol_ave, sph_IN)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!      subroutine select_output_sph_series_data                        &
!!     &         (id_file, flag_spectr, flag_vol_ave, sph_IN)
!!        integer(kind = kint), intent(in) :: id_file
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
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
     &         (id_file, flag_vol_ave, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(flag_vol_ave) then
        call write_sph_pwr_vol_head(id_file, sph_IN)
      else
        call write_sph_pwr_layer_head(id_file, sph_IN)
      end if
!
      end subroutine select_output_sph_pwr_head
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
      integer(kind = kint) :: i
!
!
      write(id_file,'(i16,1pE25.15e3)', ADVANCE='NO')                   &
     &                          sph_IN%i_step, sph_IN%time
      do i = 1, sph_IN%ntot_sph_spec
        write(id_file,'(1pE25.15e3)', ADVANCE='NO')                     &
     &                           sph_IN%spectr_IO(i,0,1)
      end do
      write(id_file,'(a)')
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
      write(fmt_txt,'(a20,i5,a16)')  '(i16,1pE25.15e3,i16,',            &
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
      write(fmt_txt,'(a31,i5,a16)')  '(i16,1pE25.15e3,i16,1pE25.15e3,', &
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
      write(fmt_txt,'(a35,i5,a16)')                                     &
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
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_vol_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(2i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')    'Lower boundary'
      write(id_file,'(i16,1pe23.15e3)') sph_IN%kr_inner, sph_IN%r_inner
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.15e3)') sph_IN%kr_outer, sph_IN%r_outer
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
      write(id_file,'(16i5)')                                           &
     &      sph_IN%ncomp_sph_spec(1:sph_IN%nfield_sph_spec)
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')  'Number_of_field, Number_of_components'
      write(id_file,'(2i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
      write(id_file,'(16i5)')                                           &
     &      sph_IN%ncomp_sph_spec(1:sph_IN%nfield_sph_spec)
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      end module write_sph_monitor_data
