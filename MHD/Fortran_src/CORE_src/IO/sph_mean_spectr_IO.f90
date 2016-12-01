!>@file   sph_mean_spectr_IO.f90
!!@brief  module sph_mean_spectr_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine open_sph_vol_mean_sq_file                            &
!!     &         (id_file, fname_rms, mode_label,                       &
!!     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,        &
!!     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,             &
!!     &          kr_inner, kr_outer, r_inner,  r_outer)
!!      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,&
!!     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,        &
!!     &          rms_name_rj, nri_rms, nlayer_ICB, nlayer_CMB)
!!      subroutine write_sph_volume_data(id_file, istep, time,          &
!!     &          ltr, ntot_rms_rj, rms_sph_x)
!!      subroutine write_sph_layerd_power(id_file, istep, time,         &
!!     &          ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph)
!!      subroutine write_sph_layer_data(id_file, istep, time,           &
!!     &          ltr, ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph_x)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module sph_mean_spectr_IO
!
      use m_precision
!
      implicit none
!
      private :: write_sph_mean_sq_header, write_sph_vol_mean_sq_header
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_sph_vol_mean_sq_file                              &
     &         (id_file, fname_rms, mode_label,                         &
     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,          &
     &          rms_name_rj, nri, nlayer_ICB, nlayer_CMB,               &
     &          kr_inner, kr_outer, r_inner,  r_outer)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      real(kind = kreal), intent(in) ::   r_inner,  r_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_vol_mean_sq_header(id_file, mode_label, ltr,       &
     &    num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,        &
     &    nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer,              &
     &    r_inner, r_outer)
!
      end subroutine open_sph_vol_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label,  &
     &          ltr, num_rms_rj, ntot_rms_rj, num_rms_comp_rj,          &
     &          rms_name_rj, nri_rms, nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
      integer(kind = kint), intent(in) :: nri_rms, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_mean_sq_header(id_file, mode_label, ltr,           &
     &    num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,        &
     &    nri_rms, nlayer_ICB, nlayer_CMB)
!
      end subroutine open_sph_mean_sq_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_mean_sq_header(id_file, mode_label, ltr, &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer,        &
     &          r_inner,  r_outer)
!
      use m_phys_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      real(kind = kreal), intent(in) ::   r_inner,  r_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri, ltr
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') nlayer_ICB, nlayer_CMB
      write(id_file,'(a)')    'Lower boudary'
      write(id_file,'(i16,1pe23.14e3)') kr_inner, r_inner
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.14e3)') kr_outer, r_outer
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')   num_rms_rj, ntot_rms_rj
      write(id_file,'(16i5)')   num_rms_comp_rj(1:num_rms_rj)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, num_rms_rj
        call set_sph_rms_labels(num_rms_comp_rj(i), rms_name_rj(i),     &
     &      labels(1))
        call write_multi_labels(id_file, num_rms_comp_rj(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_vol_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_mean_sq_header(id_file, mode_label, ltr,     &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri_rms, nlayer_ICB, nlayer_CMB)
!
      use m_phys_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: nri_rms, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri_rms, ltr
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(3i16)') nlayer_ICB, nlayer_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')   num_rms_rj, ntot_rms_rj
      write(id_file,'(16i5)')   num_rms_comp_rj(1:num_rms_rj)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, num_rms_rj
        call set_sph_rms_labels(num_rms_comp_rj(i), rms_name_rj(i),     &
     &      labels(1))
        call write_multi_labels(id_file, num_rms_comp_rj(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: num_rms_comp
      character(len = kchara), intent(in) :: rms_name
!
      character(len = kchara), intent(inout) :: labels(num_rms_comp)
!
!
      if ( rms_name .eq. fhd_velo) then
        write(labels(1),'(a)')   'K_ene_pol'
        write(labels(2),'(a)')   'K_ene_tor'
        write(labels(3),'(a)')   'K_ene'
!
      else if (rms_name .eq. fhd_magne) then
        write(labels(1),'(a)')   'M_ene_pol'
        write(labels(2),'(a)')   'M_ene_tor'
        write(labels(3),'(a)')   'M_ene'
!
      else if (rms_name .eq. fhd_filter_velo) then
        write(labels(1),'(a)')   'filter_KE_pol'
        write(labels(2),'(a)')   'filter_KE_tor'
        write(labels(3),'(a)')   'filter_KE'
!
      else if (rms_name .eq. fhd_filter_magne) then
        write(labels(1),'(a)')   'filter_ME_pol'
        write(labels(2),'(a)')   'filter_ME_tor'
        write(labels(3),'(a)')   'filter_ME'
!
      else if (num_rms_comp .eq. 1) then
        write(labels(1),'(a)')   trim(rms_name)
      else if (num_rms_comp .eq. 3) then
        call add_vector_power_sph_label(rms_name,                       &
     &          labels(1), labels(2), labels(3))
      else if (num_rms_comp .eq. 6) then
        call add_tensor_direction_label_rtp(rms_name,                   &
     &          labels(1), labels(2), labels(3), labels(4), labels(5),  &
     &          labels(6))
      end if
!
      end subroutine set_sph_rms_labels
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_volume_data(id_file, istep, time,            &
     &          ltr, ntot_rms_rj, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: ltr, ntot_rms_rj
      real(kind = kreal), intent(in) :: rms_sph_x(0:ltr, ntot_rms_rj)
!
      integer(kind = kint) :: lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do lm = 0, ltr
        write(id_file,fmt_txt) istep, time, lm,                         &
     &                         rms_sph_x(lm,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_volume_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layerd_power(id_file, istep, time,           &
     &          ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: nri_rms, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_rms(nri_rms)
      real(kind = kreal), intent(in) :: r_rms(nri_rms)
      real(kind = kreal), intent(in) :: rms_sph(nri_rms,ntot_rms_rj)
!
      integer(kind = kint) :: k
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', (ntot_rms_rj+1), '(1pe23.14e3),a1)'
      do k = 1, nri_rms
        write(id_file,fmt_txt) istep, time, kr_rms(k), r_rms(k),        &
     &                         rms_sph(k,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_layerd_power
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_data(id_file, istep, time,             &
     &          ltr, ntot_rms_rj, nri_rms, kr_rms, r_rms, rms_sph_x)
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: nri_rms, ltr, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_rms(nri_rms)
      real(kind = kreal), intent(in) :: r_rms(nri_rms)
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(nri_rms,0:ltr, ntot_rms_rj)
!
      integer(kind = kint) :: k, lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a35,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,1pe23.14e3,i16,',                       &
     &       ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do k = 1, nri_rms
        do lm = 0, ltr
          write(id_file,fmt_txt) istep, time, kr_rms(k), r_rms(k),      &
     &                           lm, rms_sph_x(k,lm,1:ntot_rms_rj)
        end do
      end do
!
      end subroutine write_sph_layer_data
!
! -----------------------------------------------------------------------
!
      end module sph_mean_spectr_IO
