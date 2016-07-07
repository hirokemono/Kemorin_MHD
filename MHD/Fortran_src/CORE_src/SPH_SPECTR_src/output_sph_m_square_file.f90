!> @file  output_sph_m_square_file.f90
!!      module output_sph_m_square_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_total_energy_to_screen(my_rank, istep, time)
!!      subroutine write_sph_vol_ave_file(istep, time, l_truncation,    &
!!     &          nlayer_ICB, nlayer_CMB, idx_rj_degree_zero)
!!
!!      subroutine write_sph_vol_ms_file(my_rank, istep, time,          &
!!     &          l_truncation, nlayer_ICB, nlayer_CMB)
!!      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time,   &
!!     &          l_truncation, nlayer_ICB, nlayer_CMB)
!!      subroutine write_sph_layer_ms_file(my_rank, istep, time,        &
!!     &          l_truncation, nlayer_ICB, nlayer_CMB)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
      module output_sph_m_square_file
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      File ID for mean square data
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!    output flag
!
!>      Output flag for layerd mean square data
      integer(kind = kint) :: iflag_layer_rms_spec =  0
!>      Output flag for volume mean square data
      integer(kind = kint) :: iflag_volume_rms_spec = 0
!>      Output flag for volume average data
      integer(kind = kint) :: iflag_volume_ave_sph =  0
!
!>      Output flag for spectrum with respect to degree
      integer(kind = kint) :: iflag_spectr_l =  1
!>      Output flag for spectrum with respect to order
      integer(kind = kint) :: iflag_spectr_m =  1
!>      Output flag for spectrum with respect to l-m
      integer(kind = kint) :: iflag_spectr_lm = 1
!>      Output flag for spectrum for axis-symmetric component
      integer(kind = kint) :: iflag_spectr_m0 = 1
!
!
!>      File prefix for volume mean square file
      character(len = kchara) :: fhead_rms_vol =    'sph_pwr_volume'
!>      File prefix for layered mean square file
      character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!>      File prefix for volume average file
      character(len = kchara) :: fhead_ave_vol =    'sph_ave_volume'
!
      private :: write_sph_volume_spec_file, write_sph_volume_pwr_file
      private :: write_sph_layer_pwr_file, write_sph_layer_spec_file
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_total_energy_to_screen(my_rank, istep, time)
!
      use m_phys_labels
      use m_rms_4_sph_spectr
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: i, icomp
!
!
      if(my_rank .gt. 0) return
      write(*,'(a10,i16,a10,1pe15.8)',advance='no')                     &
     &            'time step=',istep,'time=',time
!
      do i = 1, pwr1%num_fld_sq
        if (pwr1%pwr_name(i) .eq. fhd_velo) then
          icomp = pwr1%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_kin = ', pwr1%vol_sq(icomp)
          exit
        end if
      end do
!
      do i = 1, pwr1%num_fld_sq
        if (pwr1%pwr_name(i) .eq. fhd_magne) then
          icomp = pwr1%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_mag = ', pwr1%vol_sq(icomp)
          exit
        end if
      end do
      write(*,*)
!
      end subroutine write_total_energy_to_screen
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ave_file(istep, time, l_truncation,      &
     &          nlayer_ICB, nlayer_CMB, idx_rj_degree_zero)
!
      use m_rms_4_sph_spectr
      use sph_mean_spectr_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(idx_rj_degree_zero .eq. 0)  return
      if(pwr1%ntot_comp_sq .eq. 0)  return
!
      write(fname_rms, '(a,a4)') trim(fhead_ave_vol), '.dat'
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, pwr1%num_fld_sq, pwr1%ntot_comp_sq, pwr1%num_comp_sq,&
     &    pwr1%pwr_name, pwr1%nri_rms, nlayer_ICB, nlayer_CMB)
!
      write(id_file_rms,'(i15,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, pwr1%vol_ave(1:pwr1%ntot_comp_sq)
      close(id_file_rms)
!
      end subroutine write_sph_vol_ave_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_file(my_rank, istep, time,            &
     &          l_truncation, nlayer_ICB, nlayer_CMB)
!
      use m_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank, istep
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(pwr1%ntot_comp_sq .eq. 0)  return
!
      call add_dat_extension(fhead_rms_vol, fname_rms)
      write(mode_label,'(a)') 'EMPTY'
      call write_sph_volume_pwr_file                                    &
     &   (fname_rms, mode_label, istep, time,                           &
     &    l_truncation, nlayer_ICB, nlayer_CMB, pwr1%vol_sq)
!
      end subroutine write_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time,     &
     &          l_truncation, nlayer_ICB, nlayer_CMB)
!
      use m_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_volume_rms_spec .eq. 0)  return
      if(pwr1%ntot_comp_sq .eq. 0)  return
!
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
        write(mode_label,'(a)') 'degree'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, l_truncation, nlayer_ICB, nlayer_CMB,          &
     &      pwr1%vol_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms,'(a,a6)') trim(fhead_rms_vol), '_m.dat'
        write(mode_label,'(a)') 'order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, l_truncation, nlayer_ICB, nlayer_CMB,          &
     &      pwr1%vol_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms, '(a,a7)') trim(fhead_rms_vol), '_lm.dat'
        write(mode_label,'(a)') 'diff_deg_order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, l_truncation, nlayer_ICB, nlayer_CMB,          &
     &      pwr1%vol_lm)
      end if
!
      if(iflag_spectr_m0 .gt. izero) then
        write(fname_rms, '(a,a7)') trim(fhead_rms_vol), '_m0.dat'
        write(mode_label,'(a)') 'EMPTY'
        call write_sph_volume_pwr_file                                  &
     &     (fname_rms, mode_label, istep, time,                         &
     &      l_truncation, nlayer_ICB, nlayer_CMB, pwr1%vol_m0)
      end if
!
      end subroutine write_sph_vol_ms_spectr_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_ms_file(my_rank, istep, time,          &
     &          l_truncation, nlayer_ICB, nlayer_CMB)
!
      use m_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_layer_rms_spec .eq. izero)  return
      if(pwr1%ntot_comp_sq .eq. 0)  return
!
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id'
      call write_sph_layer_pwr_file                                     &
     &   (fname_rms, mode_label, istep, time,                           &
     &    l_truncation, nlayer_ICB, nlayer_CMB, pwr1%shl_sq)
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
        write(mode_label,'(a)') 'radial_id    degree'
        call write_sph_layer_spec_file                                  &
     &     (fname_rms, mode_label, istep, time,                         &
     &      l_truncation, nlayer_ICB, nlayer_CMB, pwr1%shl_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
        write(mode_label,'(a)') 'radial_id    order'
        call write_sph_layer_spec_file                                  &
     &     (fname_rms, mode_label, istep, time,                         &
     &      l_truncation, nlayer_ICB, nlayer_CMB, pwr1%shl_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
        write(mode_label,'(a)') 'radial_id    diff_deg_order'
        call write_sph_layer_spec_file                                  &
     &     (fname_rms, mode_label, istep, time,                         &
     &      l_truncation, nlayer_ICB, nlayer_CMB, pwr1%shl_lm)
      end if
!
      if(iflag_spectr_m0 .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_m0.dat'
        write(mode_label,'(a)') 'radial_id'
        call write_sph_layer_pwr_file                                   &
     &     (fname_rms, mode_label, istep, time,                         &
     &      l_truncation, nlayer_ICB, nlayer_CMB, pwr1%shl_m0)
      end if
!
      end subroutine write_sph_layer_ms_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_spec_file(fname_rms, mode_label,      &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          rms_sph_x)
!
      use m_rms_4_sph_spectr
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(0:l_truncation, pwr1%ntot_comp_sq)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, pwr1%num_fld_sq, pwr1%ntot_comp_sq, pwr1%num_comp_sq,&
     &    pwr1%pwr_name, pwr1%nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_volume_data(id_file_rms, istep, time,              &
     &    l_truncation, pwr1%ntot_comp_sq, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_volume_spec_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_pwr_file(fname_rms, mode_label,       &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          rms_sph_v)
!
      use m_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: rms_sph_v(pwr1%ntot_comp_sq)
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, pwr1%num_fld_sq, pwr1%ntot_comp_sq, pwr1%num_comp_sq,&
     &    pwr1%pwr_name, pwr1%nri_rms, nlayer_ICB, nlayer_CMB)
!
      write(id_file_rms,'(i16,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, rms_sph_v(1:pwr1%ntot_comp_sq)
      close(id_file_rms)
!
      end subroutine write_sph_volume_pwr_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_pwr_file(fname_rms, mode_label,        &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          rms_sph_x)
!
      use m_rms_4_sph_spectr
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: rms_sph_x(pwr1%nri_rms, pwr1%ntot_comp_sq)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, pwr1%num_fld_sq, pwr1%ntot_comp_sq, pwr1%num_comp_sq,&
     &    pwr1%pwr_name, pwr1%nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_layerd_power(id_file_rms, istep, time,             &
     &    pwr1%ntot_comp_sq, pwr1%nri_rms, pwr1%kr_4_rms, pwr1%r_4_rms, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_layer_pwr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_spec_file(fname_rms, mode_label,       &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          rms_sph_x)
!
      use m_rms_4_sph_spectr
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(pwr1%nri_rms,0:l_truncation, pwr1%ntot_comp_sq)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, pwr1%num_fld_sq, pwr1%ntot_comp_sq, pwr1%num_comp_sq,      &
     &    pwr1%pwr_name, pwr1%nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_layer_data(id_file_rms, istep, time, l_truncation, &
     &    pwr1%ntot_comp_sq, pwr1%nri_rms, pwr1%kr_4_rms, pwr1%r_4_rms, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_layer_spec_file
!
! -----------------------------------------------------------------------
!
      end module output_sph_m_square_file
