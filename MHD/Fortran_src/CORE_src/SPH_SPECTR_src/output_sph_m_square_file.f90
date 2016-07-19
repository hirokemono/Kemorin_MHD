!> @file  output_sph_m_square_file.f90
!!      module output_sph_m_square_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_total_energy_to_screen                         &
!!     &         (my_rank, istep, time, pwr)
!!      subroutine write_sph_vol_ave_file                               &
!!     &         (istep, time, sph_params, sph_rj, pwr)
!!
!!      subroutine write_sph_vol_ms_file                                &
!!     &         (my_rank, istep, time, sph_params, pwr)
!!      subroutine write_sph_vol_ms_spectr_file                         &
!!     &         (my_rank, istep, time, sph_params, pwr)
!!      subroutine write_sph_layer_ms_file                              &
!!     &         (my_rank, istep, time, sph_params, pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_mean_squares), intent(in) :: pwr
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
      subroutine write_total_energy_to_screen                           &
     &         (my_rank, istep, time, pwr)
!
      use m_phys_labels
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: i, icomp
!
!
      if(my_rank .gt. 0) return
      write(*,'(a10,i16,a10,1pe15.8)',advance='no')                     &
     &            'time step=',istep,'time=',time
!
      do i = 1, pwr%num_fld_sq
        if (pwr%pwr_name(i) .eq. fhd_velo) then
          icomp = pwr%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_kin = ', pwr%vol_sq(icomp)
          exit
        end if
      end do
!
      do i = 1, pwr%num_fld_sq
        if (pwr%pwr_name(i) .eq. fhd_magne) then
          icomp = pwr%istack_comp_sq(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_mag = ', pwr%vol_sq(icomp)
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
      subroutine write_sph_vol_ave_file                                 &
     &         (istep, time, sph_params, sph_rj, pwr)
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use sph_mean_spectr_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      write(fname_rms, '(a,a4)') trim(fhead_ave_vol), '.dat'
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_mean_sq_file                                        &
     &   (id_file_rms, fname_rms, mode_label, sph_params%l_truncation,  &
     &    pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,            &
     &    pwr%pwr_name, pwr%nri_rms,                                    &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB)
!
      write(id_file_rms,'(i15,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, pwr%vol_ave(1:pwr%ntot_comp_sq)
      close(id_file_rms)
!
      end subroutine write_sph_vol_ave_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_file                                  &
     &         (my_rank, istep, time, sph_params, pwr)
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
      call add_dat_extension(fhead_rms_vol, fname_rms)
      write(mode_label,'(a)') 'EMPTY'
      call write_sph_volume_pwr_file                                    &
     &   (fname_rms, mode_label, istep, time, sph_params%l_truncation,  &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,    &
     &    pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,            &
     &    pwr%pwr_name, pwr%vol_sq)
!
      end subroutine write_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_spectr_file                           &
     &         (my_rank, istep, time, sph_params, pwr)
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_volume_rms_spec .eq. 0)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
        write(mode_label,'(a)') 'degree'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, sph_params%l_truncation,                       &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr%nri_rms, pwr%num_fld_sq, pwr%ntot_comp_sq,              &
     &      pwr%num_comp_sq, pwr%pwr_name, pwr%vol_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms,'(a,a6)') trim(fhead_rms_vol), '_m.dat'
        write(mode_label,'(a)') 'order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, sph_params%l_truncation,                       &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr%nri_rms, pwr%num_fld_sq, pwr%ntot_comp_sq,              &
     &      pwr%num_comp_sq, pwr%pwr_name, pwr%vol_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms, '(a,a7)') trim(fhead_rms_vol), '_lm.dat'
        write(mode_label,'(a)') 'diff_deg_order'
        call write_sph_volume_spec_file(fname_rms, mode_label,          &
     &      istep, time, sph_params%l_truncation,                       &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      pwr%nri_rms, pwr%num_fld_sq, pwr%ntot_comp_sq,              &
     &      pwr%num_comp_sq, pwr%pwr_name, pwr%vol_lm)
      end if
!
      if(iflag_spectr_m0 .gt. izero) then
        write(fname_rms, '(a,a7)') trim(fhead_rms_vol), '_m0.dat'
        write(mode_label,'(a)') 'EMPTY'
        call write_sph_volume_pwr_file                                  &
     &    (fname_rms, mode_label, istep, time, sph_params%l_truncation, &
     &     sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,   &
     &     pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,           &
     &     pwr%pwr_name, pwr%vol_m0)
      end if
!
      end subroutine write_sph_vol_ms_spectr_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_ms_file                                &
     &         (my_rank, istep, time, sph_params, pwr)
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_mean_squares), intent(in) :: pwr
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_layer_rms_spec .eq. izero)  return
      if(pwr%ntot_comp_sq .eq. 0)  return
!
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id  radius'
      call write_sph_layer_pwr_file                                     &
     &   (fname_rms, mode_label, istep, time, sph_params%l_truncation,  &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,    &
     &    pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,            &
     &    pwr%pwr_name, pwr%kr_4_rms, pwr%r_4_rms, pwr%shl_sq)
!
      if(iflag_spectr_l .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
        write(mode_label,'(a)') 'radial_id  radius  degree'
        call write_sph_layer_spec_file(fname_rms, mode_label,           &
     &      istep, time, sph_params%l_truncation,                       &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,  &
     &      pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,          &
     &      pwr%pwr_name, pwr%kr_4_rms, pwr%r_4_rms, pwr%shl_l)
      end if
!
      if(iflag_spectr_m .gt. izero) then
        write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
        write(mode_label,'(a)') 'radial_id  radius  order'
        call write_sph_layer_spec_file                                  &
     &    (fname_rms, mode_label, istep, time, sph_params%l_truncation, &
     &     sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,   &
     &     pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,           &
     &     pwr%pwr_name, pwr%kr_4_rms, pwr%r_4_rms, pwr%shl_m)
      end if
!
      if(iflag_spectr_lm .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
        write(mode_label,'(a)') 'radial_id  radius  diff_deg_order'
        call write_sph_layer_spec_file                                  &
     &    (fname_rms, mode_label, istep, time, sph_params%l_truncation, &
     &     sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,   &
     &     pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,           &
     &     pwr%pwr_name, pwr%kr_4_rms, pwr%r_4_rms, pwr%shl_lm)
      end if
!
      if(iflag_spectr_m0 .gt. izero) then
        write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_m0.dat'
        write(mode_label,'(a)') 'radial_id  radius'
        call write_sph_layer_pwr_file                                   &
     &    (fname_rms, mode_label, istep, time, sph_params%l_truncation, &
     &     sph_params%nlayer_ICB, sph_params%nlayer_CMB, pwr%nri_rms,   &
     &     pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%num_comp_sq,           &
     &     pwr%pwr_name, pwr%kr_4_rms, pwr%r_4_rms, pwr%shl_m0)
      end if
!
      end subroutine write_sph_layer_ms_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_spec_file(fname_rms, mode_label,      &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          nri_rms, num_fld_sq, ntot_comp_sq, num_comp_sq,         &
     &          pwr_name, rms_sph_x)
!
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      character (len=kchara), intent(in) :: pwr_name(num_fld_sq)
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(0:l_truncation, ntot_comp_sq)
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, num_fld_sq, ntot_comp_sq, num_comp_sq,          &
     &    pwr_name, nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_volume_data(id_file_rms, istep, time,              &
     &    l_truncation, ntot_comp_sq, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_volume_spec_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_volume_pwr_file(fname_rms, mode_label,       &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          nri_rms, num_fld_sq, ntot_comp_sq, num_comp_sq,         &
     &          pwr_name, rms_sph_v)
!
      use set_parallel_file_name
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      character (len=kchara), intent(in) :: pwr_name(num_fld_sq)
      real(kind = kreal), intent(in) :: rms_sph_v(ntot_comp_sq)
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, num_fld_sq, ntot_comp_sq, num_comp_sq,          &
     &    pwr_name, nri_rms, nlayer_ICB, nlayer_CMB)
!
      write(id_file_rms,'(i16,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, rms_sph_v(1:ntot_comp_sq)
      close(id_file_rms)
!
      end subroutine write_sph_volume_pwr_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_pwr_file(fname_rms, mode_label,        &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          nri_rms, num_fld_sq, ntot_comp_sq, num_comp_sq,         &
     &          pwr_name, kr_4_rms, r_4_rms, rms_sph_x)
!
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      integer(kind = kint), intent(in) :: kr_4_rms(nri_rms)
      character (len=kchara), intent(in) :: pwr_name(num_fld_sq)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: r_4_rms(nri_rms)
      real(kind = kreal), intent(in) :: rms_sph_x(nri_rms,ntot_comp_sq)
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, num_fld_sq, ntot_comp_sq, num_comp_sq,          &
     &    pwr_name, nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_layerd_power(id_file_rms, istep, time,             &
     &    ntot_comp_sq, nri_rms, kr_4_rms, r_4_rms, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_layer_pwr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_spec_file(fname_rms, mode_label,       &
     &          istep, time, l_truncation, nlayer_ICB, nlayer_CMB,      &
     &          nri_rms, num_fld_sq, ntot_comp_sq, num_comp_sq,         &
     &          pwr_name, kr_4_rms, r_4_rms, rms_sph_x)
!
      use sph_mean_spectr_IO
!
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      integer(kind = kint), intent(in) :: kr_4_rms(nri_rms)
      character (len=kchara), intent(in) :: pwr_name(num_fld_sq)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: r_4_rms(nri_rms)
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(nri_rms,0:l_truncation,ntot_comp_sq)
!
      character(len=kchara), intent(in) :: fname_rms, mode_label
!
      call open_sph_mean_sq_file(id_file_rms, fname_rms, mode_label,    &
     &    l_truncation, num_fld_sq, ntot_comp_sq, num_comp_sq,          &
     &    pwr_name, nri_rms, nlayer_ICB, nlayer_CMB)
      call write_sph_layer_data(id_file_rms, istep, time, l_truncation, &
     &    ntot_comp_sq, nri_rms, kr_4_rms, r_4_rms, rms_sph_x)
      close(id_file_rms)
!
      end subroutine write_sph_layer_spec_file
!
! -----------------------------------------------------------------------
!
      end module output_sph_m_square_file
