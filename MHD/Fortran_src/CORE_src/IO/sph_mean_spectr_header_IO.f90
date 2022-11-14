!>@file   sph_mean_spectr_header_IO.f90
!!@brief  module sph_mean_spectr_header_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine write_sph_vol_mean_sq_header(id_file, mode_label,    &
!!     &          ene_labels, sph_params, sph_rj, v_pwr)
!!      subroutine write_sph_mean_sq_header(id_file, mode_label,        &
!!     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!!      logical function error_sph_vol_mean_sq_header                   &
!!     &               (id_file, mode_label, ene_labels,                &
!!     &                sph_params, sph_rj, v_pwr)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!
!!      subroutine set_sph_rms_labels_4_monitor                         &
!!     &         (ene_labels, pwr, pick_rms)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_rms
!!@endverbatim
!!
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module sph_mean_spectr_header_IO
!
      use m_precision
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_energy_label_parameters
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_mean_sq_header(id_file, mode_label,      &
     &          ene_labels, sph_params, sph_rj, v_pwr)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)')                                           &
     &                     sph_rj%nidx_rj(1), sph_params%l_truncation
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)')                                           &
     &                     sph_params%nlayer_ICB, sph_params%nlayer_CMB
      write(id_file,'(a)')    'Lower boundary'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     v_pwr%kr_inside, v_pwr%r_inside
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     v_pwr%kr_outside, v_pwr%r_outside
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')   v_pwr%num_fld_sq, v_pwr%ntot_comp_sq
      write(id_file,'(16i5)')   v_pwr%num_comp_sq(1:v_pwr%num_fld_sq)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, v_pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels,                             &
     &      v_pwr%num_comp_sq(i), v_pwr%pwr_name(i), labels(1))
        call write_multi_labels                                         &
     &     (id_file, v_pwr%num_comp_sq(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_vol_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_mean_sq_header(id_file, mode_label,          &
     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') pwr%nri_rms, ltr
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(3i16)') nlayer_ICB, nlayer_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')   pwr%num_fld_sq, pwr%ntot_comp_sq
      write(id_file,'(16i5)')   pwr%num_comp_sq(1:pwr%num_fld_sq)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, pwr%num_fld_sq
        call set_sph_rms_labels                                         &
     &     (ene_labels, pwr%num_comp_sq(i), pwr%pwr_name(i), labels(1))
        call write_multi_labels(id_file, pwr%num_comp_sq(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_mean_sq_header
!
!  --------------------------------------------------------------------
!
      logical function error_sph_vol_mean_sq_header                     &
     &               (id_file, mode_label, ene_labels,                  &
     &                sph_params, sph_rj, v_pwr)
!
      use write_field_labels
      use skip_comment_f
      use check_sph_monitor_header
!
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
!
      character(len = kchara), allocatable :: pwr_label(:)
      integer(kind = kint) :: i, icou
!
!
      allocate(pwr_label(v_pwr%ntot_comp_sq))
      icou = 1
      do i = 1, v_pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels, v_pwr%num_comp_sq(i),       &
     &                          v_pwr%pwr_name(i), pwr_label(icou))
        icou = icou + v_pwr%num_comp_sq(i)
      end do
!
      error_sph_vol_mean_sq_header                                      &
     &   = error_sph_vol_monitor_head(id_file, mode_label,              &
     &             sph_rj%nidx_rj(1), sph_params%l_truncation,          &
     &             sph_params%nlayer_ICB, sph_params%nlayer_CMB,        &
     &             v_pwr%kr_inside, v_pwr%r_inside,                     &
     &             v_pwr%kr_outside, v_pwr%r_outside,                   &
     &             v_pwr%num_fld_sq, v_pwr%num_comp_sq, v_pwr%pwr_name, &
     &             v_pwr%ntot_comp_sq, pwr_label)
      deallocate(pwr_label)
!
      end function error_sph_vol_mean_sq_header
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor                           &
     &         (ene_labels, pwr, pick_rms)
!
      use t_pickup_sph_spectr_data
      use add_direction_labels
!
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: i_fld, ist, ncomp
!
!
      do i_fld = 1, pwr%num_fld_sq
        ist =   pwr%istack_comp_sq(i_fld-1)
        ncomp = pwr%num_comp_sq(i_fld)
        call set_sph_rms_labels(ene_labels, ncomp, pwr%pwr_name(i_fld), &
     &      pick_rms%spectr_name(ist+1))
      end do
      pick_rms%ntot_comp_rj = pwr%ntot_comp_sq
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module sph_mean_spectr_header_IO

