!>@file   sph_mean_spectr_header_IO.f90
!!@brief  module sph_mean_spectr_header_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine dup_sph_vol_spectr_header                            &
!!     &         (mode_label, ltr, nlayer_ICB, nlayer_CMB,              &
!!     &          ene_labels, sph_rj, v_pwr, sph_OUT)
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!      subroutine dup_sph_layer_spectr_header(mode_label,              &
!!     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(energy_label_param), intent(in) :: ene_labels
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
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
      subroutine dup_sph_vol_spectr_header                              &
     &         (mode_label, ltr, nlayer_ICB, nlayer_CMB,                &
     &          ene_labels, sph_rj, v_pwr, sph_OUT)
!
      use t_read_sph_spectra
      use m_time_labels
!
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i, icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = sph_rj%nidx_rj(1)
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = v_pwr%kr_inside
      sph_OUT%kr_outer = v_pwr%kr_outside
      sph_OUT%r_inner =  v_pwr%r_inside
      sph_OUT%r_outer =  v_pwr%r_outside
!
      sph_OUT%nfield_sph_spec = v_pwr%num_fld_sq
      sph_OUT%ntot_sph_spec =   v_pwr%ntot_comp_sq
      sph_OUT%num_time_labels = 2
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%num_time_labels = sph_OUT%num_time_labels + 1
        call alloc_sph_espec_name(sph_OUT)
        call alloc_sph_spectr_data(sph_OUT%ltr_sph, sph_OUT)
      else
        call alloc_sph_espec_name(sph_OUT)
        call alloc_sph_spectr_data(izero, sph_OUT)
      end if
!
      sph_OUT%ncomp_sph_spec(1:v_pwr%num_fld_sq)                        &
     &      = v_pwr%num_comp_sq(1:v_pwr%num_fld_sq)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%ene_sph_spec_name(sph_OUT%num_time_labels)              &
     &                                       = trim(mode_label)
!
!$omp parallel do
        do i = 0, sph_OUT%ltr_sph
          sph_OUT%i_mode(i) = i
        end do
!$omp end parallel do
      end if
!
      icou = sph_OUT%num_time_labels
      do i = 1, v_pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels,                             &
     &      v_pwr%num_comp_sq(i), v_pwr%pwr_name(i),                    &
     &      sph_OUT%ene_sph_spec_name(icou+1))
        icou = icou + v_pwr%num_comp_sq(i)
      end do
!
      end subroutine dup_sph_vol_spectr_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_sph_layer_spectr_header(mode_label,                &
     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
!
      use t_read_sph_spectra
      use m_time_labels
!
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      integer(kind = kint) :: i, icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = pwr%nri_rms
      sph_OUT%nri_dat = pwr%nri_rms
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
!
      sph_OUT%nfield_sph_spec = pwr%num_fld_sq
      sph_OUT%ntot_sph_spec =   pwr%ntot_comp_sq
      sph_OUT%num_time_labels = 4
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%num_time_labels = sph_OUT%num_time_labels + 1
        call alloc_sph_espec_name(sph_OUT)
        call alloc_sph_spectr_data(sph_OUT%ltr_sph, sph_OUT)
      else
        call alloc_sph_espec_name(sph_OUT)
        call alloc_sph_spectr_data(izero, sph_OUT)
      end if
!
      sph_OUT%ncomp_sph_spec(1:pwr%num_fld_sq)                          &
     &      = pwr%num_comp_sq(1:pwr%num_fld_sq)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Radius_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Radius'
      if(mode_label .ne. 'EMPTY') then
        sph_OUT%ene_sph_spec_name(sph_OUT%num_time_labels)              &
     &                                       = trim(mode_label)
!
!$omp parallel do
        do i = 0, sph_OUT%ltr_sph
          sph_OUT%i_mode(i) = i
        end do
!$omp end parallel do
      end if
!
      icou = sph_OUT%num_time_labels
      do i = 1, pwr%num_fld_sq
        call set_sph_rms_labels(ene_labels,                             &
     &      pwr%num_comp_sq(i), pwr%pwr_name(i),                        &
     &      sph_OUT%ene_sph_spec_name(icou+1))
        icou = icou + pwr%num_comp_sq(i)
      end do
!
      end subroutine dup_sph_layer_spectr_header
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_mean_sq_header(id_file, mode_label,      &
     &          ene_labels, sph_params, sph_rj, v_pwr)
!
      use t_read_sph_spectra
      use sph_power_spectr_data_text
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call dup_sph_vol_spectr_header                                    &
     &   (mode_label, sph_params%l_truncation,                          &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    ene_labels, sph_rj, v_pwr, sph_OUT)
!
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT,           &
     &                               len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &       sph_vol_spectr_header_text(len_tot, len_each,              &
     &                                  sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
!
      end subroutine write_sph_vol_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_mean_sq_header(id_file, mode_label,          &
     &          ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr)
!
      use t_read_sph_spectra
      use sph_power_spectr_data_text
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(energy_label_param), intent(in) :: ene_labels
      type(sph_mean_squares), intent(in) :: pwr
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call dup_sph_layer_spectr_header(mode_label,                      &
     &    ltr, nlayer_ICB, nlayer_CMB, ene_labels, pwr, sph_OUT)
!
      call len_sph_layer_spectr_header(sph_pwr_labels, sph_OUT,         &
     &                                 len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &      sph_layer_spectr_header_text(len_tot, len_each,             &
     &                                   sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
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
!
      end module sph_mean_spectr_header_IO

