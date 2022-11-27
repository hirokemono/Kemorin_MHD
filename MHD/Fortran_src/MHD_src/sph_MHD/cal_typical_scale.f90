!>@file   cal_typical_scale.f90
!!@brief      module cal_typical_scale
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate lengh scale data
!!
!!@verbatim
!!      subroutine set_ctl_typical_scale_params                         &
!!     &         (scale_file_prefix_t, typ_scale_file_fmt, rj_fld, tsl)
!!        type(read_character_item), intent(in) :: scale_file_prefix_t
!!        type(read_character_item), intent(in) :: typ_scale_file_fmt
!!        type(phys_data), intent(in) :: rj_fld
!!        type(typical_scale_data), intent(inout) :: tsl
!!      subroutine write_typical_scales(i_step, time, pwr, tsl)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(in) :: tsl
!!      subroutine cal_typical_scales(pwr, tsl)
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module cal_typical_scale
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_sph_typical_scales
      use t_field_labels
      use t_phys_data
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
!
      implicit none
!
      private :: find_rms_address_4_kene, find_rms_address_4_mene
      private :: s_cal_typical_scale
!
      integer(kind = kint), parameter, private :: id_scale = 36
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_typical_scale_params                           &
     &         (scale_file_prefix_t, typ_scale_file_fmt, rj_fld, tsl)
!
      use t_phys_data
      use t_control_array_character
      use t_multi_flag_labels
      use m_base_field_labels
      use m_field_file_format_labels
!
      type(read_character_item), intent(in) :: scale_file_prefix_t
      type(read_character_item), intent(in) :: typ_scale_file_fmt
      type(phys_data), intent(in) :: rj_fld
      type(typical_scale_data), intent(inout) :: tsl
!
      integer(kind = kint) :: i
      character(len = kchara) :: input_flag
!
!    Turn On Nusselt number if temperature gradient is there
      tsl%iflag_ub_scales = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
        if(rj_fld%phys_name(i) .eq. velocity%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
      end do
!
      if(scale_file_prefix_t%iflag .gt. 0) then
        tsl%scale_prefix = scale_file_prefix_t%charavalue
      else
        tsl%iflag_ub_scales = 0
      end if
!
      tsl%flag_gzip_scale = .FALSE.
      if(tsl%iflag_ub_scales .gt. 0) then
        if(typ_scale_file_fmt%iflag .gt. 0) then
          input_flag = typ_scale_file_fmt%charavalue
          if(check_mul_flags(input_flag, gzip_flags))                   &
     &                           tsl%flag_gzip_scale = .TRUE.
        end if
      end if
!
      end subroutine set_ctl_typical_scale_params
!
! -----------------------------------------------------------------------
!
      subroutine write_typical_scales                                   &
     &         (i_step, time, sph_params, sph_rj, sph_bc_U, pwr, tsl)
!
      use t_buffer_4_gzip
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use sph_monitor_data_text
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(in) :: tsl
!
      logical :: flag_gzip_lc
      type(buffer_4_gzip) :: zbuf_t
      real(kind=kreal), allocatable :: d_rj_out(:)
      integer(kind = kint) :: icou
!
!
      if(tsl%iflag_ub_scales .le. izero) return
      if((tsl%icomp_kene + tsl%icomp_mene) .le. 0) return
      if(my_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      allocate(d_rj_out(tsl%num_lscale))
      icou = 0
      if(tsl%icomp_kene .gt. 0) then
        d_rj_out(icou+1) = tsl%dl_kin
        d_rj_out(icou+2) = tsl%dm_kin
        d_rj_out(icou+3) = tsl%dlm_kin
        icou = icou + 3
      end if
      if(tsl%icomp_mene .gt. 0) then
        d_rj_out(icou+1) = tsl%dl_mag
        d_rj_out(icou+2) = tsl%dm_mag
        d_rj_out(icou+3) = tsl%dlm_mag
      end if
!
      flag_gzip_lc = tsl%flag_gzip_scale
      call open_typical_scale_file                                      &
     &   (id_scale, sph_params%l_truncation, sph_rj%nidx_rj(1),         &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out,                              &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0), tsl,                    &
     &    flag_gzip_lc, zbuf_t)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_scale,             &
     &    volume_pwr_data_text(i_step, time, tsl%num_lscale, d_rj_out), &
     &    zbuf_t)
      close(id_scale)
!
      deallocate(d_rj_out)
!
      end subroutine write_typical_scales
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     logical function error_typical_scale_header(sph_params, sph_rj,    &
    &                                            sph_bc_U, pwr, tsl)
!
      use t_buffer_4_gzip
      use t_rms_4_sph_spectr
      use t_read_sph_spectra
      use set_parallel_file_name
      use gz_open_sph_monitor_file
      use check_sph_monitor_header
      use compare_sph_monitor_header
      use sph_power_spectr_data_text
      use select_gz_stream_file_IO
      use gz_open_sph_monitor_file
      use sel_gz_input_sph_mtr_head
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(in) :: tsl
!
      character(len = kchara) :: file_name, base_name
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
      character, pointer:: FPz_fp
      logical :: flag_gzip_lc, flag_miss
      logical :: flag_gzip_t= .TRUE.
      type(read_sph_spectr_data) :: sph_IN_t, sph_OUT_t
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
      type(buffer_4_gzip) :: zbuf_t
!
!
      error_typical_scale_header = .FALSE.
      if(tsl%iflag_ub_scales .le. izero) return
      if(tsl%num_lscale .le. 0) return
      if(my_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      flag_gzip_lc = flag_gzip_t
      base_name = add_dat_extension(tsl%scale_prefix)
      call check_gzip_or_ascii_file(base_name, file_name,               &
     &                              flag_gzip_lc, flag_miss)
      if(flag_miss) go to 99
!
      call sel_open_read_gz_stream_file(FPz_fp, id_scale,               &
     &                                file_name, flag_gzip_lc, zbuf_t)
      call s_select_input_sph_series_head(FPz_fp, id_scale,             &
     &    flag_gzip_lc, flag_current_fmt, spectr_off, volume_on,        &
     &    sph_lbl_IN_t, sph_IN_t, zbuf_t)
      call sel_close_read_gz_stream_file(FPz_fp, id_scale,              &
     &                                   flag_gzip_lc, zbuf_t)
      sph_IN_t%nri_dat = 1
!
      call dup_typ_scale_head_params                                    &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out,                              &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0), tsl, sph_OUT_t)
      error_typical_scale_header                                        &
     &  =  .not. cmp_sph_volume_monitor_heads(sph_lbl_IN_t, sph_IN_t,   &
     &                                  sph_pwr_labels, sph_OUT_t)
      call dealloc_sph_espec_name(sph_OUT_t)
!
!
  99  continue
      write(*,*) 'No typical scale file'
!
      open(id_scale, file=file_name,                                    &
     &     FORM='UNFORMATTED',ACCESS='STREAM')
      call dup_typ_scale_head_params                                    &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out,                              &
     &    sph_bc_U%r_ICB(0), sph_bc_U%r_CMB(0), tsl, sph_OUT_t)
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT_t,         &
     &                               len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_scale,             &
     &    sph_vol_spectr_header_text(len_tot, len_each,                 &
     &                              sph_pwr_labels, sph_OUT_t),         &
     &    zbuf_t)
      close(id_scale)
      call dealloc_sph_espec_name(sph_OUT_t)
      return
!
      end function error_typical_scale_header
!
! -----------------------------------------------------------------------
!
      subroutine init_typical_scales(rj_fld, pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
      integer(kind = kint) :: icou
!
!
      if(tsl%icomp_kene .le. 0)                                         &
     &          tsl%icomp_kene = find_rms_address_4_kene(pwr, rj_fld)
      if(tsl%icomp_mene .le. 0)                                         &
     &          tsl%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
!
      icou = 0
      if(tsl%icomp_kene .gt. 0) icou = icou + 3
      if(tsl%icomp_mene .gt. 0) icou = icou + 3
      call alloc_typical_scale_data(icou, tsl)
!
      icou = 0
      if(tsl%icomp_kene .gt. 0) then
        tsl%lscale_name(icou+1) = 'lscale_flow_degree'
        tsl%lscale_name(icou+2) = 'lscale_flow_order'
        tsl%lscale_name(icou+3) = 'lscale_flow_diff_lm'
        icou = icou + 3
      end if
      if(tsl%icomp_mene .gt. 0) then
        tsl%lscale_name(icou+1) = 'lscale_magnetic_degree'
        tsl%lscale_name(icou+2) = 'lscale_magnetic_order'
        tsl%lscale_name(icou+3) = 'lscale_magnetic_diff_lm'
        icou = icou + 3
      end if
!
      end subroutine init_typical_scales
!
! -----------------------------------------------------------------------
!
      subroutine cal_typical_scales(pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(inout) :: tsl
!
!
      if(tsl%num_lscale .le. 0) return
      if(tsl%iflag_ub_scales .le. izero) return
!
!
      if(tsl%icomp_kene .gt. 0) then
        call s_cal_typical_scale(tsl%icomp_kene, pwr%v_spectr(1),       &
     &                           tsl%dl_kin, tsl%dm_kin, tsl%dlm_kin)
      end if
      if(tsl%icomp_mene .gt. 0) then
        call s_cal_typical_scale(tsl%icomp_mene, pwr%v_spectr(1),       &
     &                           tsl%dl_mag, tsl%dm_mag, tsl%dlm_mag)
      end if
!
      end subroutine cal_typical_scales
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_kene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      find_rms_address_4_kene = 0
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        if(rj_fld%phys_name(i_fld) .eq. velocity%name) then
          find_rms_address_4_kene =  pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
      end function find_rms_address_4_kene
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_mene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        find_rms_address_4_mene = 0
        if(rj_fld%phys_name(i_fld) .eq. magnetic_field%name) then
          find_rms_address_4_mene = pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
!
      end function find_rms_address_4_mene
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_typical_scale(icomp_mene, v_pwr,                 &
     &                               dl_mag, dm_mag, dlm_mag)
!
      use t_sph_volume_mean_square
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: icomp_mene
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      real(kind = kreal), intent(inout) :: dl_mag, dm_mag, dlm_mag
!
      integer(kind = kint) :: l, m, lm
!
!
      dl_mag = 0.0d0
      dm_mag = 0.0d0
      dlm_mag = 0.0d0
      if(my_rank .eq. v_pwr%irank_l) then
        do l = 1, v_pwr%ltr
          dl_mag =  dl_mag +  dble(l) * v_pwr%v_l(l,icomp_mene+2)
        end do
      end if
      if(my_rank .eq. v_pwr%irank_lm) then
        do lm = 1, v_pwr%ltr
          dlm_mag = dlm_mag + dble(lm) * v_pwr%v_lm(lm,icomp_mene+2)
        end do
      end if
      if(my_rank .eq. v_pwr%irank_m) then
        do m = 1, v_pwr%ltr
          dm_mag =  dm_mag +  dble(m) * v_pwr%v_m(m,icomp_mene+2)
        end do
      end if
      call calypso_mpi_bcast_one_real(dl_mag, v_pwr%irank_l)
      call calypso_mpi_bcast_one_real(dlm_mag, v_pwr%irank_lm)
!
      if(my_rank .eq. v_pwr%irank_m) then
        dm_mag =  dm_mag /  v_pwr%v_sq(icomp_mene+2)
        dl_mag =  dl_mag /  v_pwr%v_sq(icomp_mene+2)
        dlm_mag = dlm_mag / v_pwr%v_sq(icomp_mene+2)
      end if
!
      end subroutine s_cal_typical_scale
!
! -----------------------------------------------------------------------
!
      end module cal_typical_scale
