!>@file   dup_detailed_dbench_to_IO.f90
!!@brief  module dup_detailed_dbench_to_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine write_detailed_dbench_file                           &
!!     &         (my_rank, sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, &
!!     &          time_d, bench)
!!      subroutine dup_detail_dbench_header_to_IO                       &
!!     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_vol_mean_squares), intent(in) :: v_pwr
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(time_data), intent(in) :: time_d
!!        type(dynamobench_monitor), intent(in) :: bench
!!      subroutine dup_detail_dbench_monitor_data(sph_bc_U, sph_bc_B,   &
!!     &          ipol_base, bench, num_out, detail_out)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(dynamobench_monitor), intent(in) :: bench
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module dup_detailed_dbench_to_IO
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_phys_address
      use t_base_field_labels
      use t_sph_volume_mean_square
      use t_field_4_dynamobench
      use t_read_sph_spectra
      use t_time_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_dbench = 36
!
      private :: cnt_detail_dbench_monitor_name
      private :: copy_detail_dbench_monitor_name
!
      type(sph_spectr_head_labels), parameter                           &
     &            :: sph_dnamobench_labels = sph_spectr_head_labels(    &
     &                           hdr_nri = 'radial_layers',             &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Not_used',               &
     &                           hdr_kr_out = 'Upper_boundary_ID',      &
     &                           hdr_r_out =  'Upper_boundary_radius',  &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_detailed_dbench_file                             &
     &         (my_rank, sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr,   &
     &          time_d, bench)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_vol_mntr_file
      use select_gz_stream_file_IO
!
      integer, intent(in) :: my_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      type(time_data), intent(in) :: time_d
!
!      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(dynamobench_monitor), intent(in) :: bench
!
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: detail_out(:)
!
!
      if(bench%iflag_dynamobench .le. izero) return
      if(bench%detail_bench_file_prefix .eq. 'NO_FILE') return
      if(my_rank .ne. 0) return
!
      call dup_detail_dbench_header_to_IO                               &
     &   (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT_d)
!
      allocate(detail_out(sph_OUT_d%ntot_sph_spec))
      call dup_detail_dbench_monitor_data                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base, bench,   &
     &    sph_OUT_d%ntot_sph_spec, detail_out)
!
      flag_gzip_lc = bench%gzip_flag_bench
      file_name = add_dat_extension(bench%detail_bench_file_prefix)
      call sel_open_sph_vol_monitor_file(id_dbench, file_name,          &
     &    sph_dnamobench_labels, sph_OUT_d, zbuf_d, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_d)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_dbench,            &
     &    volume_pwr_data_text(time_d%i_time_step, time_d%time,         &
     &                         sph_OUT_d%ntot_sph_spec, detail_out),    &
     &                         zbuf_d)
      close(id_dbench)
      deallocate(detail_out)
!
      end subroutine write_detailed_dbench_file
!
! ----------------------------------------------------------------------
!
      subroutine dup_detail_dbench_header_to_IO                         &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(sph_vol_mean_squares), intent(in) :: v_pwr
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = sph_params%l_truncation
      sph_OUT%nri_sph = sph_rj%nidx_rj(1)
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = v_pwr%kr_inside
      sph_OUT%kr_outer = v_pwr%kr_outside
      sph_OUT%r_inner =  v_pwr%r_inside
      sph_OUT%r_outer =  v_pwr%r_outside
!
      call cnt_detail_dbench_monitor_name                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec)
!
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call copy_detail_dbench_monitor_name                              &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%num_labels,                  &
     &    sph_OUT%ncomp_sph_spec, sph_OUT%ene_sph_spec_name)
!
      end subroutine dup_detail_dbench_header_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine dup_detail_dbench_monitor_data(sph_bc_U, sph_bc_B,     &
     &          ipol_base, bench, num_out, detail_out)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      type(dynamobench_monitor), intent(in) :: bench
!
      integer(kind = kint), intent(in) :: num_out
      real(kind = kreal), intent(inout) :: detail_out(num_out)
!
      integer(kind = kint) :: jcou
!
      jcou = 0
      detail_out(jcou+1:jcou+3) = bench%KE_bench(1:3)
      jcou = jcou + 3
!
      if(ipol_base%i_magne .gt. 0) then
        detail_out(jcou+1:jcou+3) = bench%ME_bench(1:3)
        jcou = jcou + 3
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        detail_out(jcou+1:jcou+3) = bench%mene_icore(1:3)
        jcou = jcou + 3
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        detail_out(jcou+1) = bench%rotate_icore(0)
        jcou = jcou + 1
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        detail_out(jcou+1) = bench%m_torque_icore(0)
        jcou = jcou + 1
      end if
!
      detail_out(jcou+1:jcou+4) = bench%phi_zero(1:4)
      detail_out(jcou+5) = bench%ave_phase_vr
      detail_out(jcou+6:jcou+7) = bench%omega_vm4(1:2)
      jcou = jcou + 7
!
      if(ipol_base%i_magne .gt. 0) then
        detail_out(jcou+1)                                              &
     &       = bench%d_zero(0,bench%iphys_dbench%i_magne+1)
        jcou = jcou + 1
      end if
!
      detail_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_velo+2)
      jcou = jcou + 1
      if(ipol_base%i_temp .gt. 0) then
        detail_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_temp)
        jcou = jcou + 1
      end if
      if(ipol_base%i_light .gt. 0) then
        detail_out(jcou+1) = bench%d_zero(0,bench%iphys_dbench%i_light)
      end if
!
      end subroutine dup_detail_dbench_monitor_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cnt_detail_dbench_monitor_name(sph_bc_U, sph_bc_B,    &
     &         ipol_base, nfield_sph_spec, ntot_sph_spec)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(inout) :: nfield_sph_spec
      integer(kind = kint), intent(inout) :: ntot_sph_spec
!
!
      nfield_sph_spec =  1 + 7 + 1
      ntot_sph_spec =    3 + 7 + 1
!
      if(ipol_base%i_magne .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1 + 1
        ntot_sph_spec =   ntot_sph_spec + 3 + 1
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      if(ipol_base%i_light .gt. 0) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 3
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        nfield_sph_spec = nfield_sph_spec + 1
        ntot_sph_spec =   ntot_sph_spec + 1
      end if
!
      end subroutine cnt_detail_dbench_monitor_name
!
! ----------------------------------------------------------------------
!
      subroutine copy_detail_dbench_monitor_name                        &
     &         (sph_bc_U, sph_bc_B, ipol_base,                          &
     &          nfield_sph_spec, num_labels,                            &
     &          ncomp_sph_spec, ene_sph_spec_name)
!
      use m_time_labels
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(base_field_address), intent(in) :: ipol_base
      integer(kind = kint), intent(in) :: nfield_sph_spec
      integer(kind = kint), intent(in) :: num_labels
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
!
      integer(kind = kint) :: icou, jcou, i
!
      icou = 0
      ene_sph_spec_name(1) = fhd_t_step
      ene_sph_spec_name(2) = fhd_time
      jcou = 2
!
      ncomp_sph_spec(icou+1) = 3
      ene_sph_spec_name(jcou+1) = 'KE_pol'
      ene_sph_spec_name(jcou+2) = 'KE_tor'
      ene_sph_spec_name(jcou+3) = 'KE_total'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      if(ipol_base%i_magne .gt. 0) then
        ncomp_sph_spec(icou+1) = 3
        ene_sph_spec_name(jcou+1) = 'ME_pol'
        ene_sph_spec_name(jcou+2) = 'ME_tor'
        ene_sph_spec_name(jcou+3) = 'ME_total'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        ncomp_sph_spec(icou+1) = 3
        ene_sph_spec_name(jcou+1) = 'ME_pol_icore'
        ene_sph_spec_name(jcou+2) = 'ME_tor_icore'
        ene_sph_spec_name(jcou+3) = 'ME_total_icore'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'omega_ic_z'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
!      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &    .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'MAG_torque_ic_z'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      do i = 1,  4
        ncomp_sph_spec(icou+1) = 1
        write(ene_sph_spec_name(jcou+1),'(a4,i1)')  'phi_', i
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end do
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'Average_drift_vr'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'omega_vp44'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'omega_vt54'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
      if(ipol_base%i_magne .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'B_theta'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      ncomp_sph_spec(icou+1) = 1
      ene_sph_spec_name(jcou+1) = 'v_phi'
      icou = icou + 1
      jcou = jcou + ncomp_sph_spec(icou)
!
!
      if(ipol_base%i_temp .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'temperature'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      if(ipol_base%i_light .gt. 0) then
        ncomp_sph_spec(icou+1) = 1
        ene_sph_spec_name(jcou+1) = 'composition'
        icou = icou + 1
        jcou = jcou + ncomp_sph_spec(icou)
      end if
!
      end subroutine copy_detail_dbench_monitor_name
!
! ----------------------------------------------------------------------
!
      end module dup_detailed_dbench_to_IO
