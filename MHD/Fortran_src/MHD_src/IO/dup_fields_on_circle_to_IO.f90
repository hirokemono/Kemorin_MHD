!>@file   dup_fields_on_circle_to_IO.f90
!!@brief  module dup_fields_on_circle_to_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine write_fields_on_circle_file                          &
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
!!      subroutine dup_detail_dbench_monitor_name(sph_bc_U, sph_bc_B,   &
!!     &          ipol_base, bench, num_out, detail_out)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(dynamobench_monitor), intent(in) :: bench
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module dup_fields_on_circle_to_IO
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
      use t_fields_on_circle
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_dbench = 36
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
      private :: dup_field_on_circ_header_to_IO
!      private :: copy_detail_dbench_monitor_name
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_fields_on_circle_file                            &
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
!      call dup_field_on_circ_header_to_IO                              &
!     &   (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, sph_OUT_d)
!
!      allocate(detail_out(sph_OUT_d%ntot_sph_spec))
!      call dup_detail_dbench_monitor_name                              &
!     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base, bench,  &
!     &    sph_OUT_d%ntot_sph_spec, detail_out)
!
!      flag_gzip_lc = bench%gzip_flag_bench
!      file_name = add_dat_extension(bench%detail_bench_file_prefix)
!      call sel_open_sph_vol_monitor_file(id_dbench, file_name,         &
!     &    sph_dnamobench_labels, sph_OUT_d, zbuf_d, flag_gzip_lc)
!      call dealloc_sph_espec_name(sph_OUT_d)
!
!      call sel_gz_write_text_stream(flag_gzip_lc, id_dbench,           &
!     &    volume_pwr_data_text(time_d%i_time_step, time_d%time,        &
!     &                         sph_OUT_d%ntot_sph_spec, detail_out),   &
!     &                         zbuf_d)
!      close(id_dbench)
!      deallocate(detail_out)
!
      end subroutine write_fields_on_circle_file
!
! ----------------------------------------------------------------------
!
      subroutine dup_field_on_circ_header_to_IO                         &
     &         (sph_params, sph_rj, ipol, sph_MHD_bc, v_pwr, circle, sph_OUT)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      type(fields_on_circle), intent(in) :: circle
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
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = izero
      sph_OUT%r_inner =  circle%s_circle
      sph_OUT%r_outer =  circle%z_circle
!
      call count_dynamobench_monitor_name                               &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec)
!
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call copy_dynamobench_monitor_name                                &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol%base,          &
     &    sph_OUT%nfield_sph_spec, sph_OUT%num_labels,                  &
     &    sph_OUT%ncomp_sph_spec, sph_OUT%ene_sph_spec_name)
!
      end subroutine dup_field_on_circ_header_to_IO
!
! ----------------------------------------------------------------------
!
      end module dup_fields_on_circle_to_IO
