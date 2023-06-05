!>@file   MPI_picked_sph_mean_sq_IO.f90
!!@brief  module MPI_picked_sph_mean_sq_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_picked_sph_mean_sq_file                       &
!!     &         (time_d, sph_params, sph_rj, leg, ipol, ipol_LES,      &
!!     &          rj_fld, picked)
!!      subroutine append_picked_sph_vol_msq_file(time_d, sph_params,   &
!!     &          sph_rj, leg, ipol, ipol_LES, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
      module MPI_picked_sph_mean_sq_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_schmidt_poly_on_rtm
      use t_time_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_buffer_4_gzip
!
      implicit  none
!
      integer, parameter, private :: len_fixed = 4*16 + 2*25 + 1
!
      private :: wrt_picked_sph_mean_vol_sq_mpi
      private :: write_picked_specr_head_mpi
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_picked_sph_mean_sq_file                         &
     &         (time_d, sph_params, sph_rj, leg, ipol, ipol_LES,        &
     &          rj_fld, picked)
!
      use pickup_sph_mean_square_data
      use write_each_pick_spectr_file
      use write_pick_sph_spectr_data
      use sph_monitor_data_text
      use select_gz_stream_file_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint), parameter :: id_pick = 17
      integer(kind = kint) :: inum, knum
      integer(kind = kint_gl) :: num
!
      real(kind=kreal), allocatable :: d_rj_out(:,:)
      type(buffer_4_gzip) :: zbuf_p
      logical :: flag_gzip_lc
!
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj,picked%num_layer))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call cal_rj_mean_sq_degree0_monitor(knum, sph_rj, rj_fld,       &
     &      picked, picked%ntot_comp_rj, d_rj_out)
        call convert_to_energy_sph_monitor                              &
     &     (ipol, ipol_LES, picked, picked%ntot_comp_rj, d_rj_out)
!
        flag_gzip_lc = picked%flag_gzip
        call open_write_each_picked_spectr(izero, id_pick,              &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      flag_gzip_lc, zbuf_p)
        call sel_gz_write_text_stream(flag_gzip_lc, id_pick,            &
     &      picked_each_mode_data_text(time_d%i_time_step, time_d%time, &
     &                                 zero, izero, izero, izero,       &
     &                                 picked%ntot_comp_rj,             &
     &                                 d_rj_out(1,1)),                  &
     &      zbuf_p)
        close(id_pick)
      end if
!
      do inum = 1, picked%num_sph_mode_lc
        do knum = 1, picked%num_layer
          call cal_rj_mean_sq_spectr_monitor                            &
     &       (inum, knum, sph_rj, leg, rj_fld, picked,                  &
     &        picked%ntot_comp_rj, d_rj_out(1,knum))
          call convert_to_energy_sph_monitor(ipol, ipol_LES, picked,    &
     &        picked%ntot_comp_rj, d_rj_out(1,knum))
        end do
!
        flag_gzip_lc = picked%flag_gzip
        call open_write_each_picked_spectr(inum, id_pick,               &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      flag_gzip_lc, zbuf_p)
        call sel_gz_write_picked_spec_data(flag_gzip_lc, id_pick,       &
     &      time_d, picked, inum, d_rj_out, zbuf_p)
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine append_picked_sph_mean_sq_file
!
! -----------------------------------------------------------------------
!
      subroutine append_picked_sph_vol_msq_file(time_d, sph_params,     &
     &          sph_rj, leg, ipol, ipol_LES, rj_fld, picked)
!
      use set_parallel_file_name
      use MPI_ascii_data_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(picked%file_prefix)
      call open_append_mpi_file(file_name, IO_param)
!
      if(IO_param%ioff_gl .eq. 0) then
        call write_picked_specr_head_mpi(IO_param, picked)
      end if
!
      call wrt_picked_sph_mean_vol_sq_mpi                               &
     &   (IO_param, time_d, sph_params, sph_rj, leg, ipol, ipol_LES,    &
     &    rj_fld, picked, picked%ntot_comp_rj)
!
      call close_mpi_file(IO_param)
!
      end subroutine append_picked_sph_vol_msq_file
!
! -----------------------------------------------------------------------
!
      subroutine wrt_picked_sph_mean_vol_sq_mpi(IO_param, time_d,       &
     &          sph_params, sph_rj, leg, ipol, ipol_LES, rj_fld,        &
     &          picked, ntot_comp_rj)
!
      use radial_int_for_sph_spec
      use pickup_sph_mean_square_data
      use write_picked_sph_spectr
      use sph_monitor_data_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
!
      integer(kind = kint) :: icou, ist, inum, knum
      integer(kind = kint) :: kst, ked, nlayer
!
      integer(kind = kint_gl) :: num
      integer :: ilen_n
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = len_fixed+ntot_comp_rj*25),                       &
     &                                 allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
      real(kind=kreal), allocatable :: d_layer(:,:)
!
!
      ilen_n = len_fixed + ntot_comp_rj*25
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        nlayer = sph_params%nlayer_CMB - sph_params%nlayer_ICB
        kst = sph_params%nlayer_ICB
        if(sph_params%nlayer_ICB .eq. 0) kst = 1
        ked = sph_params%nlayer_CMB
!
        allocate(d_layer(kst:ked,ntot_comp_rj))
        allocate(d_rj_out(0:ntot_comp_rj))
        allocate(pickedbuf(num))
!
        icou = 0
        ist = 1
        if(picked%idx_out(1,1) .eq. 0) then
          do knum = sph_params%nlayer_ICB, sph_params%nlayer_CMB
            call cal_rj_mean_sq_degree0_monitor                         &
     &         (knum, sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
            d_layer(knum,1:ntot_comp_rj) = d_rj_out
          end do
!
          ist = ist + 1
          icou = icou + 1
          call radial_integration                                       &
     &       (sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                 &
     &        ntot_comp_rj, d_layer, d_rj_out)
          call convert_to_energy_sph_monitor                            &
     &       (ipol, ipol_LES, picked, ntot_comp_rj, d_rj_out)
           pickedbuf(icou)                                              &
     &         = picked_each_mode_data_text                             &
     &         (time_d%i_time_step, time_d%time,                        &
     &          picked%radius_gl(knum,1), picked%id_radius(knum,1),     &
     &          picked%idx_out(1,1), picked%idx_out(1,2),               &
     &          ntot_comp_rj, d_rj_out)
        end if
!
        do inum = ist, picked%num_sph_mode_lc
          do knum = kst, ked
            call cal_rj_mean_sq_spectr_monitor                          &
     &         (inum, knum, sph_rj, leg, rj_fld, picked,                &
     &          ntot_comp_rj, d_rj_out)
            d_layer(knum,1:ntot_comp_rj) = d_rj_out
          end do
!
          icou = icou + 1
          call radial_integration                                       &
     &       (ione, nlayer, nlayer, sph_rj%radius_1d_rj_r(kst),         &
     &        ntot_comp_rj, d_layer(kst,1), d_rj_out)
          call convert_to_energy_sph_monitor                            &
     &       (ipol, ipol_LES, picked, ntot_comp_rj, d_rj_out)
           pickedbuf(icou)                                              &
     &         = picked_each_mode_data_text                             &
     &         (time_d%i_time_step, time_d%time,                        &
     &          picked%radius_gl(knum,1), picked%id_radius(knum,1),     &
     &          picked%idx_out(inum,1), picked%idx_out(inum,2),         &
     &          ntot_comp_rj, d_rj_out)
        end do
!
        call mpi_write_mul_chara_b                                      &
     &     (IO_param%id_file, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &       + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      end subroutine wrt_picked_sph_mean_vol_sq_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_picked_specr_head_mpi(IO_param, picked)
!
      use calypso_mpi_int4
      use MPI_ascii_data_IO
      use write_field_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: i
      integer :: len_head, len_fld, len_each
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = 1), parameter :: timebuf = char(10)
      character(len = kchara) :: textbuf
!
!
      len_head = len(pick_sph_header_no_field(picked))
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call mpi_write_one_chara_b(IO_param%id_file, ioffset,           &
     &      len_head, pick_sph_header_no_field(picked))
!
        len_fld = 0
        do i = 1, picked%ntot_comp_rj
          len_each = len_trim(picked%spectr_name(i)) + 4
          len_fld = len_fld + len_each
          write(textbuf,'(a,a4)') trim(picked%spectr_name(i)), '    '
          call mpi_write_one_chara_b                                    &
     &       (IO_param%id_file, ioffset, len_each, textbuf)
        end do
!
        call mpi_write_one_chara_b                                      &
     &     (IO_param%id_file, ioffset, 1, timebuf)
      end if
!
      call calypso_mpi_bcast_one_int4(len_fld, 0)
      IO_param%ioff_gl = IO_param%ioff_gl + len_head + len_fld + ione
!
      end subroutine write_picked_specr_head_mpi
!
! -----------------------------------------------------------------------
!
      function pick_sph_header_no_field(picked)
!
      use m_monitor_file_labels
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_h1 = ilen_pick_sph_head + 3*16 + 1
      integer(kind = kint), parameter                                   &
     &         :: ilen_h2 = ilen_pick_sph_num + 16 + 1
      integer(kind = kint), parameter                                   &
     &        :: len_head = ilen_h1 + ilen_h2 + ilen_time_sph_label
!
      character(len = len_head) :: pick_sph_header_no_field
!
!
      write(pick_sph_header_no_field,'(a,2i16,a1,a,i16,a1,a)')          &
     &        hd_pick_sph_head(),                                       &
     &        picked%num_layer, picked%num_sph_mode, char(10),          &
     &        hd_pick_sph_num(), picked%ntot_comp_rj, char(10),         &
     &        hd_time_sph_label()
!
      end function pick_sph_header_no_field
!
! -----------------------------------------------------------------------
!
      end module MPI_picked_sph_mean_sq_IO
