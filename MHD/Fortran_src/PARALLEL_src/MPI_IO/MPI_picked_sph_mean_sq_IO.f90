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
!!     &         (time_d, sph_rj, leg, ipol, rj_fld, picked)
!!      subroutine append_picked_sph_vol_msq_file                       &
!!     &         (time_d, sph_params, sph_rj, leg, ipol, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
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
      use t_phys_address
      use t_phys_data
      use t_time_data
!
      implicit  none
!
      integer, parameter, private :: len_fixed = 4*16 + 2*25 + 1
!
      private :: write_picked_sph_mean_sq_mpi
      private :: wrt_picked_sph_mean_vol_sq_mpi
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_picked_sph_mean_sq_file                         &
     &         (time_d, sph_rj, leg, ipol, rj_fld, picked)
!
      use set_parallel_file_name
      use MPI_ascii_data_IO
      use MPI_picked_sph_spectr_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(picked%file_prefix)
      call open_append_mpi_file(file_name, nprocs, my_rank, IO_param)
      call calypso_mpi_barrier
!
      if(IO_param%ioff_gl .eq. 0) then
        call write_picked_specr_head_mpi(IO_param, picked)
      end if
!
      call write_picked_sph_mean_sq_mpi                                 &
     &   (IO_param, time_d, sph_rj, leg, ipol, rj_fld,                  &
     &    picked, picked%ntot_comp_rj)
!
      call close_mpi_file(IO_param)
!
      end subroutine append_picked_sph_mean_sq_file
!
!  ---------------------------------------------------------------------
!
      subroutine append_picked_sph_vol_msq_file                         &
     &         (time_d, sph_params, sph_rj, leg, ipol, rj_fld, picked)
!
      use set_parallel_file_name
      use MPI_ascii_data_IO
      use MPI_picked_sph_spectr_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(picked%file_prefix)
      call open_append_mpi_file(file_name, nprocs, my_rank, IO_param)
      call calypso_mpi_barrier
!
      if(IO_param%ioff_gl .eq. 0) then
        call write_picked_specr_head_mpi(IO_param, picked)
      end if
!
      call wrt_picked_sph_mean_vol_sq_mpi                               &
     &   (IO_param, time_d, sph_params, sph_rj, leg, ipol, rj_fld,      &
     &    picked, picked%ntot_comp_rj)
!
      call close_mpi_file(IO_param)
!
      end subroutine append_picked_sph_vol_msq_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_picked_sph_mean_sq_mpi(IO_param, time_d,         &
     &          sph_rj, leg, ipol, rj_fld, picked, ntot_comp_rj)
!
      use pickup_sph_mean_square_data
      use MPI_picked_sph_spectr_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
!
      integer(kind = kint) :: icou, ist
      integer(kind = kint) :: inum, knum
!
      integer(kind = kint_gl) :: num
      integer :: ilen_n
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = len_fixed+ntot_comp_rj*25),                       &
     &                                 allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
      ilen_n = len_fixed + ntot_comp_rj*25
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(d_rj_out(ntot_comp_rj))
        allocate(pickedbuf(num))
!
        icou = 0
        if(picked%idx_out(0,4) .gt. 0) then
          icou = icou + 1
          call rms_rj_center_monitor(sph_rj, rj_fld, picked,            &
     &        ntot_comp_rj, d_rj_out)
          call convert_to_energy_sph__monitor                           &
     &       (ipol, picked, ntot_comp_rj, d_rj_out)
!
          pickedbuf(icou)                                               &
     &         = picked_each_mode_to_text                               &
     &         (time_d%i_time_step, time_d%time,                        &
     &          zero, izero, izero, izero, ntot_comp_rj, d_rj_out)
        end if
!
!
        ist = 1
        if(picked%idx_out(1,1) .eq. 0) then
          do knum = 1, picked%num_layer
            call rms_rj_degree0_monitor(knum, sph_rj, rj_fld, picked,   &
     &          ntot_comp_rj, d_rj_out)
            call convert_to_energy_sph__monitor                         &
     &         (ipol, picked, ntot_comp_rj, d_rj_out)
!
            icou = icou + 1
            pickedbuf(icou)                                             &
     &         = picked_each_mode_to_text                               &
     &         (time_d%i_time_step, time_d%time,                        &
     &            picked%radius_gl(knum), picked%id_radius(knum),       &
     &            picked%idx_out(1,1), picked%idx_out(1,2),             &
     &            ntot_comp_rj, d_rj_out)
          end do
          ist = ist + 1
        end if
!
        do inum = ist, picked%num_sph_mode_lc
          do knum = 1, picked%num_layer
            icou = icou + 1
            call rms_rj_spectrum_4_monitor                              &
     &         (inum, knum, sph_rj, rj_fld, leg, picked,                &
     &          ntot_comp_rj, d_rj_out)
            call convert_to_energy_sph__monitor                         &
     &         (ipol, picked, ntot_comp_rj, d_rj_out)
!
             pickedbuf(icou)                                            &
     &         = picked_each_mode_to_text                               &
     &         (time_d%i_time_step, time_d%time,                        &
     &            picked%radius_gl(knum), picked%id_radius(knum),       &
     &            picked%idx_out(inum,1), picked%idx_out(inum,2),       &
     &            ntot_comp_rj, d_rj_out)
          end do
        end do
!
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_file, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &       + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      end subroutine write_picked_sph_mean_sq_mpi
!
! -----------------------------------------------------------------------
!
      subroutine wrt_picked_sph_mean_vol_sq_mpi(IO_param, time_d,       &
     &          sph_params, sph_rj, leg, ipol, rj_fld,                  &
     &          picked, ntot_comp_rj)
!
      use radial_int_for_sph_spec
      use pickup_sph_mean_square_data
      use MPI_picked_sph_spectr_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
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
            call rms_rj_degree0_monitor(knum, sph_rj, rj_fld, picked,   &
     &          ntot_comp_rj, d_rj_out)
            d_layer(knum,1:ntot_comp_rj) = d_rj_out
          end do
!
          ist = ist + 1
          icou = icou + 1
          call radial_integration                                       &
     &       (sph_params%nlayer_ICB, sph_params%nlayer_CMB,             &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                 &
     &        ntot_comp_rj, d_layer, d_rj_out)
          call convert_to_energy_sph__monitor                           &
     &       (ipol, picked, ntot_comp_rj, d_rj_out)
           pickedbuf(icou)                                              &
     &         = picked_each_mode_to_text                               &
     &         (time_d%i_time_step, time_d%time,                        &
     &          picked%radius_gl(knum), picked%id_radius(knum),         &
     &          picked%idx_out(1,1), picked%idx_out(1,2),               &
     &          ntot_comp_rj, d_rj_out)
        end if
!
        do inum = ist, picked%num_sph_mode_lc
          do knum = kst, ked
            call rms_rj_spectrum_4_monitor                              &
     &         (inum, knum, sph_rj, rj_fld, leg, picked,                &
     &          ntot_comp_rj, d_rj_out)
            d_layer(knum,1:ntot_comp_rj) = d_rj_out
          end do
!
          icou = icou + 1
          call radial_integration                                       &
     &       (ione, nlayer, nlayer, sph_rj%radius_1d_rj_r(kst),         &
     &        ntot_comp_rj, d_layer(kst,1), d_rj_out)
          call convert_to_energy_sph__monitor                           &
     &       (ipol, picked, ntot_comp_rj, d_rj_out)
           pickedbuf(icou)                                              &
     &         = picked_each_mode_to_text                               &
     &         (time_d%i_time_step, time_d%time,                        &
     &          picked%radius_gl(knum), picked%id_radius(knum),         &
     &          picked%idx_out(inum,1), picked%idx_out(inum,2),         &
     &          ntot_comp_rj, d_rj_out)
        end do
!
        call calypso_mpi_seek_wrt_mul_chara                             &
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
      end module MPI_picked_sph_mean_sq_IO
