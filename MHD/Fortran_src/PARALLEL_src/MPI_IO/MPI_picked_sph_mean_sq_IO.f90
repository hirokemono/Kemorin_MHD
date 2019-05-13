!>@file   MPI_picked_sph_mean_sq_IO.f90
!!@brief  module MPI_picked_sph_mean_sq_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_picked_spectrum_file(file_name, picked)
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
     &           = picked_each_mode(time_d%i_time_step, time_d%time,    &
     &                              zero, izero, izero, izero,          &
     &                              ntot_comp_rj, d_rj_out)
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
     &           = picked_each_mode(time_d%i_time_step, time_d%time,    &
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
     &           = picked_each_mode(time_d%i_time_step, time_d%time,    &
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
!        if(picked%idx_out(0,4) .gt. 0                                  &
!         .and. sph_params%nlayer_CMB.eq. 0) then
!          icou = icou + 1
!          call rms_rj_center_monitor(sph_rj, rj_fld, picked,           &
!     &        ntot_comp_rj, d_rj_out)
!          call convert_to_energy_sph__monitor                          &
!     &       (ipol, picked, ntot_comp_rj, d_rj_out)
!          d_layer(0,1:ntot_comp_rj) = d_rj_out
!        end if
!
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
     &         = picked_each_mode(time_d%i_time_step, time_d%time,      &
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
     &         = picked_each_mode(time_d%i_time_step, time_d%time,      &
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
! -----------------------------------------------------------------------
!
      subroutine rms_rj_spectrum_4_monitor                              &
     &        (inum, knum, sph_rj, rj_fld, leg, picked,                 &
     &         ntot_comp_rj, rms_out)
!
      integer(kind = kint), intent(in) :: inum, knum

      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_spectr_monitor                                &
     &   (picked%idx_out(inum,4), picked%id_radius(knum),               &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,    &
     &    leg%g_sph_rj, rj_fld%n_point, rj_fld%num_phys,                &
     &    rj_fld%ntot_phys, rj_fld%istack_component, rj_fld%d_fld,      &
     &    picked%num_field_rj, picked%istack_comp_rj,                   &
     &    picked%ifield_monitor_rj, ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_spectrum_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine rms_rj_degree0_monitor(knum, sph_rj, rj_fld, picked,   &
     &         ntot_comp_rj, rms_out)
!
      integer(kind = kint), intent(in) :: knum

      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_degree0_monitor                               &
     &   (picked%id_radius(knum), sph_rj%idx_rj_degree_zero,            &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,    &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_degree0_monitor
!
! -----------------------------------------------------------------------
!
      subroutine rms_rj_center_monitor(sph_rj, rj_fld, picked,          &
     &         ntot_comp_rj, rms_out)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_rj)
!
!
      call cal_rj_mean_sq_center_monitor(sph_rj%inod_rj_center,         &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, rms_out)
!
      end subroutine rms_rj_center_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_spectr_monitor                          &
     &        (j, k, nidx_rj, radius_1d_rj_r, a_r_1d_rj_r, g_sph_rj,    &
     &         n_point, num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, &
     &         d_rj, nfld_monitor, istack_comp_monitor, ifld_monitor,   &
     &         ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: j, k
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp, inod
!
!
      inod = j + (k-1) * nidx_rj(2)
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1) = g_sph_rj(j,12) * (d_rj(inod,icou+2)**2    &
     &       + g_sph_rj(j,3) * (a_r_1d_rj_r(k) * d_rj(inod,icou+1))**2)
            rms_out(jcou+2) = g_sph_rj(j,12) * d_rj(inod,icou+3)**2
            rms_out(jcou+3) =  rms_out(1) + rms_out(2)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = g_sph_rj(j,11)                           &
     &           * (d_rj(inod,icou+nd) * radius_1d_rj_r(k))**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_spectr_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_degree0_monitor(k, idx_rj_degree_zero,  &
     &          nidx_rj, radius_1d_rj_r, a_r_1d_rj_r, n_point,          &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj,   &
     &          nfld_monitor, istack_comp_monitor, ifld_monitor,        &
     &          ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: k
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp, inod
!
!
      inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1)                                             &
     &             = (half * d_rj(inod,icou+1) * a_r_1d_rj_r(k))**2
            rms_out(jcou+2) = zero
            rms_out(jcou+3) = rms_out(jcou+1)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd)                                            &
     &           = (d_rj(inod,icou+nd) * radius_1d_rj_r(k))**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_degree0_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rj_mean_sq_center_monitor                          &
     &         (inod_rj_center, n_point, num_phys_rj,                   &
     &          ntot_phys_rj, istack_phys_comp_rj, d_rj,                &
     &          nfld_monitor, istack_comp_monitor, ifld_monitor,        &
     &          ntot_comp_monitor, rms_out)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
!
!
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            rms_out(jcou+1) = (half * d_rj(inod_rj_center,icou+1))**2
            rms_out(jcou+2) = zero
            rms_out(jcou+3) = rms_out(jcou+1)
        else
          do nd = 1, ncomp
            rms_out(jcou+nd) = d_rj(inod_rj_center,icou+nd)**2
          end do
        end if
      end do
!
      end subroutine cal_rj_mean_sq_center_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine convert_to_energy_sph__monitor                         &
     &         (ipol, picked, ntot_comp_monitor, rms_out)
!
      type(phys_address), intent(in) :: ipol
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_monitor
!
      real(kind=kreal), intent(inout) :: rms_out(ntot_comp_monitor)
!
      integer(kind = kint) :: i_fld, j_fld, jcou
!
!
      do j_fld = 1, picked%num_field_rj
        i_fld = picked%ifield_monitor_rj(j_fld)
        jcou = picked%istack_comp_rj(j_fld-1)
        if (   i_fld .eq. ipol%i_velo                                &
     &      .or. i_fld .eq. ipol%i_magne                             &
     &      .or. i_fld .eq. ipol%i_filter_velo                       &
     &      .or. i_fld .eq. ipol%i_filter_magne                      &
     &      .or. i_fld .eq. ipol%i_wide_fil_velo                     &
     &      .or. i_fld .eq. ipol%i_wide_fil_magne) then
          rms_out(jcou+1) = half * rms_out(jcou+1)
          rms_out(jcou+2) = half * rms_out(jcou+1)
          rms_out(jcou+3) = half * rms_out(jcou+1)
        end if
      end do
!
      end subroutine convert_to_energy_sph__monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function picked_each_mode                                         &
     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, rms_out)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, radius
      integer(kind = kint), intent(in) :: kr, l, m
      integer(kind = kint), intent(in) :: ntot_comp_rj
      real(kind = kreal), intent(in) :: rms_out(ntot_comp_rj)
!
      character(len = len_fixed+ntot_comp_rj*25) :: picked_each_mode
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a37,i4,a17)')                                     &
     &         '(i16,1pe25.14e3, i16,1pe25.14e3,2i16,',                 &
     &           ntot_comp_rj, '(1pE25.14e3), a1)'
      write(picked_each_mode,fmt_txt) i_step, time,                     &
     &          kr, radius, l, m, rms_out(1:ntot_comp_rj), char(10)
!
      end function  picked_each_mode
!
! ----------------------------------------------------------------------
!
      end module MPI_picked_sph_mean_sq_IO
