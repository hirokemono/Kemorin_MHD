!> Optional first-order axial magnetic-field volume integrals.
!> Reuses the normal nonlinear backward transform; no extra transform.
      module t_signed_axial_field_monitor
      use m_precision
      use m_constants
      use calypso_mpi
      implicit none
      private
      public :: signed_axial_field_monitor, init_signed_axial_monitor
      public :: cal_signed_axial_integrals, output_signed_axial_monitor
!
      type signed_axial_field_monitor
        logical :: enabled = .false.
        character(len=kchara) :: file_prefix = ''
!> Local r^2 dr and Gauss weights; full-shell integrals, not averages.
        real(kind=kreal), allocatable :: wr(:), wt(:), ct(:), st(:)
      end type
      contains
!
      subroutine init_signed_axial_monitor(sph, leg, mon)
      use t_spheric_parameter
      use t_schmidt_poly_on_rtm
      use radial_int_for_sph_spec
      type(sph_grids), intent(in) :: sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(signed_axial_field_monitor), intent(inout) :: mon
      real(kind=kreal), allocatable :: radial_weight(:)
      real(kind=kreal) :: center_weight, wsum
      integer(kind=kint) :: nr, nt, nri, ki, ko, k, l, kg, lg
!
      if(.not. mon%enabled) return
      if(len_trim(mon%file_prefix) .eq. 0) then
        call calypso_mpi_abort(1_kint, 'Empty signed axial field prefix')
      end if
      nr = sph%sph_rtp%nidx_rtp(1)
      nt = sph%sph_rtp%nidx_rtp(2)
      nri = sph%sph_rj%nidx_rj(1)
      ki = sph%sph_params%nlayer_ICB
      ko = sph%sph_params%nlayer_CMB
      if(ki .lt. 1 .or. ko .gt. nri .or. ki .ge. ko) then
        call calypso_mpi_abort(1_kint, 'Invalid signed axial shell bounds')
      end if
      if(size(leg%weight_rtm) .ne. sph%sph_rtp%nidx_global_rtp(2)) then
        call calypso_mpi_abort(1_kint, 'Signed axial monitor needs Gauss grid')
      end if
      if(sph%sph_rtp%nidx_global_rtp(3) .le. 0) then
        call calypso_mpi_abort(1_kint, 'Invalid signed axial longitude grid')
      end if
      allocate(radial_weight(nri))
      radial_weight = zero
      center_weight = zero
!     Global weights retain intervals crossing radial MPI partitions.
      call radial_int_matrix_by_trapezoid(nri, ki, ko,                 &
     &    sph%sph_rj%radius_1d_rj_r, radial_weight, center_weight)
      radial_weight = radial_weight * sph%sph_rj%radius_1d_rj_r**2
      if(allocated(mon%wr)) deallocate(mon%wr,mon%wt,mon%ct,mon%st)
      allocate(mon%wr(nr), mon%wt(nt), mon%ct(nt), mon%st(nt))
      do k = 1, nr
        kg = sph%sph_rtp%idx_gl_1d_rtp_r(k)
        mon%wr(k) = radial_weight(kg)
      end do
!     Normalize to integral d(cos theta) = 2, as in the TC monitor.
      wsum = sum(leg%weight_rtm)
      if(wsum .le. zero) then
        call calypso_mpi_abort(1_kint, 'Invalid signed axial Gauss weights')
      end if
      do l = 1, nt
        lg = sph%sph_rtp%idx_gl_1d_rtp_t(l)
        mon%wt(l) = two * leg%weight_rtm(lg) / wsum
        mon%ct(l) = cos(leg%g_colat_rtm(lg))
        mon%st(l) = sin(leg%g_colat_rtm(lg))
      end do
      end subroutine
!
!> Return independently accumulated Mz, Pplus, Pminus on this rank.
      subroutine cal_signed_axial_integrals(rtp, ib, fld, mon, acc)
      use t_spheric_rtp_data
      type(sph_rtp_grid), intent(in) :: rtp
      integer(kind=kint), intent(in) :: ib
      real(kind=kreal), intent(in) :: fld(:,:)
      type(signed_axial_field_monitor), intent(in) :: mon
      real(kind=kreal), intent(out) :: acc(3)
      integer(kind=kint) :: k, l, p, inod
      real(kind=kreal) :: bz, w, dphi
      acc = zero
      if(.not. mon%enabled) return
      if(.not. allocated(mon%wr)) then
        call calypso_mpi_abort(1_kint, 'Signed axial monitor not initialized')
      end if
      if(ib .le. 0 .or. ib+2 .gt. size(fld,2)) then
        call calypso_mpi_abort(1_kint, 'Signed axial monitor needs magnetic field')
      end if
!     For m-fold symmetry this includes all identical sectors once.
      dphi = two * (four*atan(one)) / dble(rtp%nidx_global_rtp(3))
!$omp parallel do collapse(2) private(k,l,p,inod,bz,w) reduction(+:acc)
      do l = 1, rtp%nidx_rtp(2)
        do k = 1, rtp%nidx_rtp(1)
          if(mon%wr(k) .eq. zero) cycle
          w = mon%wr(k) * mon%wt(l) * dphi
          do p = 1, rtp%nidx_rtp(3)
            inod = 1 + (k-1)*rtp%istep_rtp(1)                        &
     &               + (l-1)*rtp%istep_rtp(2)                        &
     &               + (p-1)*rtp%istep_rtp(3)
            bz = fld(inod,ib)*mon%ct(l) - fld(inod,ib+1)*mon%st(l)
            acc(1) = acc(1) + w*bz
            acc(2) = acc(2) + w*max(bz,zero)
            acc(3) = acc(3) + w*max(-bz,zero)
          end do
        end do
      end do
!$omp end parallel do
      end subroutine
!
      subroutine output_signed_axial_monitor(time_d, rtp, trns, mon)
      use t_time_data
      use t_spheric_rtp_data
      use t_sph_trans_arrays_MHD
      use calypso_mpi_real
      use transfer_to_long_integers
      use set_parallel_file_name
      type(time_data), intent(in) :: time_d
      type(sph_rtp_grid), intent(in) :: rtp
      type(address_4_sph_trans), intent(in) :: trns
      type(signed_axial_field_monitor), intent(in) :: mon
      real(kind=kreal) :: local(3), global(3)
      character(len=kchara) :: filename
      integer :: unit, ios
      integer(kind=kint_gl) :: file_size
!
      if(.not. mon%enabled) return
      call cal_signed_axial_integrals(rtp, trns%b_trns%base%i_magne,     &
     &    trns%backward%fld_rtp, mon, local)
      call calypso_mpi_reduce_real(local, global, cast_long(3_kint),         &
     &                            MPI_SUM, 0)
      if(my_rank .ne. 0) return
      filename = add_dat_extension(mon%file_prefix)
      open(newunit=unit, file=filename, status='unknown',              &
     &     position='append', form='formatted', iostat=ios)
      if(ios .ne. 0) then
        call calypso_mpi_abort(1_kint, 'Cannot open signed axial monitor file')
      end if
      inquire(unit=unit, size=file_size)
      if(file_size .eq. 0) then
        write(unit,'(a)') '# ICB-CMB volume integrals; no normalization'
        write(unit,'(a)') '# t_step time Bz_volume_integral '           &
     &      // 'Bz_positive_integral Bz_negative_magnitude_integral'
      end if
      write(unit,'(i16,1p4e25.16e3)',iostat=ios)                        &
     &    time_d%i_time_step, time_d%time, global
      close(unit)
      if(ios .ne. 0) then
        call calypso_mpi_abort(1_kint, 'Cannot write signed axial monitor file')
      end if
      end subroutine
      end module
