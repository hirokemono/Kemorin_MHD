!>@file   t_tc_region_monitor.f90
!!@brief  module t_tc_region_monitor
!!
!!@author T. Kera (Tohoku University)
!!@date Programmed in Oct., 2026
!
!>@brief  In-situ regional energy monitor for tangent cylinder
!!        and outer shell regions
!!
!!@verbatim
!!      subroutine set_ctl_tc_region_monitor(tc_ctl, tc_mon)
!!        type(tc_region_monitor_control), intent(in) :: tc_ctl
!!        type(tc_region_monitor), intent(inout) :: tc_mon
!!      subroutine init_tc_region_monitor(sph, leg, tc_mon)
!!        type(sph_grids), intent(in) :: sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(tc_region_monitor), intent(inout) :: tc_mon
!!      subroutine output_tc_region_monitor                             &
!!     &         (time_d, sph, ipol, rj_fld, trns_MHD, tc_mon)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(tc_region_monitor), intent(inout) :: tc_mon
!!
!!  Regions (s = r sin(theta), z = r cos(theta), r_i: ICB radius)
!!    1 TCN:  s <  r_i, z >= 0
!!    2 TCS:  s <  r_i, z <  0
!!    3 OUTN: s >= r_i, z >= 0, r <  r_split
!!    4 OUTS: s >= r_i, z <  0, r <  r_split
!!    5 CMBN: s >= r_i, z >= 0, r >= r_split
!!    6 CMBS: s >= r_i, z <  0, r >= r_split
!!  A grid point on the equator (only if N_theta is odd) is north.
!!
!!  Quantities (volume integrals, not averages)
!!    Ezon  = 1/2 \int <u_phi>^2 dV
!!    Emer  = 1/2 \int (<u_r>^2 + <u_theta>^2) dV
!!    Econv = 1/2 \int |u - <u>|^2 dV
!!    Euz   = 1/2 \int (u_z - <u_z>)^2 dV
!!    EB    = 1/2 \int |B|^2 dV
!!    EBbar = 1/2 \int |<B>|^2 dV
!!    WL    = \int u \cdot F_Lorentz dV, F_Lorentz = coef_lor (J x B)
!!  < > is the azimuthal average.
!!  Quadrature is the same as the volume spectrum monitors:
!!  trapezoidal r^2 dr from ICB to CMB, Gauss-Legendre in theta,
!!  uniform in phi.
!!@endverbatim
!
      module t_tc_region_monitor
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!>      Number of regions
      integer(kind = kint), parameter :: ntc_region = 6
!>      Number of quantities for each region
      integer(kind = kint), parameter :: ntc_quant =  7
!
      character(len=4), parameter :: tc_region_name(ntc_region)        &
     &            = (/'TCN ', 'TCS ', 'OUTN', 'OUTS', 'CMBN', 'CMBS'/)
      character(len=5), parameter :: tc_quant_name(ntc_quant)          &
     &            = (/'Ezon ', 'Emer ', 'Econv', 'Euz  ', 'EB   ',       &
     &                'EBbar', 'WL   '/)
!
      integer(kind = kint), parameter, private :: id_tc_monitor = 48
      real(kind = kreal), parameter, private :: pi = four * atan(one)
!
!>      Structure for regional energy monitor
      type tc_region_monitor
!>        Flag to output monitor
        logical :: flag_tc_monitor = .FALSE.
!>        Increment of time step for monitor
        integer(kind = kint) :: increment = 10
!>        File prefix
        character(len = kchara)                                         &
     &                 :: file_prefix = 'monitor/tc_region_monitor'
!>        Radius to split outer shell regions
        real(kind = kreal) :: r_split = 1.35d0
!
!>        Number of cylindrical radii for zonal shear fit
        integer(kind = kint) :: num_shear = 0
!>        Cylindrical radii for zonal shear fit
        real(kind = kreal), allocatable :: s_shear(:)
!
!>        Number of global radial points
        integer(kind = kint) :: nri_gl = 0
!>        Number of global meridional points
        integer(kind = kint) :: nth_gl = 0
!>        Radial address of ICB
        integer(kind = kint) :: kr_in = 0
!>        Radial address of CMB
        integer(kind = kint) :: kr_out = 0
!>        ICB radius
        real(kind = kreal) :: r_in = 0.0d0
!>        CMB radius
        real(kind = kreal) :: r_out = 0.0d0
!
!>        Global radius
        real(kind = kreal), allocatable :: radius_gl(:)
!>        Radial weight (r^2 dr, trapezoidal), zero outside shell
        real(kind = kreal), allocatable :: wr_gl(:)
!>        Global colatitude
        real(kind = kreal), allocatable :: colat_gl(:)
!>        Meridional weight (sum = 2)
        real(kind = kreal), allocatable :: wt_gl(:)
!>        Global meridional address of mirror point
        integer(kind = kint), allocatable :: mirror_gl(:)
!
!>        Region ID for local (r, theta) points
        integer(kind = kint), allocatable :: id_region(:,:)
!>        Volume of regions
        real(kind = kreal) :: vol_region(ntc_region)
!
!>        Local address of (l,m) = (1,0) in rj (0 if not in subdomain)
        integer(kind = kint) :: j10 = 0
!
!>        Local work for zonal mean of u_phi on global (r,theta)
        real(kind = kreal), allocatable :: uphi_lc(:,:)
!>        Zonal mean of u_phi on global (r,theta) on rank 0
        real(kind = kreal), allocatable :: uphi_gl(:,:)
      end type tc_region_monitor
!
      private :: cal_tc_region_integrals
      private :: cal_Ezon_asym, cal_zonal_shear_fit
      private :: write_tc_region_monitor_header
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_tc_region_monitor(tc_ctl, tc_mon)
!
      use t_ctl_data_tc_region_monitor
!
      type(tc_region_monitor_control), intent(in) :: tc_ctl
      type(tc_region_monitor), intent(inout) :: tc_mon
!
!
      tc_mon%flag_tc_monitor = (tc_ctl%i_tc_region_monitor_ctl .gt. 0)
      if(tc_mon%flag_tc_monitor .eqv. .FALSE.) return
!
      if(tc_ctl%tc_monitor_file_prefix_ctl%iflag .gt. 0) then
        tc_mon%file_prefix                                              &
     &       = tc_ctl%tc_monitor_file_prefix_ctl%charavalue
      end if
      if(tc_ctl%i_step_tc_monitor_ctl%iflag .gt. 0) then
        tc_mon%increment = tc_ctl%i_step_tc_monitor_ctl%intvalue
      end if
      if(tc_mon%increment .le. 0) tc_mon%flag_tc_monitor = .FALSE.
      if(tc_ctl%r_split_ctl%iflag .gt. 0) then
        tc_mon%r_split = tc_ctl%r_split_ctl%realvalue
      end if
!
      tc_mon%num_shear = tc_ctl%shear_fit_radii_ctl%num
      allocate(tc_mon%s_shear(tc_mon%num_shear))
      if(tc_mon%num_shear .gt. 0) then
        tc_mon%s_shear(1:tc_mon%num_shear)                              &
     &       = tc_ctl%shear_fit_radii_ctl%vect(1:tc_mon%num_shear)
      end if
!
      end subroutine set_ctl_tc_region_monitor
!
! ----------------------------------------------------------------------
!
      subroutine init_tc_region_monitor(sph, leg, tc_mon)
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_schmidt_poly_on_rtm
      use calypso_mpi_real
      use transfer_to_long_integers
      use set_parallel_file_name
!
      type(sph_grids), intent(in) :: sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(tc_region_monitor), intent(inout) :: tc_mon
!
      integer(kind = kint) :: kr, lt, kg, lg, m, ireg
      real(kind = kreal) :: r, s, z, wsum, dmin, vol_lc(ntc_region)
      character(len = kchara) :: file_name
      logical :: flag_exist
!
!
      if(tc_mon%flag_tc_monitor .eqv. .FALSE.) return
!
      if(sph%sph_rtp%nidx_rtp(3) .ne. sph%sph_rtp%nidx_global_rtp(3))   &
     &   call calypso_mpi_abort(1,                                      &
     &      'tc_region_monitor needs complete phi in each subdomain')
      if(size(leg%weight_rtm) .ne. sph%sph_rtp%nidx_global_rtp(2))      &
     &   call calypso_mpi_abort(1,                                      &
     &      'tc_region_monitor needs global Gauss weights')
!
      tc_mon%nri_gl = sph%sph_rj%nidx_rj(1)
      tc_mon%nth_gl = sph%sph_rtp%nidx_global_rtp(2)
      tc_mon%kr_in =  sph%sph_params%nlayer_ICB
      tc_mon%kr_out = sph%sph_params%nlayer_CMB
!
      allocate(tc_mon%radius_gl(tc_mon%nri_gl))
      allocate(tc_mon%wr_gl(tc_mon%nri_gl))
      allocate(tc_mon%colat_gl(tc_mon%nth_gl))
      allocate(tc_mon%wt_gl(tc_mon%nth_gl))
      allocate(tc_mon%mirror_gl(tc_mon%nth_gl))
      allocate(tc_mon%uphi_lc(tc_mon%nri_gl,tc_mon%nth_gl))
      allocate(tc_mon%uphi_gl(tc_mon%nri_gl,tc_mon%nth_gl))
!
      tc_mon%radius_gl(1:tc_mon%nri_gl)                                 &
     &         = sph%sph_rj%radius_1d_rj_r(1:tc_mon%nri_gl)
      tc_mon%r_in =  tc_mon%radius_gl(tc_mon%kr_in)
      tc_mon%r_out = tc_mon%radius_gl(tc_mon%kr_out)
!
!   Trapezoidal weight for r^2 dr (same as radial_int_by_trapezoid)
      tc_mon%wr_gl(1:tc_mon%nri_gl) = zero
      do kg = tc_mon%kr_in, tc_mon%kr_out-1
        r = half * (tc_mon%radius_gl(kg+1) - tc_mon%radius_gl(kg))
        tc_mon%wr_gl(kg) =   tc_mon%wr_gl(kg)                           &
     &                     + r * tc_mon%radius_gl(kg)**2
        tc_mon%wr_gl(kg+1) = tc_mon%wr_gl(kg+1)                         &
     &                     + r * tc_mon%radius_gl(kg+1)**2
      end do
!
!   Gauss-Legendre weight normalized as \int sin(theta) d theta = 2
      wsum = sum(leg%weight_rtm(1:tc_mon%nth_gl))
      tc_mon%colat_gl(1:tc_mon%nth_gl)                                  &
     &         = leg%g_colat_rtm(1:tc_mon%nth_gl)
      tc_mon%wt_gl(1:tc_mon%nth_gl)                                     &
     &         = two * leg%weight_rtm(1:tc_mon%nth_gl) / wsum
!
      do lg = 1, tc_mon%nth_gl
        dmin = 1.0d30
        do m = 1, tc_mon%nth_gl
          s = abs(tc_mon%colat_gl(lg) + tc_mon%colat_gl(m) - pi)
          if(s .lt. dmin) then
            dmin = s
            tc_mon%mirror_gl(lg) = m
          end if
        end do
      end do
!
      allocate(tc_mon%id_region(sph%sph_rtp%nidx_rtp(1),                &
     &                          sph%sph_rtp%nidx_rtp(2)))
      vol_lc(1:ntc_region) = zero
      do lt = 1, sph%sph_rtp%nidx_rtp(2)
        lg = sph%sph_rtp%idx_gl_1d_rtp_t(lt)
        do kr = 1, sph%sph_rtp%nidx_rtp(1)
          kg = sph%sph_rtp%idx_gl_1d_rtp_r(kr)
          r = tc_mon%radius_gl(kg)
          s = r * sin(tc_mon%colat_gl(lg))
          z = r * cos(tc_mon%colat_gl(lg))
          if(abs(z) .lt. 1.0d-12 * r) z = zero
!
          if(s .lt. tc_mon%r_in) then
            ireg = 1
          else if(r .lt. tc_mon%r_split) then
            ireg = 3
          else
            ireg = 5
          end if
          if(z .lt. zero) ireg = ireg + 1
          tc_mon%id_region(kr,lt) = ireg
!
          vol_lc(ireg) = vol_lc(ireg)                                   &
     &                  + two*pi * tc_mon%wr_gl(kg) * tc_mon%wt_gl(lg)
        end do
      end do
      call calypso_mpi_allreduce_real(vol_lc, tc_mon%vol_region,        &
     &    cast_long(ntc_region), MPI_SUM)
!
      tc_mon%j10 = find_local_sph_address(sph%sph_rj, 1, 0)
!
      if(my_rank .ne. 0) return
      file_name = add_dat_extension(tc_mon%file_prefix)
      inquire(file = file_name, exist = flag_exist)
      if(flag_exist) return
!
      open(id_tc_monitor, file = file_name, status = 'new',             &
     &     form = 'formatted')
      call write_tc_region_monitor_header(id_tc_monitor, tc_mon)
      close(id_tc_monitor)
!
      end subroutine init_tc_region_monitor
!
! ----------------------------------------------------------------------
!
      subroutine output_tc_region_monitor                               &
     &         (time_d, sph, ipol, rj_fld, trns_MHD, tc_mon)
!
      use t_time_data
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
      use t_sph_trans_arrays_MHD
      use calypso_mpi_real
      use transfer_to_long_integers
      use set_parallel_file_name
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(tc_region_monitor), intent(inout) :: tc_mon
!
      integer(kind = kint), parameter :: ntot = ntc_region*ntc_quant + 1
      real(kind = kreal) :: acc_lc(ntot), acc_gl(ntot)
      real(kind = kreal) :: Ezon_A, g10
      real(kind = kreal), allocatable :: dOdz(:)
      integer(kind = kint) :: i, i_B
      character(len = kchara) :: file_name, fmt_txt
!
!
      if(tc_mon%flag_tc_monitor .eqv. .FALSE.) return
      if(mod(time_d%i_time_step, tc_mon%increment) .ne. 0) return
!
      call cal_tc_region_integrals(sph%sph_rtp, trns_MHD, tc_mon,       &
     &    acc_lc(1))
!
!   Gauss coefficient g_1^0 evaluated at r = r_o
      acc_lc(ntot) = zero
      i_B = ipol%base%i_magne
      if(tc_mon%j10 .gt. 0 .and. i_B .gt. 0) then
        i = tc_mon%j10 + (tc_mon%kr_out-1) * sph%sph_rj%nidx_rj(2)
        acc_lc(ntot) = rj_fld%d_fld(i,i_B) / tc_mon%r_out**2
      end if
!
      call calypso_mpi_reduce_real(acc_lc, acc_gl, cast_long(ntot),     &
     &    MPI_SUM, 0)
      call calypso_mpi_reduce_real(tc_mon%uphi_lc, tc_mon%uphi_gl,      &
     &    cast_long(tc_mon%nri_gl*tc_mon%nth_gl), MPI_SUM, 0)
!
      if(my_rank .ne. 0) return
!
      Ezon_A = cal_Ezon_asym(tc_mon)
      g10 = acc_gl(ntot)
      allocate(dOdz(tc_mon%num_shear))
      do i = 1, tc_mon%num_shear
        dOdz(i) = cal_zonal_shear_fit(tc_mon%s_shear(i), tc_mon)
      end do
!
      write(fmt_txt,'(a,i4,a)')                                         &
     &        '(i16,1pE25.15e3,', (ntot+1+tc_mon%num_shear), 'ES16.8)'
      file_name = add_dat_extension(tc_mon%file_prefix)
      open(id_tc_monitor, file = file_name, status = 'unknown',         &
     &     position = 'append', form = 'formatted')
      write(id_tc_monitor,fmt_txt) time_d%i_time_step, time_d%time,     &
     &      acc_gl(1:ntot-1), Ezon_A, g10, dOdz(1:tc_mon%num_shear)
      close(id_tc_monitor)
      deallocate(dOdz)
!
      end subroutine output_tc_region_monitor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_tc_region_integrals(sph_rtp, trns_MHD, tc_mon,     &
     &                                   acc)
!
      use t_spheric_rtp_data
      use t_sph_trans_arrays_MHD
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(tc_region_monitor), intent(inout) :: tc_mon
      real(kind = kreal), intent(inout)                                 &
     &                   :: acc(ntc_quant,ntc_region)
!
      integer(kind = kint) :: i_u, i_b, i_f, nr, nt, np
      integer(kind = kint) :: klt, kr, lt, mp, kg, lg, ireg, inod, ist
      real(kind = kreal) :: wa, wn, ct, st, anp
      real(kind = kreal) :: ub(3), bb(3), du(3), u(3), b(3), f(3)
      real(kind = kreal) :: uzb, a(ntc_quant)
!
!
      i_u = trns_MHD%b_trns%base%i_velo
      i_b = trns_MHD%b_trns%base%i_magne
      i_f = trns_MHD%f_trns%forces%i_lorentz
      nr = sph_rtp%nidx_rtp(1)
      nt = sph_rtp%nidx_rtp(2)
      np = sph_rtp%nidx_rtp(3)
      anp = one / dble(np)
!
      acc(1:ntc_quant,1:ntc_region) = zero
      tc_mon%uphi_lc(1:tc_mon%nri_gl,1:tc_mon%nth_gl) = zero
!
!$omp parallel do private(klt,kr,lt,mp,kg,lg,ireg,inod,ist,wa,wn,ct,st, &
!$omp&                    ub,bb,du,u,b,f,uzb,a) reduction(+:acc)
      do klt = 1, nr*nt
        kr = mod(klt-1,nr) + 1
        lt = (klt-1) / nr + 1
        kg = sph_rtp%idx_gl_1d_rtp_r(kr)
        lg = sph_rtp%idx_gl_1d_rtp_t(lt)
        if(tc_mon%wr_gl(kg) .eq. zero) cycle
!
        ireg = tc_mon%id_region(kr,lt)
        ct = cos(tc_mon%colat_gl(lg))
        st = sin(tc_mon%colat_gl(lg))
        wa = two*pi * tc_mon%wr_gl(kg) * tc_mon%wt_gl(lg)
        wn = wa * anp
        ist = 1 + (kr-1) * sph_rtp%istep_rtp(1)                         &
     &          + (lt-1) * sph_rtp%istep_rtp(2)
!
        ub(1:3) = zero
        bb(1:3) = zero
        do mp = 1, np
          inod = ist + (mp-1) * sph_rtp%istep_rtp(3)
          if(i_u .gt. 0) ub(1:3) = ub(1:3)                              &
     &        + trns_MHD%backward%fld_rtp(inod,i_u:i_u+2)
          if(i_b .gt. 0) bb(1:3) = bb(1:3)                              &
     &        + trns_MHD%backward%fld_rtp(inod,i_b:i_b+2)
        end do
        ub(1:3) = ub(1:3) * anp
        bb(1:3) = bb(1:3) * anp
        uzb = ub(1) * ct - ub(2) * st
!
        a(1:ntc_quant) = zero
        u(1:3) = zero
        b(1:3) = zero
        f(1:3) = zero
        do mp = 1, np
          inod = ist + (mp-1) * sph_rtp%istep_rtp(3)
          if(i_u .gt. 0) u(1:3) = trns_MHD%backward%fld_rtp(inod,i_u:i_u+2)
          if(i_b .gt. 0) b(1:3) = trns_MHD%backward%fld_rtp(inod,i_b:i_b+2)
          if(i_f .gt. 0) f(1:3) = trns_MHD%forward%fld_rtp(inod,i_f:i_f+2)
          du(1:3) = u(1:3) - ub(1:3)
          a(3) = a(3) + du(1)**2 + du(2)**2 + du(3)**2
          a(4) = a(4) + (du(1)*ct - du(2)*st)**2
          a(5) = a(5) + b(1)**2 + b(2)**2 + b(3)**2
          a(7) = a(7) + u(1)*f(1) + u(2)*f(2) + u(3)*f(3)
        end do
!
        acc(1,ireg) = acc(1,ireg) + half * wa * ub(3)**2
        acc(2,ireg) = acc(2,ireg) + half * wa * (ub(1)**2 + ub(2)**2)
        acc(3,ireg) = acc(3,ireg) + half * wn * a(3)
        acc(4,ireg) = acc(4,ireg) + half * wn * a(4)
        acc(5,ireg) = acc(5,ireg) + half * wn * a(5)
        acc(6,ireg) = acc(6,ireg)                                       &
     &               + half * wa * (bb(1)**2 + bb(2)**2 + bb(3)**2)
        acc(7,ireg) = acc(7,ireg) + wn * a(7)
!
        tc_mon%uphi_lc(kg,lg) = ub(3)
      end do
!$omp end parallel do
!
      end subroutine cal_tc_region_integrals
!
! ----------------------------------------------------------------------
!
      real(kind = kreal) function cal_Ezon_asym(tc_mon)
!
      type(tc_region_monitor), intent(in) :: tc_mon
!
      integer(kind = kint) :: kg, lg
      real(kind = kreal) :: ua
!
!
      cal_Ezon_asym = zero
      do lg = 1, tc_mon%nth_gl
        do kg = tc_mon%kr_in, tc_mon%kr_out
          ua = half * (tc_mon%uphi_gl(kg,lg)                            &
     &               - tc_mon%uphi_gl(kg,tc_mon%mirror_gl(lg)))
          cal_Ezon_asym = cal_Ezon_asym + half * two*pi                 &
     &                   * tc_mon%wr_gl(kg) * tc_mon%wt_gl(lg) * ua**2
        end do
      end do
!
      end function cal_Ezon_asym
!
! ----------------------------------------------------------------------
!
!>    Least-squares slope d Omega / d z of <u_phi>/s along a column
!!    at cylindrical radius s0, for |z| < sqrt(r_o^2 - s0^2) - 0.05.
!!    Points are where the column crosses each Gauss colatitude;
!!    <u_phi> is linearly interpolated in r.
      real(kind = kreal) function cal_zonal_shear_fit(s0, tc_mon)
!
      real(kind = kreal), intent(in) :: s0
      type(tc_region_monitor), intent(in) :: tc_mon
!
      integer(kind = kint) :: lg, kg, num
      real(kind = kreal) :: r, z, zmax, c, omega
      real(kind = kreal) :: sz, so, szz, szo
!
!
      cal_zonal_shear_fit = zero
      if(s0 .ge. tc_mon%r_out) return
      zmax = sqrt(tc_mon%r_out**2 - s0**2) - 0.05d0
!
      num = 0
      sz =  zero
      so =  zero
      szz = zero
      szo = zero
      do lg = 1, tc_mon%nth_gl
        r = s0 / sin(tc_mon%colat_gl(lg))
        z = r * cos(tc_mon%colat_gl(lg))
        if(r .lt. tc_mon%r_in .or. r .gt. tc_mon%r_out) cycle
        if(abs(z) .ge. zmax) cycle
!
        do kg = tc_mon%kr_in, tc_mon%kr_out-1
          if(r .le. tc_mon%radius_gl(kg+1)) exit
        end do
        kg = min(kg, tc_mon%kr_out-1)
        c = (r - tc_mon%radius_gl(kg))                                  &
     &     / (tc_mon%radius_gl(kg+1) - tc_mon%radius_gl(kg))
        omega = ((one - c) * tc_mon%uphi_gl(kg,  lg)                    &
     &         +        c  * tc_mon%uphi_gl(kg+1,lg)) / s0
!
        num = num + 1
        sz =  sz +  z
        so =  so +  omega
        szz = szz + z*z
        szo = szo + z*omega
      end do
!
      if(num .lt. 2) return
      cal_zonal_shear_fit = (dble(num)*szo - sz*so)                     &
     &                     / (dble(num)*szz - sz*sz)
!
      end function cal_zonal_shear_fit
!
! ----------------------------------------------------------------------
!
      subroutine write_tc_region_monitor_header(id_file, tc_mon)
!
      integer(kind = kint), intent(in) :: id_file
      type(tc_region_monitor), intent(in) :: tc_mon
!
      integer(kind = kint) :: ireg, iq, i
      character(len = 16) :: label
!
!
      write(id_file,'(a)')                                              &
     &  '# tc_region_monitor: regional volume integrals (not averages)'
      write(id_file,'(a)')                                              &
     &  '# Divide sums by V_shell to compare with sph_pwr_volume'
      write(id_file,'(a,1p3E25.15e3)') '# r_i, r_o, r_split: ',         &
     &     tc_mon%r_in, tc_mon%r_out, tc_mon%r_split
      write(id_file,'(a,1pE25.15e3)') '# V_shell: ',                   &
     &     sum(tc_mon%vol_region)
      write(id_file,'(a,6(1x,a))') '# regions:',                        &
     &     (trim(tc_region_name(ireg)), ireg = 1, ntc_region)
      write(id_file,'(a,1p6E25.15e3)') '# volumes:',                    &
     &     tc_mon%vol_region(1:ntc_region)
      write(id_file,'(a)')                                              &
     &  '# TC: s < r_i; N: z >= 0; CMB: r >= r_split'
      write(id_file,'(a)')                                              &
     &  '# WL = int u.F_Lorentz dV, with coef_lor in F_Lorentz'
      write(id_file,'(a)')                                              &
     &  '# g10_CMB: Gauss coefficient g_1^0 at r = r_o'
      if(tc_mon%num_shear .gt. 0) then
        write(id_file,'(a,1p10E16.8)')                                  &
     &     '# dOdz shear-fit radii s: ', tc_mon%s_shear(:)
      end if
!
      write(id_file,'(a)', advance='NO') '# i_step  time'
      do ireg = 1, ntc_region
        do iq = 1, ntc_quant
          write(label,'(a,a1,a)') trim(tc_region_name(ireg)), '_',      &
     &                          trim(tc_quant_name(iq))
          write(id_file,'(2x,a)', advance='NO') trim(label)
        end do
      end do
      write(id_file,'(a)', advance='NO') '  Ezon_A  g10_CMB'
      do i = 1, tc_mon%num_shear
        write(label,'(a,f4.2)') 'dOdz_s', tc_mon%s_shear(i)
        write(id_file,'(2x,a)', advance='NO') trim(label)
      end do
      write(id_file,'(a)') ''
!
      end subroutine write_tc_region_monitor_header
!
! ----------------------------------------------------------------------
!
      end module t_tc_region_monitor
