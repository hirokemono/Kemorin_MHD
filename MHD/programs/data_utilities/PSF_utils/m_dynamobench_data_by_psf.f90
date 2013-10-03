!>@file   m_dynamobench_data_by_psf.f90
!!@brief  module m_dynamobench_data_by_psf
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Pick data on line defined by two surfaces
!!
!!@verbatim
!!      subroutine cal_dynamobench_data_by_psf(istep, time, dt, line)
!!@endverbatim
!
      module m_dynamobench_data_by_psf
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: id_bench = 18
!
      real(kind = kreal) :: phi(4), phi_prev(4)
      real(kind = kreal) ::  v_phi(4), b_theta(4), temp(4), omega
      integer(kind = kint) :: idx(4)
!
      private :: phi, phi_prev, v_phi, b_theta, temp, omega
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_dynamobench_data_by_psf(istep, time, dt, line)
!
      use m_psf_edge_connect
      use quicksort
!
      use t_ucd_data
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time, dt
      type(ucd_data), intent(in) :: line
!
      integer(kind = kint) :: k1, i1, i2, icou
      integer(kind = kint) :: iege1
      real(kind = kreal) :: c1, c2, coef1
!
      integer(kind = kint) :: icomp_vr, icomp_vp
      integer(kind = kint) :: icomp_bt, icomp_t
      real(kind = kreal) :: phi1, phi2
!
!
      icou = 0
      do i1 = 1, line%num_field
        if(line%phys_name(i1) .eq. 'velocity_sph') icomp_vr = icou + 1
        if(line%phys_name(i1) .eq. 'temperature') icomp_t =  icou + 1
        if(line%phys_name(i1) .eq. 'magnetic_field_sph')                &
     &                             icomp_bt =  icou + 2
        icou = icou + line%num_comp(i1)
      end do
      icomp_vp = icomp_vr + 2
!
!
      write(*,*) 'icomp_vr', icomp_vr
      icou = 0
      do iege1 = 1, line%nele
        i1 = line%ie(iege1,1)
        i2 = line%ie(iege1,2)
        c1 = line%d_ucd(i1,icomp_vr)
        c2 = line%d_ucd(i2,icomp_vr)
        phi1  = atan2(line%xx(i1,2),line%xx(i1,1))
        phi2  = atan2(line%xx(i2,2),line%xx(i2,1))
        if ( abs(phi1-phi2) .gt. atan(1.0d0) ) then
          if(phi1 .lt. 0.0d0) phi1 = phi1 + 8.0d0*atan(1.0d0)
          if(phi2 .lt. 0.0d0) phi2 = phi2 + 8.0d0*atan(1.0d0)
        end if
        if( (c1*c2).le.0.0d0 .and. (phi2-phi1)*(c2-c1).gt.0.0d0) then
          icou = icou + 1
          coef1 =  c2 / (c2 - c1)
          phi(icou) =  coef1 *  phi1 + (one - coef1) *  phi2
          v_phi(icou) =   coef1 * line%d_ucd(i1,icomp_vp)               &
     &                 + (one - coef1) * line%d_ucd(i2,icomp_vp)
          b_theta(icou)  =  coef1 * line%d_ucd(i1,icomp_bt)             &
     &                 + (one - coef1) * line%d_ucd(i2,icomp_bt)
          temp(icou) =      coef1 * line%d_ucd(i1,icomp_t)              &
     &                   + (one - coef1) * line%d_ucd(i2,icomp_t)
          if(icou .eq. 4) exit
        end if
      end do
!
      v_phi(1) = 0.25d0* (v_phi(1)+v_phi(2)+v_phi(3)+v_phi(4))
      b_theta(1) = 0.25d0* (b_theta(1)+b_theta(2)                       &
     &                     +b_theta(3)+b_theta(4))
      temp(1) = 0.25d0* (temp(1)+temp(2)+temp(3)+temp(4))
      idx = 1
!
      call quicksort_real_w_index(ifour, phi, ione, ifour, idx)
!
      omega = 0
      do k1 = 1, 4
        omega = omega + (phi(k1) - phi_prev(k1)) / dt
        phi_prev(k1) = phi(k1)
      end do
      omega = 0.25d0*omega
!
      open(id_bench, file='dynamobench.dat',position='append')
      write(id_bench,'(i10,1p12e23.15)') istep, time, phi(1:4),         &
     &                        v_phi(1), b_theta(1), temp(1), omega
      close(id_bench)
!
      end subroutine cal_dynamobench_data_by_psf
!
!-----------------------------------------------------------------------
!
      end module m_dynamobench_data_by_psf
