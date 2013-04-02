!
!      module set_filter_moments_3d
!
      module set_filter_moments_3d
!
!      modified by H. Matsui on July, 2006
!      modified by H. Matsui on Mar, 2008
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_moments_order
!      subroutine s_set_seeds_moments(dx, dy, dz)
!      subroutine cal_ref_rms_filter(rms_filter, dxdxi, dxdei, dxdzi,   &
!     &          dydxi, dydei, dydzi, dzdxi, dzdei, dzdzi)
!      subroutine s_set_seeds_moments_org(dx, dy, dz)
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_moments_order
!
      use m_ctl_params_4_gen_filter
      use m_reference_moments
!
      integer(kind = kint) :: icou, iflag
      integer(kind = kint) :: n, i, ix, iy, iz
!
!
      icou = 0
      do n = 1, num_moments_order
!
        do iz = 1, n
          do iy = 1, n
            do ix = 1, n
!
              if (ix.ge.n .or. iy.ge.n .or. iz.ge.n) then
                iflag = 0
                do i = 1, icou
                  if     (id_moments(i,1).eq.ix                         &
     &              .and. id_moments(i,2).eq.iy                         &
     &              .and. id_moments(i,3).eq.iz) then
                    iflag = 1
                    exit
                  end if
                end do
!
                if (iflag .eq. 0) then
                  icou = icou + 1
                  if (icou .gt. max_num_order_3d) go to 10
                  id_moments(icou,1) = ix
                  id_moments(icou,2) = iy
                  id_moments(icou,3) = iz
                end if
!
                iflag = 0
                do i = 1, icou
                  if     (id_moments(i,2).eq.ix                         &
     &              .and. id_moments(i,3).eq.iy                         &
     &              .and. id_moments(i,1).eq.iz) then
                    iflag = 1
                    exit
                  end if
                end do
!
                if (iflag .eq. 0) then
                  icou = icou + 1
                  if (icou .gt. max_num_order_3d) go to 10
                  id_moments(icou,2) = ix
                  id_moments(icou,3) = iy
                  id_moments(icou,1) = iz
                end if
!
                iflag = 0
                do i = 1, icou
                  if     (id_moments(i,3).eq.ix                         &
     &              .and. id_moments(i,1).eq.iy                         &
     &              .and. id_moments(i,2).eq.iz) then
                    iflag = 1
                    exit
                  end if
                end do
!
                if (iflag .eq. 0) then
                  icou = icou + 1
                  if (icou .gt. max_num_order_3d) go to 10
                  id_moments(icou,3) = ix
                  id_moments(icou,1) = iy
                  id_moments(icou,2) = iz
                end if
!
              end if
!
            end do
          end do
        end do
!
      end do
 10   continue
!
!
      do icou = 1, num_order_3d
                ix = id_moments(icou,1)
                iy = id_moments(icou,2)
                iz = id_moments(icou,3)
                iorder_mom_3d(icou,1) = mom_order(ix)
                iorder_mom_3d(icou,2) = mom_order(iy)
                iorder_mom_3d(icou,3) = mom_order(iz)
      end do
!
!      write(75,*) 'num_moments_order', num_moments_order
!      write(75,*) 'icou, iorder_mom_3d(icou,1:3)'
!      write(75,*)  max_num_order_1d, num_order_3d
!      do icou = 1, num_order_3d
!        write(75,*) icou, iorder_mom_3d(icou,1:3)
!      end do
!      close(75)
!
      end subroutine s_set_moments_order
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_set_table_4_moments
!
      use m_ctl_params_4_gen_filter
      use m_reference_moments
!
!
      integer(kind = kint) :: n, m, mcou, mst_pre, mst_cor
      integer(kind = kint) :: mx, my, mz
!
!
      call allocate_istack_power
!
      istack_power(-1) = -1
      istack_power(0) =   0
      do n = 1, max_num_order_1d
        istack_power(n) = istack_power(n-1) + 3**n
      end do
      ntot_power = istack_power(max_num_order_1d)
!
      call allocate_itbl_power
!
      itbl_power(1,0) = 0
      itbl_power(2,0) = 0
      itbl_power(3,0) = 0
      do n = 1, max_num_order_1d
        mst_pre = istack_power(n-2)
        mst_cor = istack_power(n-1)
        do m = 1, 3**(n-1)
          mcou = mst_pre + m
          mx = mst_cor + 3*m-2
          my = mst_cor + 3*m-1
          mz = mst_cor + 3*m
          itbl_power(1,mx) = itbl_power(1,mcou) + 1
          itbl_power(2,mx) = itbl_power(2,mcou)
          itbl_power(3,mx) = itbl_power(3,mcou)
          itbl_power(1,my) = itbl_power(1,mcou)
          itbl_power(2,my) = itbl_power(2,mcou) + 1
          itbl_power(3,my) = itbl_power(3,mcou)
          itbl_power(1,mz) = itbl_power(1,mcou)
          itbl_power(2,mz) = itbl_power(2,mcou)
          itbl_power(3,mz) = itbl_power(3,mcou) + 1
        end do
      end do
!
      call allocate_coef_4_filter_moms
!
      end subroutine s_set_table_4_moments
!
! ----------------------------------------------------------------------
!
      subroutine s_set_seeds_moments(dxdxi, dxdei, dxdzi,               &
     &          dydxi, dydei, dydzi, dzdxi, dzdei, dzdzi)
!
      use m_ctl_params_4_gen_filter
      use m_reference_moments
!
      real(kind = kreal), intent(in) :: dxdxi, dxdei, dxdzi
      real(kind = kreal), intent(in) :: dydxi, dydei, dydzi
      real(kind = kreal), intent(in) :: dzdxi, dzdei, dzdzi
!
      integer(kind = kint) :: n, m, mcou, mst_pre, mst_cor
      integer(kind = kint) :: nmax_x, im_x, mst_x, med_x, mx, ip_xi
      integer(kind = kint) :: nmax_y, im_y, mst_y, med_y, my, ip_ei
      integer(kind = kint) :: nmax_z, im_z, mst_z, med_z, mz, ip_zi
!
!
!
      nmax_x = iorder_mom_3d(1,1)
      nmax_y = iorder_mom_3d(1,2)
      nmax_z = iorder_mom_3d(1,3)
      do n = 2, num_order_3d
        nmax_x = max(nmax_x,iorder_mom_3d(n,1))
        nmax_y = max(nmax_y,iorder_mom_3d(n,2))
        nmax_z = max(nmax_z,iorder_mom_3d(n,3))
      end do
!
      ipower_x_xi = 0
      ipower_x_ei = 0
      ipower_x_zi = 0
      ipower_y_xi = 0
      ipower_y_ei = 0
      ipower_y_zi = 0
      ipower_z_xi = 0
      ipower_z_ei = 0
      ipower_z_zi = 0
!
      coef_x(0) = 1.0d0
      do n = 1, nmax_x
        mst_pre = istack_power(n-2)
        mst_cor = istack_power(n-1)
        do m = 1, 3**(n-1)
          mcou = mst_pre + m
          mx = mst_cor + 3*m-2
          my = mst_cor + 3*m-1
          mz = mst_cor + 3*m
          coef_x(mx) = coef_x(mcou) * dxdxi
          coef_x(my) = coef_x(mcou) * dxdei
          coef_x(mz) = coef_x(mcou) * dxdzi
          ipower_x_xi(mx) = ipower_x_xi(mcou) + 1
          ipower_x_ei(my) = ipower_x_ei(mcou) + 1
          ipower_x_zi(mz) = ipower_x_zi(mcou) + 1
        end do
      end do
!
      coef_y(0) = 1.0d0
      do n = 1, nmax_y
        mst_pre = istack_power(n-2)
        mst_cor = istack_power(n-1)
        do m = 1, 3**(n-1)
          mcou = mst_pre + m
          mx = mst_cor + 3*m-2
          my = mst_cor + 3*m-1
          mz = mst_cor + 3*m
          coef_y(mx) = coef_y(mcou) * dydxi
          coef_y(my) = coef_y(mcou) * dydei
          coef_y(mz) = coef_y(mcou) * dydzi
          ipower_y_xi(mx) = ipower_y_xi(mcou) + 1
          ipower_y_ei(my) = ipower_y_ei(mcou) + 1
          ipower_y_zi(mz) = ipower_y_zi(mcou) + 1
        end do
      end do
!
      coef_z(0) = 1.0d0
      do n = 1, nmax_z
        mst_pre = istack_power(n-2)
        mst_cor = istack_power(n-1)
        do m = 1, 3**(n-1)
          mcou = mst_pre + m
          mx = mst_cor + 3*m-2
          my = mst_cor + 3*m-1
          mz = mst_cor + 3*m
          coef_z(mx) = coef_z(mcou) * dzdxi
          coef_z(my) = coef_z(mcou) * dzdei
          coef_z(mz) = coef_z(mcou) * dzdzi
          ipower_z_xi(mx) = ipower_z_xi(mcou) + 1
          ipower_z_ei(my) = ipower_z_ei(mcou) + 1
          ipower_z_zi(mz) = ipower_z_zi(mcou) + 1
        end do
      end do
!
!
      seed_moments = 0.0d0
      do n = 1, num_order_3d
        im_x = iorder_mom_3d(n,1)
        im_y = iorder_mom_3d(n,2)
        im_z = iorder_mom_3d(n,3)
!
!          write(70+my_rank,*) 'im_x', n, im_x, im_y, im_z
        if( (mod((im_x+im_y+im_z),2)) .eq. 0 ) then
!        if( (mod((im_x),2)).eq.0 .and. (mod((im_y),2)).eq.0 .and. (mod((im_z),2)).eq.0 ) then
          mst_x = istack_power(im_x-1) + 1
          med_x = istack_power(im_x)
          mst_y = istack_power(im_y-1) + 1
          med_y = istack_power(im_y)
          mst_z = istack_power(im_z-1) + 1
          med_z = istack_power(im_z)
!
!          write(70+my_rank,*) 'mst_x', n, mst_x, med_x,  mst_y, med_y,  mst_z, med_z
          do mx = mst_x, med_x
            do my = mst_y, med_y
              do mz = mst_z, med_z
                ip_xi = itbl_power(1,mx)                                &
     &                + itbl_power(1,my)                                &
     &                + itbl_power(1,mz)
                ip_ei = itbl_power(2,mx)                                &
     &                + itbl_power(2,my)                                &
     &                + itbl_power(2,mz)
                ip_zi = itbl_power(3,mx)                                &
     &                + itbl_power(3,my)                                &
     &                + itbl_power(3,mz)
                if (   (mod(ip_xi,2)).eq.0 .and. (mod(ip_ei,2)).eq.0    &
     &           .and. (mod(ip_zi,2)).eq.0) then
                  seed_moments(n) = seed_moments(n)                     &
     &                           +  coef_x(mx) * ref_moments_1d(ip_xi)  &
     &                            * coef_y(my) * ref_moments_1d(ip_ei)  &
     &                            * coef_z(mz) * ref_moments_1d(ip_zi)
!                  seed_moments(n) = seed_moments(n)                    &
!     &                           +  dxdxi**ipower_x_xi(mx)             &
!     &                            * dxdei**ipower_x_ei(mx)             &
!     &                            * dxdzi**ipower_x_zi(mx)             &
!     &                            * dydxi**ipower_y_xi(my)             &
!     &                            * dydei**ipower_y_ei(my)             &
!     &                            * dydzi**ipower_y_zi(my)             &
!     &                            * dzdxi**ipower_z_xi(mz)             &
!     &                            * dzdei**ipower_z_ei(mz)             &
!     &                            * dzdzi**ipower_z_zi(mz)             &
!     &                            * ref_moments_1d(ip_xi)              &
!     &                            * ref_moments_1d(ip_ei)              &
!     &                            * ref_moments_1d(ip_zi)
                end if
              end do
            end do
          end do
!
        else
          seed_moments(n) = 0.0d0
        end if
!
      end do
!
!      write(70+my_rank,*) 'dxdxi, dxdei, dxdzi', dxdxi, dxdei, dxdzi
!      write(70+my_rank,*) 'dydxi, dydei, dydzi', dydxi, dydei, dydzi
!      write(70+my_rank,*) 'dzdxi, dzdei, dzdzi', dzdxi, dzdei, dzdzi
!      write(70+my_rank,*) 'ref_moments_1d', ref_moments_1d
!      do n = 1, num_order_3d
!        write(70+my_rank,1000) n, iorder_mom_3d(n,1:3), seed_moments(n)
!      end do
! 1000 format('seed_moments', 4i4,1pE25.15e3)
!
      end subroutine s_set_seeds_moments
!
! ----------------------------------------------------------------------
!
      subroutine cal_ref_rms_filter(rms_filter, dxdxi, dxdei, dxdzi,    &
     &          dydxi, dydei, dydzi, dzdxi, dzdei, dzdzi)
!
      real(kind = kreal), intent(in) :: dxdxi, dxdei, dxdzi
      real(kind = kreal), intent(in) :: dydxi, dydei, dydzi
      real(kind = kreal), intent(in) :: dzdxi, dzdei, dzdzi
      real(kind = kreal), intent(inout) :: rms_filter
!
      rms_filter = dxdxi*dxdxi + dxdei*dxdei + dxdzi*dxdzi              &
     &           + dydxi*dydxi + dydei*dydei + dydzi*dydzi              &
     &           + dzdxi*dzdxi + dzdei*dzdei + dzdzi*dzdzi
!
      end subroutine cal_ref_rms_filter
!
! ----------------------------------------------------------------------
!
      subroutine s_set_seeds_moments_org(dx, dy, dz)
!
      use m_ctl_params_4_gen_filter
      use m_reference_moments
!
      real(kind = kreal), intent(in) :: dx, dy, dz
!
      integer(kind = kint) :: n, ix, iy, iz
!
!
      do n = 1, num_order_3d
        ix = id_moments(n,1)
        iy = id_moments(n,2)
        iz = id_moments(n,3)
        seed_moments(n) =  (dx**iorder_mom_3d(n,1) )*mom_value(ix)      &
     &                   * (dy**iorder_mom_3d(n,2) )*mom_value(iy)      &
     &                   * (dz**iorder_mom_3d(n,3) )*mom_value(iz)
      end do
!
!      write(70+my_rank,*) 'dx, dy, dz', dx, dy, dz
!      write(70+my_rank,*) 'mom_value', mom_value(ix), mom_value(iy), mom_value(iz)
!      do n = 1, num_order_3d
!        write(70+my_rank,1000)  n, iorder_mom_3d(n,1:3), seed_moments(n)
!      end do
! 1000 format('seed_moments_org', 4i4,1pE25.15e3)
!
      end subroutine s_set_seeds_moments_org
!
! ----------------------------------------------------------------------
!
      end module set_filter_moments_3d
