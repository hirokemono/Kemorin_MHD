!
!     module cal_fft_for_horizontal
!
      module cal_fft_for_horizontal
!
!      Written by H.Matsui
!      Modified by H.Matsui on June, 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: Nsmp
      integer(kind = kint), allocatable :: Nstacksmp(:)
      private :: Nsmp, Nstacksmp
!
!      subroutine s_cal_fft_for_horizontal
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_fft_for_horizontal
!
      use m_spectr_4_ispack
      use FFT_selector
!
      integer(kind=kint ) :: i, j, ix, iy, iz, i1, i2, i3, i4, i5, i6
      integer(kind=kint ) :: i_org, n1, inod
!
!
!
      Nsmp = num_fft
      allocate (Nstacksmp(0:Nsmp))
!
!    set array
!
!      do j = 1, num_fft
!       do iy = 1, ky_max
!        do ix = 1, kx_max
!         do iz = 1, iz_max
!
!          i = (j-1)*num_spectr + ix + (iy-1)*kx_max                    &
!     &          + (iz-1)*(kx_max*ky_max)
!
!           write(40,*) j, iz, iy, ix, phys_d(i)
!
!          end do
!         end do
!        end do
!       end do
!
      do j = 1, num_fft
       do iz = 1, iz_max
        do iy = 1, ky_max
         do ix = 1, kx_max
!
          i_org = (j-1)*num_spectr + ix + (iy-1)*kx_max                 &
     &          + (iz-1)*(kx_max*ky_max)
          i = (ix-1)*(num_fft*iz_max*ky_max) + (j-1)*(iz_max*ky_max)    &
     &          + (iz-1)*ky_max + iy
!
           work(i) = phys_d (i_org)
!
          end do
         end do
        end do
       end do
!
      n1 = num_fft*iz_max*ky_max
      Nstacksmp(0) = 0
      do j = 1, num_fft
        Nstacksmp(j) = Nstacksmp(j-1) + iz_max*kx_max
      end do
!
      call verify_FFT_select(num_fft, Nstacksmp, kx_max)
      call forward_FFT_select(Nsmp, Nstacksmp, n1, kx_max, work)
!
!    swap array
!
      do j = 1, num_fft
        do inod = 1, num_spectr
          i = (j-1)*num_spectr + inod
          phys_d(i) = work(i)
        end do
      end do
!
!      do j = 1, num_fft
!       do iy = 1, ky_max
!        do ix = 1, kx_max
!         do iz = 1, iz_max
!
!          i = (ix-1)*(num_fft*iz_max*ky_max) + (j-1)*(iz_max*ky_max)   &
!     &          + (iz-1)*ky_max + iy
!
!           write(41,*) j, iz, iy, ix, phys_d (i)
!
!          end do
!         end do
!        end do
!       end do
!
      do j = 1, num_fft
       do iz = 1, iz_max
        do ix = 1, kx_max
         do iy = 1, ky_max
!
          i_org = (ix-1)*(num_fft*iz_max*ky_max)                        &
     &           + (j-1)*(iz_max*ky_max) + (iz-1)*ky_max + iy
          i = (iy-1)*(num_fft*iz_max*kx_max) + (j-1)*(iz_max*kx_max)    &
     &          + (iz-1)*kx_max + ix
!
          work(i) = phys_d(i_org)
!
         end do
        end do
       end do
      end do
!
      n1 = num_fft*iz_max*kx_max
      Nstacksmp(0) = 0
      do j = 1, num_fft
        Nstacksmp(j) = Nstacksmp(j-1) + iz_max*kx_max
      end do
!
      write(*,*) 'forward_FFT_select', n1, ky_max
      call verify_FFT_select(num_fft, Nstacksmp, ky_max)
      call forward_FFT_select(Nsmp, Nstacksmp, n1, ky_max, work)
!
!    swap array
!
!
      do j = 1, num_fft
       do iz = 1, iz_max
        do ix = 1, kx_max
         do iy = 1, ky_max
!
          i_org = (iy-1)*(num_fft*iz_max*kx_max)                        &
     &           + (j-1)*(iz_max*kx_max) + (iz-1)*kx_max + ix 
          i     = (j-1)*num_spectr + (iy-1)*(iz_max*kx_max)             &
     &           + (ix-1)*iz_max + iz
!
          phys_io(i) = work(i_org)
!
         end do
        end do
       end do
      end do
!
!      do j = 1, num_fft
!       do iy = 1, ky_max
!        do ix = 1, kx_max
!         do iz = 1, iz_max
!
!          i   = (j-1)*num_spectr + (iy-1)*(iz_max*kx_max)              &
!     &          + (ix-1)*iz_max + iz
!
!           write(42,*) j, iz, iy, ix, phys_io (i)
!
!          end do
!         end do
!        end do
!       end do
!
!
!    lead amplitude
!
       do j = 1, num_fft
        if ( icomp_fft(j) .eq. -1) then
         do iz = 1, iz_max
          do ix = 1, kx_max
           do iy = 1, ky_max
!
            i1 = (j-4)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i2 = (j-3)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i3 = (j-2)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i = (j-1)*num_spectr + (iy-1)*(iz_max*kx_max)               &
     &          + (ix-1)*iz_max + iz
!
            phys_io(i) = sqrt( phys_io(i1) * phys_io(i1)                &
     &                           + phys_io(i2) * phys_io(i2)            &
     &                           + phys_io(i3) * phys_io(i3) )
!
           end do
          end do
         end do
        end if
       end do
!
!    lead amplitude
!
       do j = 1, num_fft
        if ( icomp_fft(j) .eq. -2) then
         do iz = 1, iz_max
          do ix = 1, kx_max
           do iy = 1, ky_max
!
            i1 = (j-7)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i2 = (j-6)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i3 = (j-5)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i4 = (j-4)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i5 = (j-3)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i6 = (j-2)*num_spectr + (iy-1)*(iz_max*kx_max)              &
     &          + (ix-1)*iz_max + iz
            i = (j-1)*num_spectr + (iy-1)*(iz_max*kx_max)               &
     &          + (ix-1)*iz_max + iz
!
            phys_io(i) =    phys_io(i1) * phys_io(i1)                   &
     &                  + 2*phys_io(i2) * phys_io(i2)                   &
     &                  + 2*phys_io(i3) * phys_io(i3)                   &
     &                  +   phys_io(i4) * phys_io(i4)                   &
     &                  + 2*phys_io(i5) * phys_io(i5)                   &
     &                  +   phys_io(i6) * phys_io(i6)
!
           end do
          end do
         end do
        end if
       end do
!
      deallocate(Nstacksmp)
!
      end subroutine s_cal_fft_for_horizontal
!
!  ---------------------------------------------------------------------
!
      end module cal_fft_for_horizontal
