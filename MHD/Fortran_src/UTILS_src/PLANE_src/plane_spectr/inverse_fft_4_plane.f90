!
!      module inverse_fft_4_plane
!
!      Written by H. Matsui
!
!!      subroutine s_inverse_fft_4_plane(nx_all, ny_all, nz_all)
!!      subroutine copy_2_inverted_data(nx_all, ny_all, nz_all)
!!      subroutine copy_2_inverted_udt(nx_all, ny_all, merged_fld)
!!        type(phys_data), intent(inout) :: merged_fld
!
      module inverse_fft_4_plane
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: Nsmp = 1
      integer(kind = kint) :: Nstacksmp(0:Nsmp)
!
      private :: Nsmp, Nstacksmp
      private :: copy_4_inversse_fft_y, copy_4_inversse_fft_x
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine s_inverse_fft_4_plane(nx_all, ny_all, nz_all)
!
      use m_spectr_4_ispack
      use t_FFT_selector
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
!
      type(working_FFTs) :: WK_FFTS
      integer(kind=kint ) :: n1
!
!
      Nstacksmp(0) = 0
!
      call copy_4_inversse_fft_y(nx_all, ny_all, nz_all)
!
      n1 = num_fft*iz_max*kx_max
      Nstacksmp(1) = n1
!
      call verify_FFT_select(Nsmp, Nstacksmp, ky_max, WK_FFTS)
      call backward_FFT_select                                          &
     &   (Nsmp, Nstacksmp, n1, ky_max, work, WK_FFTS)
!
      call copy_4_inversse_fft_x
!
      n1 = num_fft*iz_max*ky_max
      Nstacksmp(1) = n1
!
!      write(*,*) 'start FFT', n1, kx_max
      call verify_FFT_select(Nsmp, Nstacksmp, kx_max, WK_FFTS)
      call backward_FFT_select                                          &
     &   (Nsmp, Nstacksmp, n1, kx_max, work, WK_FFTS)
!
      end subroutine s_inverse_fft_4_plane
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine copy_4_inversse_fft_y(nx_all, ny_all, nz_all)
!
      use m_spectr_4_ispack
      use m_set_new_spectr
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
!
      integer(kind = kint) :: inod, j, ix, iy, iz, i1, i2
!
!
!$omp parallel do private(j,ix,iy,iz,inod,i1)
      do j = 1, num_fft
        do iy = 1, ny_all
          do ix = 1, nx_all
            do iz = 1, nz_all
              inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
              i1   = (j-1)*num_spectr + inod
              phys_d(i1) = new_spectr(inod,j)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!
!   swap array
!
!
!$omp parallel do private(j,ix,iy,iz,i1,i2)
      do j = 1, num_fft
        do iz = 1, iz_max
          do ix = 1, kx_max
            do iy = 1, ky_max
!
              i2 = (iy-1)*(num_fft*iz_max*kx_max)                       &
     &            + (j-1)*(iz_max*kx_max) + (iz-1)*kx_max + ix
              i1 = (j-1)*num_spectr                                     &
     &            + (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
!
              work(i2) = phys_d(i1)
!
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_4_inversse_fft_y
!
!  --------------------------------------------------------------------
!
      subroutine copy_4_inversse_fft_x
!
      use m_spectr_4_ispack
!
      integer(kind = kint) :: inod, i, j, ix, iy, iz, i1, i2
!
!
!$omp parallel do private(inod,i,j)
      do j = 1, num_fft
        do inod = 1, num_spectr
          i = (j-1)*num_spectr + inod
          phys_d(i) = work(i)
        end do
      end do
!$omp end parallel do
!
!      do j = 1, num_fft
!        do iy = 1, ky_max
!         do ix = 1, kx_max
!          do iz = 1, iz_max
!
!          i1 = (iy-1)*(num_fft*iz_max*kx_max) + (j-1)*(iz_max*kx_max)  &
!     &          + (iz-1)*kx_max + ix
!
!           write(52,*) j, iz, iy, ix, phys_d(i1)
!
!          end do
!         end do
!        end do
!       end do
!
!$omp parallel do private(j,ix,iy,iz,i1,i2)
      do j = 1, num_fft
        do iz = 1, iz_max
          do ix = 1, kx_max
            do iy = 1, ky_max
!
              i2 = (ix-1)*(num_fft*iz_max*ky_max)                       &
     &            + (j-1)*(iz_max*ky_max) + (iz-1)*ky_max + iy
              i1 = (iy-1)*(num_fft*iz_max*kx_max)                       &
     &            + (j-1)*(iz_max*kx_max) + (iz-1)*kx_max + ix
!
              work(i2) = phys_d(i1)
!
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_4_inversse_fft_x
!
!  --------------------------------------------------------------------
!
      subroutine copy_2_inverted_data(nx_all, ny_all, nz_all)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
!
      integer(kind = kint) :: j, ix, iy, iz, i1, i2
!
!$omp parallel do private(j,ix,iy,iz,i1,i2)
      do j = 1, num_fft
        do iz = 1, iz_max
          do iy = 1, ky_max
            do ix = 1, kx_max
!
              i1 = (j-1)*(nx_all*ny_all*nz_all)                         &
     &            + (iz-1)*(nx_all*ny_all) + (iy-1)*nx_all + ix
              i2 = (ix-1)*(num_fft*iz_max*ky_max)                       &
     &            + (j-1)*(iz_max*ky_max)  + (iz-1)*ky_max + iy
!
              phys_d(i1) = work(i2)
!
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!      do j = 1, num_fft
!        do iy = 1, ky_max
!         do ix = 1, kx_max
!          do iz = 1, iz_max
!
!          i1 = (j-1)*(nx_all*ny_all*nz_all) + (iz-1)*(nx_all*ny_all)   &
!     &        + (iy-1)*nx_all + ix
!            write(53,*) j, iz, iy, ix, phys_d (i1)
!
!          end do
!         end do
!        end do
!       end do
!
      end subroutine copy_2_inverted_data
!
!  --------------------------------------------------------------------
!
      subroutine copy_2_inverted_udt(nx_all, ny_all, merged_fld)
!
      use m_spectr_4_ispack
      use t_phys_data
!
      integer(kind=kint), intent(in) :: nx_all, ny_all
      type(phys_data), intent(inout) :: merged_fld
!
      integer(kind = kint) :: j, ix, iy, iz, i1, i2
!
!$omp parallel do private(j,ix,iy,iz,i1,i2)
      do j = 1, num_fft
        do iz = 1, iz_max
          do iy = 1, ky_max
            do ix = 1, kx_max
!
              i1 = (iz-1)*(nx_all*ny_all) + (iy-1)*nx_all + ix
              i2 = (ix-1)*(num_fft*iz_max*ky_max)                       &
     &            + (j-1)*(iz_max*ky_max) + (iz-1)*ky_max + iy
!
              merged_fld%d_fld(i1,j) = work(i2)
!
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!      do j = 1, num_fft
!        do iy = 1, ky_max
!         do ix = 1, kx_max
!          do iz = 1, iz_max
!
!            i1 = (iz-1)*(nx_all*ny_all) + (iy-1)*nx_all + ix
!            write(53,*) j, iz, iy, ix, merged_fld%d_fld(i1,j)
!
!          end do
!         end do
!        end do
!       end do
!
      end subroutine copy_2_inverted_udt
!
!  --------------------------------------------------------------------
!
      end module inverse_fft_4_plane
