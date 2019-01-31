!
!      module cal_circle_position
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine allocate_square_circ_posi_tmp(c_sphere)
!!      subroutine deallocate_square_circ_posi_tmp
!!
!!      subroutine projection_2_circle                                  &
!!     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!!      subroutine adjust_to_circle                                     &
!!     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!!      subroutine back_to_square                                       &
!!     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!
!!      subroutine projection_to_circle_quad                            &
!!     &         (ifile, rprm_csph, c_sphere, inod)
!!      subroutine adjust_to_circle_quad                                &
!!     &         (ifile, rprm_csph, c_sphere, inod)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      module cal_circle_position
!
      use m_precision
      use m_constants
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_radius
!
      implicit  none
!
      real(kind= kreal), allocatable :: x(:), y(:)
      private :: x, y
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_square_circ_posi_tmp(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      integer(kind = kint) :: num
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      allocate(x(num), y(num))
      x = 0.0d0
      y = 0.0d0
!
      end subroutine allocate_square_circ_posi_tmp
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_square_circ_posi_tmp
!
      deallocate(x, y)
!
      end subroutine deallocate_square_circ_posi_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine projection_2_circle                                    &
     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = rprm_csph%nr_adj+1, rprm_csph%nr_back
        do inod0 = 1, c_sphere%numnod_sf
          ratio = rprm_csph%r_nod(k) / c_sphere%r_csph(inod0)
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), zero
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i15,1p3E25.15e3)')                         &
     &          inod, x(inod0), y(inod0), zero
          end if
        end do
      end do
!
      end subroutine projection_2_circle
!
!   --------------------------------------------------------------------
!
      subroutine adjust_to_circle                                       &
     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = 1, rprm_csph%nr_adj
        do inod0 = 1, c_sphere%numnod_sf
          ratio = (dble(rprm_csph%nr_adj - k)                           &
     &           + dble(k-1)*rprm_csph%r_nod(1)/c_sphere%r_csph(inod0)) &
     &            * rprm_csph%r_nod(k)                                  &
     &            / (dble(rprm_csph%nr_adj-1) * rprm_csph%r_nod(1))
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), zero
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i15,1p3E25.15e3)')                         &
     &          inod, x(inod0), y(inod0), zero
          end if
        end do
      end do
!
      end subroutine adjust_to_circle
!
!   --------------------------------------------------------------------
!
      subroutine back_to_square                                         &
     &         (ifile, ifile_q, rprm_csph, c_sphere, inod)
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = rprm_csph%nr_back+1, rprm_csph%n_shell
        do inod0 = 1, c_sphere%numnod_sf
          ratio = (dble(k-rprm_csph%nr_back)                            &
     &           + dble(rprm_csph%n_shell-k) * rprm_csph%r_nod(1)       &
     &                 / c_sphere%r_csph(inod0)) * rprm_csph%r_nod(k)   &
     &           / ( dble(rprm_csph%n_shell - rprm_csph%nr_back)        &
     &              * rprm_csph%r_nod(1))
!
      write(*,*) 'nr_back', rprm_csph%nr_back, rprm_csph%n_shell, ratio
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), zero
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i15,1p3E25.15e3)')                         &
     &          inod, x(inod0), y(inod0), zero
          end if
        end do
      end do
!
      end subroutine back_to_square
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine projection_to_circle_quad                              &
     &         (ifile, rprm_csph, c_sphere, inod)
!
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0, iedge0, num
      real(kind = kreal) :: ratio
!
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      do k = rprm_csph%nr_adj+1, rprm_csph%nr_back
        do inod0 = 1, c_sphere%numnod_sf
          ratio = half * (rprm_csph%r_nod(k)+rprm_csph%r_nod(k-1))      &
     &           / c_sphere%r_csph(inod0)
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do iedge0 = 1, c_sphere%numedge_sf
          inod0 = iedge0 + c_sphere%numnod_sf
          ratio = rprm_csph%r_nod(k)/c_sphere%r_csph(inod0)
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do inod0 = 1, c_sphere%numnod_sf + c_sphere%numedge_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), zero
        end do
      end do
!
      end subroutine projection_to_circle_quad
!
!   --------------------------------------------------------------------
!
      subroutine adjust_to_circle_quad                                  &
     &         (ifile, rprm_csph, c_sphere, inod)
!
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0, iedge0, num
      real(kind = kreal) :: ratio1, ratio2
!
      real(kind = kreal) :: ratio
!
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      do k = 1, rprm_csph%nr_adj-1
        do inod0 = 1, c_sphere%numnod_sf
          ratio1 = (dble(rprm_csph%nr_adj - k)                          &
     &          + dble(k-1)*rprm_csph%r_nod(1)/c_sphere%r_csph(inod0))  &
     &           * rprm_csph%r_nod(k)                                   &
     &           / ( dble(rprm_csph%nr_adj - 1)*rprm_csph%r_nod(1) )
          ratio2 = (dble(rprm_csph%nr_adj - k - 1)                      &
     &          + dble(k)*rprm_csph%r_nod(1)/c_sphere%r_csph(inod0))    &
     &           * rprm_csph%r_nod(k+1)                                 &
     &           / ( dble(rprm_csph%nr_adj - 1)*rprm_csph%r_nod(1) )
          ratio = (ratio1 + ratio2) * half
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do iedge0 = 1, c_sphere%numedge_sf
          inod0 = iedge0 + c_sphere%numnod_sf
!
          ratio = (dble(rprm_csph%nr_adj - k - 1)                       &
     &             + dble(k)*rprm_csph%r_nod(1)/c_sphere%r_csph(inod0)) &
     &              * rprm_csph%r_nod(k+1)                              &
     &              / ( dble(rprm_csph%nr_adj - 1)*rprm_csph%r_nod(1) )
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do inod0 = 1, c_sphere%numnod_sf + c_sphere%numedge_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), zero
        end do
      end do
!
      end subroutine adjust_to_circle_quad
!
!   --------------------------------------------------------------------
!
      end module cal_circle_position
