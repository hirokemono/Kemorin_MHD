!
!      module cal_circle_position
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine allocate_square_circ_posi_tmp(c_sphere)
!!      subroutine deallocate_square_circ_posi_tmp
!!
!!      subroutine projection_2_circle(inod, ifile, ifile_q, c_sphere)
!!      subroutine adjust_to_circle(inod, ifile, ifile_q, c_sphere)
!!      subroutine back_to_square(inod, ifile, ifile_q, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!
!!      subroutine projection_to_circle_quad(inod, ifile, c_sphere)
!!      subroutine adjust_to_circle_quad(inod, ifile, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      module cal_circle_position
!
      use m_precision
!
      use t_cubed_sph_surf_mesh
      use m_cubed_sph_radius
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
      subroutine projection_2_circle(inod, ifile, ifile_q, c_sphere)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = nr_adj+1, nr_back
        do inod0 = 1, c_sphere%numnod_sf
          ratio = r_nod(k) / c_sphere%r_csph(inod0)
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
      subroutine adjust_to_circle(inod, ifile, ifile_q, c_sphere)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = 1, nr_adj
        do inod0 = 1, c_sphere%numnod_sf
          ratio = (dble(nr_adj-k)                                       &
     &             + dble(k-1)*r_nod(1)/c_sphere%r_csph(inod0))         &
     &           * r_nod(k) / ( dble(nr_adj-1)*r_nod(1) )
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
      subroutine back_to_square(inod, ifile, ifile_q, c_sphere)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: ratio
!
!
      do k = nr_back+1, n_shell
        do inod0 = 1, c_sphere%numnod_sf
          ratio = (dble(k-nr_back)                                      &
     &            + dble(n_shell-k)*r_nod(1)/c_sphere%r_csph(inod0))    &
     &         * r_nod(k) / ( dble(n_shell-nr_back)*r_nod(1) )
!
      write(*,*) 'nr_back', nr_back, n_shell, ratio
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
      subroutine projection_to_circle_quad(inod, ifile, c_sphere)
!
      use m_constants
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: k, inod0, iedge0, num
      real(kind = kreal) :: ratio
!
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      do k = nr_adj+1, nr_back
        do inod0 = 1, c_sphere%numnod_sf
          ratio = half * (r_nod(k)+r_nod(k-1))/c_sphere%r_csph(inod0)
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do iedge0 = 1, c_sphere%numedge_sf
          inod0 = iedge0 + c_sphere%numnod_sf
          ratio = r_nod(k)/c_sphere%r_csph(inod0)
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
      subroutine adjust_to_circle_quad(inod, ifile, c_sphere)
!
      use m_constants
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
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
      do k = 1, nr_adj-1
        do inod0 = 1, c_sphere%numnod_sf
          ratio1 = (dble(nr_adj-k)                                      &
     &              + dble(k-1)*r_nod(1)/c_sphere%r_csph(inod0))        &
     &            * r_nod(k)   / ( dble(nr_adj-1)*r_nod(1) )
          ratio2 = (dble(nr_adj-k-1)                                    &
     &              + dble(k)*r_nod(1)/c_sphere%r_csph(inod0))          &
     &            * r_nod(k+1) / ( dble(nr_adj-1)*r_nod(1) )
          ratio = (ratio1 + ratio2) * half
!
          x(inod0) = ratio * c_sphere%x_csph(inod0,1)
          y(inod0) = ratio * c_sphere%x_csph(inod0,2)
        end do
!
        do iedge0 = 1, c_sphere%numedge_sf
          inod0 = iedge0 + c_sphere%numnod_sf
!
          ratio = (dble(nr_adj-k-1)                                     &
     &             + dble(k)*r_nod(1)/c_sphere%r_csph(inod0))           &
     &           * r_nod(k+1) / ( dble(nr_adj-1)*r_nod(1) )
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
