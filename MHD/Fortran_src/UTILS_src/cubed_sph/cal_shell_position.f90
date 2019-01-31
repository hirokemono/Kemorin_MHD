!
!      module cal_shell_position
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine allocate_cubed_sph_posi_tmp(c_sphere)
!!      subroutine deallocate_cubed_sph_posi_tmp
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!
!!      subroutine project_to_sphere                                    &
!!     &         (ifile, ifile_q, num_h, num_v, inod, c_sphere)
!!      subroutine projection_quad(ifile, num_h, num_v, inod, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine adjust_to_shell                                      &
!!     &         (ifile, ifile_q, num_h, num_v, inod, c_sphere)
!!      subroutine adjust_to_shell_quad                                 &
!!     &         (ifile, num_h, num_v, inod, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module cal_shell_position
!
      use m_precision
      use m_constants
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
      use m_cubed_sph_radius
!
      implicit  none
!
      real(kind= kreal), allocatable :: x(:), y(:), z(:)
      real(kind= kreal), allocatable :: r(:), t(:), p(:)
      real(kind= kreal), allocatable :: ratio(:), ratio1(:), ratio2(:)
      private :: x, y, z, r, t, p, ratio, ratio1, ratio2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_cubed_sph_posi_tmp(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      integer(kind = kint) :: num
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      allocate( x(num), y(num), z(num) )
      allocate( r(num), t(num), p(num) )
      allocate( ratio(num), ratio1(num), ratio2(num) )
      x = 0.0d0
      y = 0.0d0
      z = 0.0d0
      r = 0.0d0
      t = 0.0d0
      p = 0.0d0
      ratio =  0.0d0
      ratio1 = 0.0d0
      ratio2 = 0.0d0
!
      end subroutine allocate_cubed_sph_posi_tmp
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_cubed_sph_posi_tmp
!
      deallocate( x, y, z, r, t, p, ratio, ratio1, ratio2)
!
      end subroutine deallocate_cubed_sph_posi_tmp
!
!   --------------------------------------------------------------------
!
      subroutine project_to_sphere                                      &
     &         (ifile, ifile_q, num_h, num_v, inod, c_sphere)
!
      use const_rect_sphere_surface
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      integer(kind = kint), intent(in) :: num_h, num_v
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: rad_edge
!
!
      do k = nr_adj+1, n_shell
        r(1:c_sphere%numnod_sf) = r_nod(k)
!
        if(num_edge_latitude_ref.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio(num_h, num_v, edge_latitude(k))
          rad_edge = atan(one) * edge_latitude(k) / 45.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
        end if
!
!
        call position_2_xyz(c_sphere%numnod_sf,                         &
     &      r(1), c_sphere%theta_csph(1), c_sphere%phi_csph(1),         &
     &      x(1), y(1), z(1))
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &         inod, x(inod0), y(inod0), z(inod0)
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i15,1p3E25.15e3)')                         &
     &           inod, x(inod0), y(inod0), z(inod0)
          end if
        end do
      end do
!
      end subroutine project_to_sphere
!
!   --------------------------------------------------------------------
!
      subroutine projection_quad(ifile, num_h, num_v, inod, c_sphere)
!
      use const_rect_sphere_surface
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: k, inod0, num
      real(kind = kreal) :: rad_edge
!
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      do k = nr_adj+1, n_shell
        r(1:c_sphere%numnod_sf) = (r_nod(k) + r_nod(k-1)) * half
        r(c_sphere%numnod_sf+1:num) = r_nod(k)
!
        if(num_edge_latitude_ref.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio(num_h, num_v, edge_latitude(k))
!
          rad_edge = atan(one) * (edge_latitude(k)+edge_latitude(k-1))  &
     &             / 90.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
          t(1:c_sphere%numnod_sf)                                       &
     &        = c_sphere%theta_csph(1:c_sphere%numnod_sf)
!
          rad_edge = atan(one) * edge_latitude(k) / 45.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
          t(c_sphere%numnod_sf+1:num)                                   &
     &        = c_sphere%theta_csph(c_sphere%numnod_sf+1:num)
        else
          t(1:c_sphere%numnod_sf)                                       &
     &        = c_sphere%theta_csph(1:c_sphere%numnod_sf)
          t(c_sphere%numnod_sf+1:num)                                   &
     &          = c_sphere%theta_csph(c_sphere%numnod_sf+1:num)
        end if
!
!
        call position_2_xyz(num, r(1), t(1), c_sphere%phi_csph(1),      &
     &      x(1), y(1), z(1))
!
        do inod0 = 1, c_sphere%numnod_sf + c_sphere%numedge_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &         inod, x(inod0), y(inod0), z(inod0)
        end do
      end do
!
      end subroutine projection_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine adjust_to_shell                                        &
     &         (ifile, ifile_q, num_h, num_v, inod, c_sphere)
!
      use const_rect_sphere_surface
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      integer(kind = kint), intent(in) :: num_h, num_v
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: k, inod0
      real(kind = kreal) :: rad_edge
!
!
      do k = 1, nr_adj
        if(num_edge_latitude_ref.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio(num_h, num_v, edge_latitude(k))
          rad_edge = atan(one) * edge_latitude(k) / 45.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
        end if
!
        do inod0 = 1, c_sphere%numnod_sf
          ratio(inod0) = (dble(nr_adj-k)                                &
     &                    + dble(k-1)*r_nod(1)/c_sphere%r_csph(inod0))  &
     &                  * r_nod(k) / ( dble(nr_adj-1)*r_nod(1) )
          r(inod0) = c_sphere%r_csph(inod0) * ratio(inod0)
        end do
!
        call position_2_xyz(c_sphere%numnod_sf,                         &
     &      r(1), c_sphere%theta_csph(1), c_sphere%phi_csph(1),         &
     &      x(1), y(1), z(1))
!
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), z(inod0)
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i15,1p3E25.15e3)')                         &
     &          inod, x(inod0), y(inod0), z(inod0)
          end if
!
        end do
      end do
!
      end subroutine adjust_to_shell
!
!   --------------------------------------------------------------------
!
      subroutine adjust_to_shell_quad                                   &
     &         (ifile, num_h, num_v, inod, c_sphere)
!
      use const_rect_sphere_surface
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: k, inod0, iedge0, num
      real(kind = kreal) :: rad_edge
!
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      do k = 1, nr_adj-1
        if(num_edge_latitude_ref.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio(num_h, num_v, edge_latitude(k))
!
          rad_edge = atan(one) * (edge_latitude(k)+edge_latitude(k-1))  &
     &             / 90.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
          t(1:c_sphere%numnod_sf)                                       &
     &          = c_sphere%theta_csph(1:c_sphere%numnod_sf)
!
          rad_edge = atan(one) * edge_latitude(k) / 45.0d0
          call const_rect_sphere_surf_node(rad_edge, c_sphere)
          t(c_sphere%numnod_sf+1:num)                                   &
     &          = c_sphere%theta_csph(c_sphere%numnod_sf+1:num)
        else
          t(1:c_sphere%numnod_sf)                                       &
     &          = c_sphere%theta_csph(1:c_sphere%numnod_sf)
          t(c_sphere%numnod_sf+1:num)                                   &
     &          = c_sphere%theta_csph(c_sphere%numnod_sf+1:num)
        end if
!
        do inod0 = 1, c_sphere%numnod_sf
          ratio1(inod0) = (dble(nr_adj-k)                               &
     &                   + dble(k-1)*r_nod(1)/c_sphere%r_csph(inod0))   &
     &                 * r_nod(k)   / ( dble(nr_adj-1)*r_nod(1) )
          ratio2(inod0) = (dble(nr_adj-k-1)                             &
     &                   + dble(k)*r_nod(1)/c_sphere%r_csph(inod0))     &
     &                 * r_nod(k+1) / ( dble(nr_adj-1)*r_nod(1) )
          ratio(inod0) = (ratio1(inod0) + ratio2(inod0)) * half
!
          r(inod0) = c_sphere%r_csph(inod0) * ratio(inod0)
        end do
!
        do iedge0 = 1, c_sphere%numedge_sf
          inod0 = iedge0 + c_sphere%numnod_sf
!
          ratio(inod0) = (dble(nr_adj-k-1)                              &
     &                    + dble(k)*r_nod(1)/c_sphere%r_csph(inod0))    &
     &                  * r_nod(k+1) / ( dble(nr_adj-1)*r_nod(1) )
!
          r(inod0) = c_sphere%r_csph(inod0) * ratio(inod0)
        end do
!
        call position_2_xyz(num, r(1), t(1), c_sphere%phi_csph(1),      &
     &      x(1), y(1), z(1))
!
        do inod0 = 1, c_sphere%numnod_sf + c_sphere%numedge_sf
          inod = inod + 1
          write (ifile,'(i15,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), z(inod0)
        end do
      end do
!
      end subroutine adjust_to_shell_quad
!
!   --------------------------------------------------------------------
!
      end module cal_shell_position
