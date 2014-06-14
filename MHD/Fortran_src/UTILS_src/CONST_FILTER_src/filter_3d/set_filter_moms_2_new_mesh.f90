!set_filter_moms_2_new_mesh.f90
!      module set_filter_moms_2_new_mesh
!
      module set_filter_moms_2_new_mesh
!
!     Written by H. Matsui on Nov., 2008
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: max_gl_ele_newdomain
      integer(kind = kint), allocatable :: iele_local_2nd(:)
!
      integer(kind = kint), parameter :: n_vector = 3
!
      private :: iele_local_2nd, n_vector
!
!      subroutine allocate_iele_local_newfilter
!      subroutine deallocate_iele_local_newfilter
!
!      subroutine set_iele_table_4_newfilter
!
!      subroutine set_new_elength_ele
!      subroutine set_new_filter_moms_ele
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_iele_local_newfilter
!
      allocate(iele_local_2nd(max_gl_ele_newdomain))
      iele_local_2nd(1:max_gl_ele_newdomain) = 0
!
      end subroutine allocate_iele_local_newfilter
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iele_local_newfilter
!
      deallocate(iele_local_2nd)
!
      end subroutine deallocate_iele_local_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_iele_table_4_newfilter
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: iele, iele_gl
!
!
      iele_local_2nd(1:max_gl_ele_newdomain) = 0
      do iele = 1, ele_2nd%numele
        iele_gl = ele_2nd%iele_global(iele)
        iele_local_2nd(iele_gl) = iele
      end do
!
      end subroutine set_iele_table_4_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_new_elength_ele
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_filter_ele_length
      use m_filter_elength
!
      integer(kind = kint) :: iele, iele_gl, iele_2nd, nd
!
!
      do iele = 1, numele
        iele_gl = globalelmid(iele)
        if (ie(iele,1) .le. internal_nod_2nd                            &
     &      .and. iele_gl .le. max_gl_ele_newdomain                     &
     &      .and. iele_local_2nd(iele_gl) .gt. 0) then
!
          iele_2nd = iele_local_2nd(iele_gl)
!
          elen_dx2_ele_2nd(iele_2nd) = elen_dx2_ele(iele)
          elen_dy2_ele_2nd(iele_2nd) = elen_dy2_ele(iele)
          elen_dz2_ele_2nd(iele_2nd) = elen_dz2_ele(iele)
          elen_dxdy_ele_2nd(iele_2nd) = elen_dxdy_ele(iele)
          elen_dydz_ele_2nd(iele_2nd) = elen_dydz_ele(iele)
          elen_dzdx_ele_2nd(iele_2nd) = elen_dzdx_ele(iele)
!
          do nd = 1, n_vector
            elen_dx2_ele_dx_2nd(iele_2nd,nd)                            &
     &         = elen_dx2_ele_dx(iele,nd)
            elen_dy2_ele_dx_2nd(iele_2nd,nd)                            &
     &         = elen_dy2_ele_dx(iele,nd)
            elen_dz2_ele_dx_2nd(iele_2nd,nd)                            &
     &         = elen_dz2_ele_dx(iele,nd)
            elen_dxdy_ele_dx_2nd(iele_2nd,nd)                           &
     &         = elen_dxdy_ele_dx(iele,nd)
            elen_dydz_ele_dx_2nd(iele_2nd,nd)                           &
     &         = elen_dydz_ele_dx(iele,nd)
            elen_dzdx_ele_dx_2nd(iele_2nd,nd)                           &
     &         = elen_dzdx_ele_dx(iele,nd)
!
            elen_dx2_ele_dx2_2nd(iele_2nd,nd)                           &
     &         = elen_dx2_ele_dx2(iele,nd)
            elen_dy2_ele_dx2_2nd(iele_2nd,nd)                           &
     &         = elen_dy2_ele_dx2(iele,nd)
            elen_dz2_ele_dx2_2nd(iele_2nd,nd)                           &
     &         = elen_dz2_ele_dx2(iele,nd)
            elen_dxdy_ele_dx2_2nd(iele_2nd,nd)                          &
     &         = elen_dxdy_ele_dx2(iele,nd)
            elen_dydz_ele_dx2_2nd(iele_2nd,nd)                          &
     &         = elen_dydz_ele_dx2(iele,nd)
            elen_dzdx_ele_dx2_2nd(iele_2nd,nd)                          &
     &         = elen_dzdx_ele_dx2(iele,nd)
          end do
!
        end if
      end do
!
      end subroutine set_new_elength_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_new_filter_moms_ele
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_filter_moments
      use m_filter_moments
!
      integer(kind = kint) :: iele, iele_gl, iele_2nd, nd, ifil
!
!
      do iele = 1, numele
        iele_gl = globalelmid(iele)
        if (ie(iele,1) .le. internal_nod_2nd                            &
     &      .and. iele_gl .le. max_gl_ele_newdomain                     &
     &      .and. iele_local_2nd(iele_gl) .gt. 0) then
!
          iele_2nd = iele_local_2nd(iele_gl)
!
          if (iele_2nd .gt. size(filter_x2_ele_2nd,1) ) then
            write(*,*) 'aho-', iele, iele_gl, iele_2nd
          end if
          if (iele_2nd .le. 0 ) then
            write(*,*) 'baka-', iele, iele_gl, iele_2nd
          end if
!
          do ifil = 1, num_filter_moms
            filter_x2_ele_2nd(iele_2nd,ifil) = filter_x2_ele(iele,ifil)
            filter_y2_ele_2nd(iele_2nd,ifil) = filter_y2_ele(iele,ifil)
            filter_z2_ele_2nd(iele_2nd,ifil) = filter_z2_ele(iele,ifil)
            filter_xy_ele_2nd(iele_2nd,ifil) = filter_xy_ele(iele,ifil)
            filter_yz_ele_2nd(iele_2nd,ifil) = filter_yz_ele(iele,ifil)
            filter_zx_ele_2nd(iele_2nd,ifil) = filter_zx_ele(iele,ifil)
            filter_x_ele_2nd(iele_2nd,ifil) =  filter_x_ele(iele,ifil)
            filter_y_ele_2nd(iele_2nd,ifil) =  filter_y_ele(iele,ifil)
            filter_z_ele_2nd(iele_2nd,ifil) =  filter_z_ele(iele,ifil)
!
            do nd = 1, n_vector
              filter_x2_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_x2_ele_dx(iele,nd,ifil)
              filter_y2_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_y2_ele_dx(iele,nd,ifil)
              filter_z2_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_z2_ele_dx(iele,nd,ifil)
              filter_xy_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_xy_ele_dx(iele,nd,ifil)
              filter_yz_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_yz_ele_dx(iele,nd,ifil)
              filter_zx_ele_dx_2nd(iele_2nd,nd,ifil)                    &
     &          = filter_zx_ele_dx(iele,nd,ifil)
              filter_x_ele_dx_2nd(iele_2nd,nd,ifil)                     &
     &          =  filter_x_ele_dx(iele,nd,ifil)
              filter_y_ele_dx_2nd(iele_2nd,nd,ifil)                     &
     &          =  filter_y_ele_dx(iele,nd,ifil)
              filter_z_ele_dx_2nd(iele_2nd,nd,ifil)                     &
     &          =  filter_z_ele_dx(iele,nd,ifil)
!
              filter_x2_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_x2_ele_dx2(iele,nd,ifil)
              filter_y2_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_y2_ele_dx2(iele,nd,ifil)
              filter_z2_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_z2_ele_dx2(iele,nd,ifil)
              filter_xy_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_xy_ele_dx2(iele,nd,ifil)
              filter_yz_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_yz_ele_dx2(iele,nd,ifil)
              filter_zx_ele_dx2_2nd(iele_2nd,nd,ifil)                   &
     &          = filter_zx_ele_dx2(iele,nd,ifil)
              filter_x_ele_dx2_2nd(iele_2nd,nd,ifil)                    &
     &          =  filter_x_ele_dx2(iele,nd,ifil)
              filter_y_ele_dx2_2nd(iele_2nd,nd,ifil)                    &
     &          =  filter_y_ele_dx2(iele,nd,ifil)
              filter_z_ele_dx2_2nd(iele_2nd,nd,ifil)                    &
     &          =  filter_z_ele_dx2(iele,nd,ifil)
            end do
          end do
!
        end if
      end do
!
      end subroutine set_new_filter_moms_ele
!
!   --------------------------------------------------------------------
!
      end module set_filter_moms_2_new_mesh
