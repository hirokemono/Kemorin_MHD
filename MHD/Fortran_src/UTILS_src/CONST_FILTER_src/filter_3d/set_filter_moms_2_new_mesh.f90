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
      integer(kind = kint_gl) :: max_gl_ele_newdomain
      integer(kind = kint), allocatable :: iele_local_2nd(:)
!
      integer(kind = kint), parameter :: n_vector = 3
!
      private :: iele_local_2nd, n_vector
!
!      subroutine allocate_iele_local_newfilter
!      subroutine deallocate_iele_local_newfilter
!
!      subroutine set_iele_table_4_newfilter(new_ele)
!
!      subroutine set_new_elength_ele(new_node)
!      subroutine set_new_filter_moms_ele(new_node)
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
      subroutine set_iele_table_4_newfilter(new_ele)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: iele
      integer(kind = kint_gl) :: iele_gl
!
!
      iele_local_2nd(1:max_gl_ele_newdomain) = 0
      do iele = 1, new_ele%numele
        iele_gl = new_ele%iele_global(iele)
        iele_local_2nd(iele_gl) = iele
      end do
!
      end subroutine set_iele_table_4_newfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_new_elength_ele(new_node)
!
      use t_geometry_data
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_filter_ele_length
      use m_filter_elength
!
      type(node_data), intent(in) :: new_node
!
      integer(kind = kint) :: iele, iele_2nd, nd
      integer(kind = kint_gl) :: iele_gl
!
!
      do iele = 1, numele
        iele_gl = iele_global(iele)
        if (ie(iele,1) .le. new_node%internal_node                      &
     &      .and. int(iele_gl) .le. max_gl_ele_newdomain                &
     &      .and. iele_local_2nd(iele_gl) .gt. 0) then
!
          iele_2nd = iele_local_2nd(iele_gl)
!
          elen_2%f_x2(iele_2nd) = FEM1_elen%elen_ele%moms%f_x2(iele)
          elen_2%f_y2(iele_2nd) = FEM1_elen%elen_ele%moms%f_y2(iele)
          elen_2%f_z2(iele_2nd) = FEM1_elen%elen_ele%moms%f_z2(iele)
          elen_2%f_xy(iele_2nd) = FEM1_elen%elen_ele%moms%f_xy(iele)
          elen_2%f_yz(iele_2nd) = FEM1_elen%elen_ele%moms%f_yz(iele)
          elen_2%f_zx(iele_2nd) = FEM1_elen%elen_ele%moms%f_zx(iele)
!
          do nd = 1, n_vector
            diff1_2%df_x2(iele_2nd,nd)                            &
     &         = FEM1_elen%elen_ele%diff%df_x2(iele,nd)
            diff1_2%df_y2(iele_2nd,nd)                            &
     &         = FEM1_elen%elen_ele%diff%df_y2(iele,nd)
            diff1_2%df_z2(iele_2nd,nd)                            &
     &         = FEM1_elen%elen_ele%diff%df_z2(iele,nd)
            diff1_2%df_xy(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff%df_xy(iele,nd)
            diff1_2%df_yz(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff%df_yz(iele,nd)
            diff1_2%df_zx(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff%df_zx(iele,nd)
!
            diff2_2%df_x2(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff2%df_x2(iele,nd)
            diff2_2%df_y2(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff2%df_y2(iele,nd)
            diff2_2%df_z2(iele_2nd,nd)                           &
     &         = FEM1_elen%elen_ele%diff2%df_z2(iele,nd)
            diff2_2%df_xy(iele_2nd,nd)                          &
     &         = FEM1_elen%elen_ele%diff2%df_xy(iele,nd)
            diff2_2%df_yz(iele_2nd,nd)                          &
     &         = FEM1_elen%elen_ele%diff2%df_yz(iele,nd)
            diff2_2%df_zx(iele_2nd,nd)                          &
     &         = FEM1_elen%elen_ele%diff2%df_zx(iele,nd)
          end do
!
        end if
      end do
!
      end subroutine set_new_elength_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_new_filter_moms_ele(new_node)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
      use m_2nd_filter_moments
      use m_filter_moments
!
      type(node_data), intent(in) :: new_node
!
      integer(kind = kint) :: iele, iele_2nd, nd, ifil
      integer(kind = kint_gl) :: iele_gl
!
!
      do iele = 1, numele
        iele_gl = iele_global(iele)
        if (ie(iele,1) .le. new_node%internal_node                      &
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
