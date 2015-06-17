!copy_filter_moms_from_2nd.f90
!      module copy_filter_moms_from_2nd
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine copy_elength_ele_from_2nd(new_node, new_ele)
!      subroutine copy_filter_moments_from_2nd(new_node, new_ele)
!
      module copy_filter_moms_from_2nd
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: n_vector = 3
      private :: n_vector
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_elength_ele_from_2nd(new_node, new_ele)
!
      use t_geometry_data
      use m_2nd_filter_ele_length
      use m_filter_elength
      use copy_filter_moment_type
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
!
      nnod_filter_mom = new_node%numnod
      nele_filter_mom = new_ele%numele
      call allocate_ele_length
      call copy_elength_type(nele_filter_mom, elen_2, elen1%moms)
      call copy_elen_diffs_type(nele_filter_mom, diff1_2, elen1%diff)
      call copy_elen_diffs_type(nele_filter_mom, diff2_2, elen1%diff2)
      call deallocate_2nd_ele_length
!
      end subroutine copy_elength_ele_from_2nd
!
!   --------------------------------------------------------------------
!
      subroutine copy_filter_moments_from_2nd(new_node, new_ele)
!
      use t_geometry_data
      use m_2nd_filter_moments
      use m_filter_moments
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
!
      integer(kind = kint) :: iele, ifil, nd
!
!
      nnod_fmom = new_node%numnod
      call allocate_filter_moms_ele(new_ele%numele)
!
      do ifil = 1, num_filter_moms
        do iele = 1, new_ele%numele
          filter_x2_ele(iele,ifil) = filter_x2_ele_2nd(iele,ifil)
          filter_y2_ele(iele,ifil) = filter_y2_ele_2nd(iele,ifil)
          filter_z2_ele(iele,ifil) = filter_z2_ele_2nd(iele,ifil)
          filter_xy_ele(iele,ifil) = filter_xy_ele_2nd(iele,ifil)
          filter_yz_ele(iele,ifil) = filter_yz_ele_2nd(iele,ifil)
          filter_zx_ele(iele,ifil) = filter_zx_ele_2nd(iele,ifil)
          filter_x_ele(iele,ifil) =  filter_x_ele_2nd(iele,ifil)
          filter_y_ele(iele,ifil) =  filter_y_ele_2nd(iele,ifil)
          filter_z_ele(iele,ifil) =  filter_z_ele_2nd(iele,ifil)
        end do
      end do
!
      do ifil = 1, num_filter_moms
        do nd = 1, n_vector
          do iele = 1, new_ele%numele
            filter_x2_ele_dx(iele,nd,ifil)                              &
     &          = filter_x2_ele_dx_2nd(iele,nd,ifil)
            filter_y2_ele_dx(iele,nd,ifil)                              &
     &          = filter_y2_ele_dx_2nd(iele,nd,ifil)
            filter_z2_ele_dx(iele,nd,ifil)                              &
     &          = filter_z2_ele_dx_2nd(iele,nd,ifil)
            filter_xy_ele_dx(iele,nd,ifil)                              &
     &          = filter_xy_ele_dx_2nd(iele,nd,ifil)
            filter_yz_ele_dx(iele,nd,ifil)                              &
     &          = filter_yz_ele_dx_2nd(iele,nd,ifil)
            filter_zx_ele_dx(iele,nd,ifil)                              &
     &          = filter_zx_ele_dx_2nd(iele,nd,ifil)
            filter_x_ele_dx(iele,nd,ifil)                               &
     &          =  filter_x_ele_dx_2nd(iele,nd,ifil)
            filter_y_ele_dx(iele,nd,ifil)                               &
     &          =  filter_y_ele_dx_2nd(iele,nd,ifil)
            filter_z_ele_dx(iele,nd,ifil)                               &
     &          =  filter_z_ele_dx_2nd(iele,nd,ifil)
!
            filter_x2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_x2_ele_dx2_2nd(iele,nd,ifil)
            filter_y2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_y2_ele_dx2_2nd(iele,nd,ifil)
            filter_z2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_z2_ele_dx2_2nd(iele,nd,ifil)
            filter_xy_ele_dx2(iele,nd,ifil)                             &
     &          = filter_xy_ele_dx2_2nd(iele,nd,ifil)
            filter_yz_ele_dx2(iele,nd,ifil)                             &
     &          = filter_yz_ele_dx2_2nd(iele,nd,ifil)
            filter_zx_ele_dx2(iele,nd,ifil)                             &
     &          = filter_zx_ele_dx2_2nd(iele,nd,ifil)
            filter_x_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_x_ele_dx2_2nd(iele,nd,ifil)
            filter_y_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_y_ele_dx2_2nd(iele,nd,ifil)
            filter_z_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_z_ele_dx2_2nd(iele,nd,ifil)
          end do
        end do
      end do
!
      call deallocate_2nd_filter_moms_ele
!
      end subroutine copy_filter_moments_from_2nd
!
!   --------------------------------------------------------------------
!
      end module copy_filter_moms_from_2nd
