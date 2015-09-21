!
!      module set_initial_for_MHD
!
!      Written by H. Matsui
!
!!      subroutine set_initial_vect_p(isig, node, ncomp_nod,            &
!!     &          i_vecp, i_magne, i_mag_p, d_nod)
!!      subroutine set_initial_magne(isig, node, ncomp_nod,             &
!!     &          i_magne, i_mag_p, d_nod)
!!      subroutine set_initial_kinematic(numnod, nnod_fl, inod_fluid,   &
!!     &          ncomp_nod, i_velo, i_press, i_magne, d_nod)
!
      module set_initial_for_MHD
!
      use m_precision
      use m_constants
!
      use m_control_parameter
      use m_schmidt_polynomial
      use m_spherical_harmonics
!
      use t_geometry_data
!
      use spherical_harmonics
      use cvt_vector_2_cartecian
!
      implicit none
!
      private :: cvt_spectr_2_field
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_vect_p(isig, node, ncomp_nod,              &
     &          i_vecp, i_magne, i_mag_p, d_nod)
!
      use m_physical_property
      use spherical_harmonics
!
      use dynamobench_r_func_sph_vecp
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: isig
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_vecp, i_magne, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ifl, j_rst, l_rst, m_rst
!
!
      j_rst = ( isig-mod(isig,ikilo) )/ikilo
      call get_dgree_order_by_full_j(j_rst, l_rst, m_rst)
!
      ifl = 1
      if ( abs(depth_low_t/depth_high_t - 0.35) .lt. 1.0d-4) ifl = 2
!
      call allocate_schmidt_polynomial
      call allocate_spherical_harmonics(nth)
      call set_radial_coefficients
!
      call idx28
!
        do inod = 1, node%numnod
         call dschmidt(node%theta(inod))
         call spheric(node%phi(inod))
!
         call radial_function_sph_vecp                                  &
     &      (node%rr(inod), ifl, j_rst, l_rst,                          &
     &       depth_high_t, depth_low_t)
!
!         d_nod(inod,i_mag_p) = 0.0d0
!         do j = 1, jmax_tri_sph
!          d_nod(inod,i_mag_p) = d_nod(inod,i_mag_p) + mp(j)*s(j,0)
!         end do
!
         call cvt_spectr_2_field(node%rr(inod), node%theta(inod))
!
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
         d_nod(inod,i_vecp  ) = b_cart(1)
         d_nod(inod,i_vecp+1) = b_cart(2)
         d_nod(inod,i_vecp+2) = b_cart(3)
!
         if ( node%rr(inod) .le. 1.0d-20 ) then
           d_nod(inod,i_magne  ) = 0.0d0
           d_nod(inod,i_magne+1) = 0.0d0
           d_nod(inod,i_magne+2) = 0.0d0
           d_nod(inod,i_mag_p)   = 0.0d0
         end if
      end do
!
      call deallocate_schmidt_polynomial
      call deallocate_spherical_harmonics
!
      end subroutine set_initial_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne(isig, node, ncomp_nod,               &
     &          i_magne, i_mag_p, d_nod)
!
      use m_physical_property
!
      use radial_func_sph_magne
      use spherical_harmonics
!
      type(node_data), intent(in) :: node
      integer ( kind = kint), intent(in) :: isig
      integer(kind = kint), intent(in) :: ncomp_nod, i_magne, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer ( kind = kint) :: inod, j, ifl, j_rst, l_rst, m_rst
!
!
      j_rst = ( isig-mod(isig,ikilo) )/ikilo
      call get_dgree_order_by_full_j(j_rst, l_rst, m_rst)
!
      ifl = 1
      if ( abs(depth_low_t/depth_high_t - 0.35) .lt. 1.0d-4) ifl = 2
!
      call allocate_schmidt_polynomial
      call allocate_spherical_harmonics(nth)
      call set_radial_coefficients
!
      call idx28
!
        do inod = 1, node%numnod
         call dschmidt(node%theta(inod))
         call spheric(node%phi(inod))
!
         call radial_function_sph(node%rr(inod), ifl, j_rst, l_rst,    &
     &       depth_high_t, depth_low_t)
!
         d_nod(inod,i_mag_p) = 0.0d0
         do j = 1, jmax_tri_sph
          d_nod(inod,i_mag_p) = d_nod(inod,i_mag_p) + mp(j)*s(j,0)
         end do
!
         call cvt_spectr_2_field(node%rr(inod), node%theta(inod))
!
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
         d_nod(inod,i_magne  ) = b_cart(1)
         d_nod(inod,i_magne+1) = b_cart(2)
         d_nod(inod,i_magne+2) = b_cart(3)
!
         if ( node%rr(inod)  .le. 1.0d-20 ) then
           d_nod(inod,i_magne  ) = 0.0d0
           d_nod(inod,i_magne+1) = 0.0d0
           d_nod(inod,i_magne+2) = 2.83351986832173d-1
           d_nod(inod,i_mag_p)   = 0.0d0
         end if
!
      end do
!
      call deallocate_schmidt_polynomial
      call deallocate_spherical_harmonics
!
      end subroutine set_initial_magne
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_kinematic(node, nnod_fl, inod_fluid,       &
     &          ncomp_nod, i_velo, i_press, i_magne, d_nod)
!
      use dynamobench_r_func_sph_velo
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: nnod_fl, ncomp_nod
      integer(kind = kint), intent(in) :: inod_fluid(nnod_fl)
      integer(kind = kint), intent(in) :: i_velo, i_press, i_magne
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, inum
!
!
      do inod = 1, node%numnod
       d_nod(inod,i_press) = 0.0d0
      end do
!
      call allocate_schmidt_polynomial
      call allocate_spherical_harmonics(nth)
!
      call idx28
!
        do inum = 1, nnod_fl
         inod = inod_fluid(inum)
!
         call dschmidt(node%theta(inod))
         call spheric(node%phi(inod))
!
         call radial_function_sph_velo( node%rr(inod) )
!
         call cvt_spectr_2_field(node%rr(inod), node%theta(inod))
!
         call cvt_one_vector_2_cart                                     &
     &      (v_cart, v_pole, node%theta(inod), node%phi(inod))
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
!
         d_nod(inod,i_velo  ) = v_cart(1)
         d_nod(inod,i_velo+1) = v_cart(2)
         d_nod(inod,i_velo+2) = v_cart(3)
         d_nod(inod,i_magne  ) = b_cart(1)
         d_nod(inod,i_magne+1) = b_cart(2)
         d_nod(inod,i_magne+2) = b_cart(3)
!
      end do
!
      call deallocate_schmidt_polynomial
      call deallocate_spherical_harmonics
!
      end subroutine set_initial_kinematic
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cvt_spectr_2_field(r, theta)
!
      use sph_spectr_2_vector
!
      real(kind = kreal), intent(in) :: r, theta
!
      if ( r .le. 0.0d0 ) return
      if ( sin(theta) .eq. 0.0d0 ) then
        call cvtp(r, theta)
      else 
        call cvt(r, theta)
      end if
!
      end subroutine cvt_spectr_2_field
!
!-----------------------------------------------------------------------
!
      end module set_initial_for_MHD
