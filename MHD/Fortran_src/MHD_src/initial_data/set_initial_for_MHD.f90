!
!      module set_initial_for_MHD
!
!      Written by H. Matsui
!
!      subroutine set_initial_vect_p(isig)
!      subroutine set_initial_magne(isig)
!      subroutine set_initial_kinematic
!
      module set_initial_for_MHD
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_physical_property
      use m_schmidt_polynomial
      use m_spherical_harmonics
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
      subroutine set_initial_vect_p(isig)
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      use dynamobench_r_func_sph_vecp
!
      integer ( kind = kint), intent(in) :: isig
      integer ( kind = kint) :: inod, ifl, j_rst
!
!
      j_rst = ( isig-mod(isig,1000) )/1000
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
        do inod = 1, numnod
!
         dth = colatitude(inod)
         dph = longitude(inod)
!
         call dschmidt
         call spheric
!
         call radial_function_sph_vecp(radius(inod), ifl, j_rst,        &
     &       depth_high_t, depth_low_t)
!
!         d_nod(inod,iphys%i_mag_p) = 0.0d0
!         do j = 1, jmax_tri_sph
!          d_nod(inod,iphys%i_mag_p) = d_nod(inod,iphys%i_mag_p)        &
!     &                               + mp(j)*s(j,0)
!         end do
!
         call cvt_spectr_2_field(radius(inod), dth)
!
         call cvt_one_vector_2_cart(b_cart, b_pole, dth, dph)
         d_nod(inod,iphys%i_vecp  ) = b_cart(1)
         d_nod(inod,iphys%i_vecp+1) = b_cart(2)
         d_nod(inod,iphys%i_vecp+2) = b_cart(3)
!
         if ( radius(inod) .le. 1.0d-20 ) then
           d_nod(inod,iphys%i_magne  ) = 0.0d0
           d_nod(inod,iphys%i_magne+1) = 0.0d0
           d_nod(inod,iphys%i_magne+2) = 0.0d0
           d_nod(inod,iphys%i_mag_p)   = 0.0d0
         end if
!
      end do
!
      call deallocate_schmidt_polynomial
      call deallocate_spherical_harmonics
!
      end subroutine set_initial_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne(isig)
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      use radial_func_sph_magne
!
      integer ( kind = kint), intent(in) :: isig
      integer ( kind = kint) :: inod, j, ifl, j_rst
!
!
      j_rst = ( isig-mod(isig,1000) )/1000
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
        do inod = 1, numnod
!
         dth = colatitude(inod)
         dph = longitude(inod)
!
         call dschmidt
         call spheric
!
         call radial_function_sph(radius(inod), ifl, j_rst,             &
     &       depth_high_t, depth_low_t)
!
         d_nod(inod,iphys%i_mag_p) = 0.0d0
         do j = 1, jmax_tri_sph
          d_nod(inod,iphys%i_mag_p) = d_nod(inod,iphys%i_mag_p)         &
     &                               + mp(j)*s(j,0)
         end do
!
         call cvt_spectr_2_field(radius(inod), dth)
!
         call cvt_one_vector_2_cart(b_cart, b_pole, dth, dph)
         d_nod(inod,iphys%i_magne  ) = b_cart(1)
         d_nod(inod,iphys%i_magne+1) = b_cart(2)
         d_nod(inod,iphys%i_magne+2) = b_cart(3)
!
         if ( radius(inod)  .le. 1.0d-20 ) then
           d_nod(inod,iphys%i_magne  ) = 0.0d0
           d_nod(inod,iphys%i_magne+1) = 0.0d0
           d_nod(inod,iphys%i_magne+2) = 2.83351986832173d-1
           d_nod(inod,iphys%i_mag_p)   = 0.0d0
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
      subroutine set_initial_kinematic
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
!
      use dynamobench_r_func_sph_velo
!
      integer (kind = kint) :: inod, inum
!
!
      do inod = 1, numnod
       d_nod(inod,iphys%i_press) = 0.0d0
      end do
!
      call allocate_schmidt_polynomial
      call allocate_spherical_harmonics(nth)
!
      call idx28
!
        do inum = 1, numnod_fluid
         inod = inod_fluid(inum)
!
         dth = colatitude(inod)
         dph = longitude(inod)
!
         call dschmidt
         call spheric
!
         call radial_function_sph_velo( radius(inod) )
!
         call cvt_spectr_2_field(radius(inod), dth)
!
         call cvt_one_vector_2_cart(v_cart, v_pole, dth, dph)
         call cvt_one_vector_2_cart(b_cart, b_pole, dth, dph)
!
         d_nod(inod,iphys%i_velo  ) = v_cart(1)
         d_nod(inod,iphys%i_velo+1) = v_cart(2)
         d_nod(inod,iphys%i_velo+2) = v_cart(3)
         d_nod(inod,iphys%i_magne  ) = b_cart(1)
         d_nod(inod,iphys%i_magne+1) = b_cart(2)
         d_nod(inod,iphys%i_magne+2) = b_cart(3)
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
      if ( r .gt. 0.0d0 ) then
        if ( sin(theta) .eq. 0.0d0 ) then
          call cvtp(r)
        else 
          call cvt(r)
        end if
      end if
!
      end subroutine cvt_spectr_2_field
!
!-----------------------------------------------------------------------
!
      end module set_initial_for_MHD
