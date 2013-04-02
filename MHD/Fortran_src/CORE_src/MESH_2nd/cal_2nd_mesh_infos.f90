!
!      module cal_2nd_mesh_infos
!
      module cal_2nd_mesh_infos
!
!      Written by H. Matsui on July, 2006
!
!      subroutine set_2nd_spherical_position
!      subroutine set_2nd_center_of_element
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_2nd_spherical_position
!
      use  m_2nd_geometry_param
      use  m_2nd_geometry_data
!
      use coordinate_converter
!
       call position_2_sph( nnod_2nd, xx_2nd, radius_2nd, theta_2nd,    &
     &     phi_2nd, a_radius_2nd, s_cyl_2nd, a_s_cyl_2nd)
!
      end subroutine set_2nd_spherical_position
!
! ----------------------------------------------------------------------
!
      subroutine set_2nd_center_of_element
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      use m_2nd_element_geometry_data
!
      use set_element_position
      use coordinate_converter
!
!
      if (nnod_4_ele_2nd .eq. num_t_quad) then
        call set_quad_ele_position(nnod_2nd, nele_2nd, ie_2nd,          &
     &      xx_2nd, x_ele_2nd)
      else if (nnod_4_ele_2nd .eq. num_t_linear) then
        call set_linear_ele_position(nnod_2nd, nele_2nd, ie_2nd,        &
     &      xx_2nd, x_ele_2nd)
      else if (nnod_4_ele_2nd .eq. num_t_lag) then
        call set_lag_ele_position(nnod_2nd, nele_2nd, ie_2nd,           &
     &      xx_2nd, x_ele_2nd)
      end if
!
      call position_2_sph( nele_2nd, x_ele_2nd, r_ele_2nd,              &
     &    theta_ele_2nd, phi_ele_2nd, ar_ele_2nd, s_ele_2nd,            &
     &    as_ele_2nd)
!
      end subroutine set_2nd_center_of_element
!
! ----------------------------------------------------------------------
!
      end module cal_2nd_mesh_infos
