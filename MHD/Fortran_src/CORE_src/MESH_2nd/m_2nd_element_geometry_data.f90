!
!     module   m_2nd_element_geometry_data
!
!     written by H. Matsui on Aug., 2006
!
!       subroutine allocate_2nd_ele_geometry
!       subroutine deallocate_2nd_ele_geometry
!       subroutine unlink_2nd_ele_geometry
!
      module   m_2nd_element_geometry_data
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal), pointer  :: x_ele_2nd(:,:)
!   position of centre of element
      real(kind=kreal), pointer  :: r_ele_2nd(:)
!   distance from the centre of element
      real(kind=kreal), pointer  :: ar_ele_2nd(:)
!   1/r_ele
      real(kind=kreal), pointer  :: phi_ele_2nd(:)
!   longitude of node
      real(kind=kreal), pointer  :: theta_ele_2nd(:)
!   colatitude of node
      real(kind=kreal), pointer  :: s_ele_2nd(:)
!   cylindorical radius of node
      real(kind=kreal), pointer  :: as_ele_2nd(:)
!   1 / s_ele
!
      real (kind=kreal), pointer :: volume_ele_2nd(:)
      real (kind=kreal), pointer :: a_vol_ele_2nd(:)
!   volume of each element
!
!
      real(kind=kreal) :: volume_2nd
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine allocate_2nd_ele_geometry
!
       use m_2nd_geometry_param
!
        allocate(x_ele_2nd(nele_2nd,3))
        allocate(r_ele_2nd(nele_2nd))
        allocate(ar_ele_2nd(nele_2nd))
        allocate(phi_ele_2nd(nele_2nd))
        allocate(theta_ele_2nd(nele_2nd))
        allocate(s_ele_2nd(nele_2nd))
        allocate(as_ele_2nd(nele_2nd))
!
        allocate( volume_ele_2nd (nele_2nd))
        allocate( a_vol_ele_2nd (nele_2nd))
!
       x_ele_2nd = 0.0d0
!
       r_ele_2nd = 0.0d0
       ar_ele_2nd = 0.0d0
       phi_ele_2nd = 0.0d0
       theta_ele_2nd = 0.0d0
       s_ele_2nd = 0.0d0
       as_ele_2nd = 0.0d0
!
       volume_ele_2nd = 0.0d0
       a_vol_ele_2nd = 0.0d0
!
       end subroutine allocate_2nd_ele_geometry
!
! ------------------------------------------------------
!
       subroutine deallocate_2nd_ele_geometry
!
        deallocate(x_ele_2nd)
        deallocate(r_ele_2nd)
        deallocate(ar_ele_2nd)
        deallocate(phi_ele_2nd)
        deallocate(theta_ele_2nd)
        deallocate(s_ele_2nd)
        deallocate(as_ele_2nd)
!
        deallocate(volume_ele_2nd)
        deallocate(a_vol_ele_2nd)
!
       end subroutine deallocate_2nd_ele_geometry
!
! ------------------------------------------------------
!
       subroutine unlink_2nd_ele_geometry
!
        nullify(x_ele_2nd)
        nullify(r_ele_2nd)
        nullify(ar_ele_2nd)
        nullify(phi_ele_2nd)
        nullify(theta_ele_2nd)
        nullify(s_ele_2nd)
        nullify(as_ele_2nd)
!
        nullify(volume_ele_2nd)
        nullify(a_vol_ele_2nd)
!
       end subroutine unlink_2nd_ele_geometry
!
! ------------------------------------------------------
!
      end module   m_2nd_element_geometry_data
