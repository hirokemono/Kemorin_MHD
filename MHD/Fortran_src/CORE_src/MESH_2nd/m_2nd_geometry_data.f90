!
!     module   m_2nd_geometry_data
!
!     written by H. Matsui on Aug., 2006
!
!      subroutine allocate_2nd_geometry_data
!
!       subroutine deallocate_2nd_node_position
!       subroutine deallocate_2nd_element_connect
!
!      subroutine allocate_2nd_node_position
!      subroutine allocate_2nd_element_connect
!      subroutine allocate_2nd_element_data
!      subroutine allocate_2nd_surface_connect
!
!       subroutine unlink_2nd_geometry_data
!       subroutine unlink_2nd_node_position
!       subroutine unlink_2nd_element_connect
!       subroutine unlink_2nd_element_data
!
!      subroutine check_smp_size_2nd_surf_edge
!
      module   m_2nd_geometry_data
!
      use m_precision
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!
!>   position of nodes (i:direction, j:node ID)
      real(kind=kreal)  , pointer  :: xx_2nd(:,:)
!>   element connectivity ie(i:element ID,j:element index)
      integer(kind=kint), pointer  :: ie_2nd(:,:)
!>   element type defined by the first element
      integer(kind=kint) ::  first_ele_type_2nd
!
!
      integer(kind=kint), pointer ::  elmtyp_2nd(:)
!     element type id   (where i:element id)
      integer(kind=kint), pointer ::  nodelm_2nd(:)
!     element type id   (where i:element id)
      integer(kind=kint), pointer ::  globalnodid_2nd(:)
!     global node    id (where i:node id)
      integer(kind=kint), pointer ::  globalelmid_2nd(:)
!     global element id (where i:element id)
!
      real(kind=kreal), pointer :: radius_2nd(:)
!   distance from the centre
      real(kind=kreal), pointer :: a_radius_2nd(:)
!   1/radius
      real(kind=kreal), pointer :: theta_2nd(:)
!   longitude of node
      real(kind=kreal), pointer :: phi_2nd(:)
!   colatitude of node
      real(kind=kreal), pointer :: s_cyl_2nd(:)
!   cylindorical radius of node
      real(kind=kreal), pointer :: a_s_cyl_2nd(:)
!   1 / a_s_cylinder
!
      integer(kind = kint), pointer :: interior_ele_2nd(:)
!   flag for interior element
      real(kind=kreal)  , pointer  :: e_multi_2nd(:)
!   parameter for overlap
!
!>      surface information for 2nd mesh
      type(surface_data), save :: surf_2nd
!>      Strucure for second edge data
      type(edge_data), save :: edge_2nd
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_geometry_data
!
      use m_2nd_geometry_param
!
!
      call allocate_2nd_node_position
!
      call allocate_2nd_element_connect
!
      call allocate_2nd_element_data
!
      end subroutine allocate_2nd_geometry_data
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_node_position
!
      use m_2nd_geometry_param
!
!
      allocate(globalnodid_2nd(nnod_2nd))
      allocate(xx_2nd(nnod_2nd,3))
!
      allocate(radius_2nd(nnod_2nd))
      allocate(a_radius_2nd(nnod_2nd))
      allocate(s_cyl_2nd(nnod_2nd))
      allocate(a_s_cyl_2nd(nnod_2nd))
      allocate(theta_2nd(nnod_2nd))
      allocate(phi_2nd(nnod_2nd))
!
      globalnodid_2nd = 0
      xx_2nd = 0.0d00

      radius_2nd = 0.0d00
      a_radius_2nd = 0.0d00
      s_cyl_2nd = 0.0d00
      a_s_cyl_2nd = 0.0d00
      theta_2nd = 0.0d00
      phi_2nd = 0.0d00
!
      end subroutine allocate_2nd_node_position
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_element_connect
!
      use m_2nd_geometry_param
!
!
      allocate(globalelmid_2nd(nele_2nd))
      allocate(elmtyp_2nd(nele_2nd))
      allocate(nodelm_2nd(nele_2nd))
!
      allocate(ie_2nd(nele_2nd,nnod_4_ele_2nd))
!
      globalelmid_2nd = 0
      elmtyp_2nd = 0
      nodelm_2nd = 0
!
      ie_2nd = 0
!
      end subroutine allocate_2nd_element_connect
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_element_data
!
      use m_2nd_geometry_param
!
      allocate ( interior_ele_2nd(nele_2nd) )
      allocate ( e_multi_2nd(nele_2nd) )
!
      interior_ele_2nd = 1
      e_multi_2nd = 1.0d0
!
      end subroutine allocate_2nd_element_data
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_surface_connect
!
      use m_2nd_geometry_param
!
!
      call allocate_surface_connect_type(surf_2nd, nele_2nd)
!
      end subroutine allocate_2nd_surface_connect
!
! ------------------------------------------------------
!------------------------------------------------------------------
!
       subroutine deallocate_2nd_geometry_data
!
!
       call deallocate_2nd_node_position
       call deallocate_2nd_element_connect
!
       call deallocate_2nd_element_data
!
       end subroutine deallocate_2nd_geometry_data
!
! ------------------------------------------------------
!
       subroutine deallocate_2nd_node_position
!
!
        deallocate(globalnodid_2nd)
        deallocate(xx_2nd)
!
        deallocate(radius_2nd)
        deallocate(a_radius_2nd)
        deallocate(s_cyl_2nd)
        deallocate(a_s_cyl_2nd)
        deallocate(theta_2nd)
        deallocate(phi_2nd)
!
       end subroutine deallocate_2nd_node_position
!
! ------------------------------------------------------
!
       subroutine deallocate_2nd_element_connect
!
!
        deallocate(globalelmid_2nd)
        deallocate(elmtyp_2nd)
        deallocate(nodelm_2nd)
!
        deallocate(ie_2nd)
!
       end subroutine deallocate_2nd_element_connect
!
! ------------------------------------------------------
!
       subroutine deallocate_2nd_element_data
!
        deallocate ( interior_ele_2nd )
        deallocate ( e_multi_2nd )
!
       end subroutine deallocate_2nd_element_data
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_surface_connect
!
      call deallocate_surface_connect_type(surf_2nd)
!
      end subroutine deallocate_2nd_surface_connect
!
! ------------------------------------------------------
! ------------------------------------------------------
!
       subroutine unlink_2nd_geometry_data
!
!
       call unlink_2nd_node_position
       call unlink_2nd_element_connect
!
       call unlink_2nd_element_data
!
       end subroutine unlink_2nd_geometry_data
!
! ------------------------------------------------------
!
       subroutine unlink_2nd_node_position
!
!
        nullify(globalnodid_2nd)
        nullify(xx_2nd)
!
        nullify(radius_2nd)
        nullify(a_radius_2nd)
        nullify(s_cyl_2nd)
        nullify(a_s_cyl_2nd)
        nullify(theta_2nd)
        nullify(phi_2nd)
!
       end subroutine unlink_2nd_node_position
!
! ------------------------------------------------------
!
       subroutine unlink_2nd_element_connect
!
!
        nullify(globalelmid_2nd)
        nullify(elmtyp_2nd)
        nullify(nodelm_2nd)
!
        nullify(ie_2nd)
!
       end subroutine unlink_2nd_element_connect
!
! ------------------------------------------------------
!
       subroutine unlink_2nd_element_data
!
        nullify ( interior_ele_2nd )
        nullify ( e_multi_2nd )
!
       end subroutine unlink_2nd_element_data
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine check_smp_size_2nd_surf_edge
!
      use m_2nd_geometry_param
!
      write(*,*) 'surf_2nd%istack_surf_smp ', surf_2nd%istack_surf_smp
      write(*,*) 'edge_2nd%istack_edge_smp ', edge_2nd%istack_edge_smp
!
      end subroutine check_smp_size_2nd_surf_edge
!
!-----------------------------------------------------------------------
!
      end module m_2nd_geometry_data
