!
!     module   m_2nd_geometry_data
!
!     written by H. Matsui on Aug., 2006
!
!      subroutine allocate_2nd_geometry_data
!      subroutine allocate_2nd_geomet_param_smp
!
!      subroutine deallocate_2nd_geomet_param_smp
!       subroutine deallocate_2nd_node_position
!
!      subroutine allocate_2nd_node_position
!
!       subroutine unlink_2nd_geometry_data
!       subroutine unlink_2nd_node_position
!
!      subroutine check_smp_size_2nd(my_rank)
!      subroutine check_smp_size_2nd_surf_edge
!
      module   m_2nd_geometry_data
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!
!>   position of nodes (i:direction, j:node ID)
      real(kind=kreal)  , pointer  :: xx_2nd(:,:)
!
!
      integer(kind=kint), pointer ::  globalnodid_2nd(:)
!     global node    id (where i:node id)
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
!   parameter for overlap
!
!
!
!>      element information for 2nd mesh
      type(element_data), save :: ele_2nd
!ele_2nd%ie
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
      call allocate_ele_connect_type(ele_2nd)
      call allocate_overlaped_ele_type(ele_2nd)
!
      end subroutine allocate_2nd_geometry_data
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_geomet_param_smp
!
      use m_2nd_geometry_param
      use m_machine_parameter
!
      call allocate_ele_param_smp_type(ele_2nd)
      allocate( inod_smp_stack_2nd(0:np_smp))
      allocate( inter_smp_stack_2nd(0:np_smp))
!
      inod_smp_stack_2nd = 0
      inter_smp_stack_2nd = 0
!
      end subroutine allocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
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
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_geomet_param_smp
!
      use m_2nd_geometry_param
!
      call deallocate_ele_param_smp_type(ele_2nd)
      deallocate( inod_smp_stack_2nd)
      deallocate( inter_smp_stack_2nd)
!
      end subroutine deallocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_2nd_geometry_data
!
!
      call deallocate_2nd_node_position
      call deallocate_ele_connect_type(ele_2nd)
!
      call deallocate_overlaped_ele_type(ele_2nd)
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
! ------------------------------------------------------
!
       subroutine unlink_2nd_geometry_data
!
!
       call unlink_2nd_node_position
       call unlink_ele_connect_type(ele_2nd)
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_2nd(my_rank)
!
      use m_2nd_geometry_param
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_smp_stack_2nd ', inod_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_smp_stack_2nd ', inter_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'ele_2nd%istack_ele_smp ', ele_2nd%istack_ele_smp
!
      end subroutine check_smp_size_2nd
!
!-----------------------------------------------------------------------
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
