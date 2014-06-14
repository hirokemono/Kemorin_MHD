!
!     module   m_2nd_geometry_data
!
!     written by H. Matsui on Aug., 2006
!
!      subroutine check_smp_size_2nd(my_rank)
!      subroutine check_smp_size_2nd_surf_edge
!
      module m_2nd_geometry_data
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!
!node_2nd%numnod
!>      node information for 2nd mesh
      type(node_data), save :: node_2nd
!>      element information for 2nd mesh
      type(element_data), save :: ele_2nd
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
      subroutine check_smp_size_2nd(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'node_2nd%istack_nod_smp ', node_2nd%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'node_2nd%istack_nod_smp ', node_2nd%istack_nod_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'ele_2nd%istack_ele_smp ', ele_2nd%istack_ele_smp
!
      end subroutine check_smp_size_2nd
!
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_2nd_surf_edge
!
      write(*,*) 'surf_2nd%istack_surf_smp ', surf_2nd%istack_surf_smp
      write(*,*) 'edge_2nd%istack_edge_smp ', edge_2nd%istack_edge_smp
!
      end subroutine check_smp_size_2nd_surf_edge
!
!-----------------------------------------------------------------------
!
      end module m_2nd_geometry_data
