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
      use t_comm_table
!
      implicit  none
!
!
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
!
!>     Node communication table for second mesh
      type(communication_table), save :: comm_2nd
!
!>     element communication table for second mesh
      type(communication_table), save :: ele_comm_2nd
!>     Surface communication table for second mesh
      type(communication_table), save :: surf_comm_2nd
!>     Edge communication table for second mesh
      type(communication_table), save :: edge_comm_2nd
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_num_nod_4_each_elements_2
!
      use m_geometry_constants
!
!
      if (ele_2nd%nnod_4_ele .eq. num_t_quad) then
        surf_2nd%nnod_4_surf = num_quad_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      else if (ele_2nd%nnod_4_ele .eq. num_t_linear) then
        surf_2nd%nnod_4_surf = num_linear_sf
        edge_2nd%nnod_4_edge = num_linear_edge
      else if (ele_2nd%nnod_4_ele .eq. num_t_lag) then
        surf_2nd%nnod_4_surf = num_lag_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      end if
!
      end subroutine set_num_nod_4_each_elements_2
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
