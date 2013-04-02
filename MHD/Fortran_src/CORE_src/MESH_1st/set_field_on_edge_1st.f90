!
!      module set_field_on_edge_1st
!
      module set_field_on_edge_1st
!
!      Written by H. Matsui on Feb., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine set_scalar_on_edge_quad_1(i_field)
!      subroutine set_vector_on_edge_quad_1(i_field)
!      subroutine set_sym_tensor_on_edge_quad_1(i_field)
!
!      subroutine cal_scalar_field_on_edge_1(i_field)
!      subroutine cal_vector_field_on_edge_1(i_field)
!      subroutine cal_tensor_field_on_edge_1(i_field)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_on_edge_quad_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call set_scalar_on_edge_quad(numnod, numedge,                     &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      end subroutine set_scalar_on_edge_quad_1
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_on_edge_quad_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call set_vector_on_edge_quad(numnod, numedge,                     &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      end subroutine set_vector_on_edge_quad_1
!
! -----------------------------------------------------------------------
!
      subroutine set_sym_tensor_on_edge_quad_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call set_sym_tensor_on_edge_quad(numnod, numedge,                 &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      end subroutine set_sym_tensor_on_edge_quad_1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_scalar_field_on_edge_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_edge_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call cal_scalar_field_on_edge(numnod, numedge,                    &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      end subroutine cal_scalar_field_on_edge_1
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_field_on_edge_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_edge_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call cal_vector_field_on_edge(numnod, numedge,                    &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      end subroutine cal_vector_field_on_edge_1
!
!-----------------------------------------------------------------------
!
      subroutine cal_tensor_field_on_edge_1(i_field)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_edge_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call cal_tensor_field_on_edge(numnod, numedge,                    &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      end subroutine cal_tensor_field_on_edge_1
!
!-----------------------------------------------------------------------
!
      end module set_field_on_edge_1st
