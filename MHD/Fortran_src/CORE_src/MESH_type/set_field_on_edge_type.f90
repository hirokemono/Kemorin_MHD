!
!      module set_field_on_edge_type
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine set_scalar_on_edge_quad_type(node, edge,              &
!     &          i_field, nod_fld)
!      subroutine set_vector_on_edge_quad_type(node, edge,              &
!     &          i_field, nod_fld)
!      subroutine set_sym_tensor_on_edge_quad_type(node, edge,          &
!     &          i_field, nod_fld)
!
!      subroutine cal_scalar_field_on_edge_type(node, edge,             &
!     &          i_field, nod_fld, edge_fld)
!      subroutine cal_vector_field_on_edge_type(node, edge,             &
!     &          i_field, nod_fld, edge_fld)
!      subroutine cal_tensor_field_on_edge_type(node, edge,             &
!     &           i_field, nod_fld, edge_fld)
!
      module set_field_on_edge_type
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_on_edge_quad_type(node, edge,               &
     &          i_field, nod_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_scalar_on_edge_quad(node%numnod, edge%numedge,           &
     &    edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,         &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_scalar_on_edge_quad_type
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_on_edge_quad_type(node, edge,               &
     &          i_field, nod_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_vector_on_edge_quad(node%numnod, edge%numedge,           &
     &    edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,         &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_vector_on_edge_quad_type
!
! -----------------------------------------------------------------------
!
      subroutine set_sym_tensor_on_edge_quad_type(node, edge,           &
     &          i_field, nod_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_sym_tensor_on_edge_quad(node%numnod, edge%numedge,       &
     &    edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,         &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_sym_tensor_on_edge_quad_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_scalar_field_on_edge_type(node, edge,              &
     &          i_field, nod_fld, edge_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: nod_fld
      type(phys_data), intent(inout) :: edge_fld
!
!
      call cal_scalar_field_on_edge(node%numnod, edge%numedge,          &
     &    edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,         &
     &    nod_fld%ntot_phys, edge_fld%ntot_phys, i_field,               &
     &    nod_fld%d_fld, edge_fld%d_fld)
!
      end subroutine cal_scalar_field_on_edge_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_field_on_edge_type(node, edge,              &
     &          i_field, nod_fld, edge_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: nod_fld
      type(phys_data), intent(inout) :: edge_fld
!
!
      call cal_vector_field_on_edge(node%numnod, edge%numedge,          &
     &     edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,        &
     &     nod_fld%ntot_phys, edge_fld%ntot_phys, i_field,              &
     &     nod_fld%d_fld, edge_fld%d_fld)
!
      end subroutine cal_vector_field_on_edge_type
!
!-----------------------------------------------------------------------
!
      subroutine cal_tensor_field_on_edge_type(node, edge,              &
     &           i_field, nod_fld, edge_fld)
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
      use set_field_on_edge
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(phys_data), intent(in) :: nod_fld
      type(phys_data), intent(inout) :: edge_fld
!
!
      call cal_tensor_field_on_edge(node%numnod, edge%numedge,          &
     &    edge%nnod_4_edge, edge%istack_edge_smp, edge%ie_edge,         &
     &    nod_fld%ntot_phys, edge_fld%ntot_phys, i_field,               &
     &    nod_fld%d_fld, edge_fld%d_fld)
!
      end subroutine cal_tensor_field_on_edge_type
!
!-----------------------------------------------------------------------
!
      end module set_field_on_edge_type
