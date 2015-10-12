!nodal_fld_2_each_element.f90
!      module nodal_fld_2_each_element
!
!      Written by H. Matsui on July, 2005
!
!      subroutine position_2_each_element(node, ele, k2, xe, radius_e)
!      subroutine scalar_2_each_element                                 &
!     &         (node, ele, k2, scalar, scalar_e)
!      subroutine vector_2_each_element(node, ele, k2, vector, vect_e)
!
!      subroutine scalar_phys_2_each_element(node, ele, nod_fld,        &
!     &          k2, i_field, scalar_e)
!      subroutine vector_phys_2_each_element(node, ele, nod_fld,        &
!     &          k2, i_field, vect_e)
!      subroutine tensor_phys_2_each_element(node, ele, nod_fld,        &
!     &          k2, i_field, tensor_e)
!
!      subroutine tensor_2_vec_each_ele(node, ele, nod_fld,             &
!     &          k2, nd, i_flux, vect_e)
!      subroutine as_tensor_2_vec_each_ele(node, ele, nod_fld,          &
!     &          k2, nd, i_flux, vect_e)
!        type(node_data), intent(in) ::    node
!        type(element_data), intent(in) :: ele
!        type(phys_data), intent(in) :: nod_fld
!
      module nodal_fld_2_each_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine position_2_each_element(node, ele, k2, xe, radius_e)
!
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(inout) :: xe(ele%numele,3)
      real (kind=kreal), intent(inout) :: radius_e(ele%numele)
!
!
      call position_to_local_ele(node%numnod, ele%numele,               &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp, k2,       &
     &    node%xx, node%rr, xe, radius_e)
!
      end subroutine position_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine scalar_2_each_element                                  &
     &         (node, ele, k2, scalar, scalar_e)
!
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: scalar(node%numnod)
      real (kind=kreal), intent(inout) :: scalar_e(ele%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, ione, ione, scalar(1),        &
     &    scalar_e)
!
      end subroutine scalar_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine vector_2_each_element(node, ele, k2, vector, vect_e)
!
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: vector(node%numnod,3)
      real (kind=kreal), intent(inout) :: vect_e(ele%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, ione, ithree,                 &
     &    vector, vect_e)
!
      end subroutine vector_2_each_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_phys_2_each_element(node, ele, nod_fld,         &
     &          k2, i_field, scalar_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: scalar_e(ele%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_field,                      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, scalar_e)
!
      end subroutine scalar_phys_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine vector_phys_2_each_element(node, ele, nod_fld,         &
     &          k2, i_field, vect_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: vect_e(ele%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_field,                      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine vector_phys_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_element(node, ele, nod_fld,         &
     &          k2, i_field, tensor_e)
!
      use t_phys_data
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: tensor_e(ele%numele,6)
!
!
      call tensor_to_local_ele                                          &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_field,                      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, tensor_e)
!
      end subroutine tensor_phys_2_each_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_2_vec_each_ele(node, ele, nod_fld,              &
     &          k2, nd, i_flux, vect_e)
!
      use m_phys_constants
      use t_phys_data
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, nd, i_flux
      real (kind=kreal), intent(inout) :: vect_e(ele%numele,3)
!
      call tensor_to_local_ele_v                                        &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_flux, nd, l_sim_t,          &
     &    nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine tensor_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine as_tensor_2_vec_each_ele(node, ele, nod_fld,           &
     &          k2, nd, i_flux, vect_e)
!
      use m_phys_constants
      use t_phys_data
      use set_nodal_2_each_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, nd, i_flux
      real (kind=kreal), intent(inout) :: vect_e(ele%numele,3)
!
!
      call as_tensor_to_local_ele_v                                     &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_flux, nd, l_asim_t,         &
     &    nod_fld%ntot_phys, nod_fld%d_fld, vect_e)
!
      end subroutine as_tensor_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_2_each_element
