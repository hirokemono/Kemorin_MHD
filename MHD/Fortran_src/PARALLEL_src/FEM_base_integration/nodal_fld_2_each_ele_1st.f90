!nodal_fld_2_each_ele_1st.f90
!      module nodal_fld_2_each_ele_1st
!
!      Written by H. Matsui on July, 2005
!
!      subroutine position_2_each_element(k2, xe, radius_e)
!      subroutine scalar_2_each_element(k2, scalar, scalar_e)
!      subroutine vector_2_each_element(k2, vector, vect_e)
!
!      subroutine scalar_phys_2_each_element(k2, i_field, scalar_e)
!      subroutine vector_phys_2_each_element(k2, i_field, vect_e)
!      subroutine tensor_phys_2_each_element(k2, i_field, tensor_e)
!
!      subroutine tensor_2_vec_each_ele(k2, nd, i_flux, vect_e)
!      subroutine as_tensor_2_vec_each_ele(k2, nd, i_flux, vect_e)
!
      module nodal_fld_2_each_ele_1st
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine position_2_each_element(k2, xe, radius_e)
!
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(inout) :: xe(ele1%numele,3)
      real (kind=kreal), intent(inout) :: radius_e(ele1%numele)
!
!
      call position_to_local_ele(node1%numnod, ele1%numele,             &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp, k2,    &
     &    node1%xx, node1%rr, xe, radius_e)
!
      end subroutine position_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine scalar_2_each_element(k2, scalar, scalar_e)
!
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: scalar(node1%numnod)
      real (kind=kreal), intent(inout) :: scalar_e(ele1%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, ione, ione, scalar(1),       &
     &    scalar_e )
!
      end subroutine scalar_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine vector_2_each_element(k2, vector, vect_e)
!
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: vector(node1%numnod,3)
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, ione, ithree,                &
     &    vector, vect_e)
!
      end subroutine vector_2_each_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_phys_2_each_element(k2, i_field, scalar_e)
!
      use m_node_phys_data
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: scalar_e(ele1%numele)
!
!
      call scalar_to_local_ele                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, i_field,                     &
     &    nod_fld1%ntot_phys, d_nod, scalar_e)
!
      end subroutine scalar_phys_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine vector_phys_2_each_element(k2, i_field, vect_e)
!
      use m_node_phys_data
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
!
      call vector_to_local_ele                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, i_field,                     &
     &    nod_fld1%ntot_phys, d_nod, vect_e)
!
      end subroutine vector_phys_2_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_element(k2, i_field, tensor_e)
!
      use m_node_phys_data
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(inout) :: tensor_e(ele1%numele,6)
!
!
      call tensor_to_local_ele                                          &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, i_field,                     &
     &    nod_fld1%ntot_phys, d_nod, tensor_e)
!
      end subroutine tensor_phys_2_each_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_2_vec_each_ele(k2, nd, i_flux, vect_e)
!
      use m_phys_constants
      use m_node_phys_data
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2, nd, i_flux
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
      call tensor_to_local_ele_v                                        &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, i_flux, nd, l_sim_t,         &
     &    nod_fld1%ntot_phys, d_nod, vect_e)
!
      end subroutine tensor_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine as_tensor_2_vec_each_ele(k2, nd, i_flux, vect_e)
!
      use m_phys_constants
      use m_node_phys_data
      use set_nodal_2_each_element
!
      integer(kind = kint), intent(in) :: k2, nd, i_flux
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
!
      call as_tensor_to_local_ele_v                                     &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,          &
     &    np_smp, ele1%istack_ele_smp, k2, i_flux, nd, l_asim_t,        &
     &    nod_fld1%ntot_phys, d_nod, vect_e)
!
      end subroutine as_tensor_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_2_each_ele_1st
