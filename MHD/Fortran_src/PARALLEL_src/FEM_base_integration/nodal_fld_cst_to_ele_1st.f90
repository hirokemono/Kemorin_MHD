!nodal_fld_cst_to_ele_1st.f90
!      module nodal_fld_cst_to_ele_1st
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine scalar_const_2_each_ele(k2, scalar, coef, scalar_e)
!      subroutine vector_const_2_each_ele(k2, vector, coef, vector_e)
!      subroutine tensor_const_2_each_ele(k2, tensor, coef, tensor_e)
!
!      subroutine scalar_cst_phys_2_each_ele(k2, i_fld, coef, scalar_e)
!      subroutine vector_cst_phys_2_each_ele(k2, i_fld, coef, vector_e)
!      subroutine tensor_cst_phys_2_each_ele(k2, i_fld, coef, tensor_e)
!
!      subroutine tensor_cst_phys_2_vec_each_ele(k2, i_flux, nd,        &
!     &          coef, vector_e)
!      subroutine as_tsr_cst_phys_2_vec_each_ele(k2, i_flux, nd,        &
!     &          coef, vector_e)
!
      module nodal_fld_cst_to_ele_1st
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data
!
      use nodal_cst_fld_each_element
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine scalar_const_2_each_ele(k2, scalar, coef, scalar_e)
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: scalar(node1%numnod)
      real (kind=kreal), intent(inout) :: scalar_e(ele1%numele)
!
!
      call const_scalar_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, ione, ione, scalar(1), coef, scalar_e)
!
      end subroutine scalar_const_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine vector_const_2_each_ele(k2, vector, coef, vector_e)
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vector(node1%numnod,3)
      real (kind=kreal), intent(inout) :: vector_e(ele1%numele,3)
!
!
      call const_vector_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, ione, ithree, vector, coef, vector_e)
!
      end subroutine vector_const_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_const_2_each_ele(k2, tensor, coef, tensor_e)
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: tensor(node1%numnod,6)
      real (kind=kreal), intent(inout) :: tensor_e(ele1%numele,6)
!
!
      call const_tensor_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, ione, isix, tensor, coef, tensor_e)
!
      end subroutine tensor_const_2_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_cst_phys_2_each_ele(k2, i_fld, coef, scalar_e)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(ele1%numele)
!
!
      call const_scalar_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, i_fld, nod_fld1%ntot_phys, nod_fld1%d_fld,                &
     &    coef, scalar_e)
!
      end subroutine scalar_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine vector_cst_phys_2_each_ele(k2, i_fld, coef, vector_e)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele1%numele,3)
!
!
      call const_vector_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, i_fld, nod_fld1%ntot_phys, nod_fld1%d_fld,                &
     &    coef, vector_e)
!
      end subroutine vector_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_phys_2_each_ele(k2, i_fld, coef, tensor_e)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: tensor_e(ele1%numele,6)
!
!
      call const_tensor_2_each_ele(node1%numnod, ele1%numele,           &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, i_fld, nod_fld1%ntot_phys, nod_fld1%d_fld,                &
     &    coef, tensor_e)
!
      end subroutine tensor_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_phys_2_vec_each_ele(k2, i_flux, nd,         &
     &          coef, vector_e)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: k2, i_flux, nd
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele1%numele,3)
!
!
      call const_tensor_2_vec_each_ele(node1%numnod, ele1%numele,       &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, i_flux, nd, nod_fld1%ntot_phys, nod_fld1%d_fld,           &
     &    coef, vector_e)
!
      end subroutine tensor_cst_phys_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine as_tsr_cst_phys_2_vec_each_ele(k2, i_flux, nd,         &
     &          coef, vector_e)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: k2, i_flux, nd
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele1%numele,3)
!
!
      call const_as_tsr_2_vec_each_ele(node1%numnod, ele1%numele,       &
     &    ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,        &
     &    k2, i_flux, nd, nod_fld1%ntot_phys, nod_fld1%d_fld,           &
     &    coef, vector_e)
!
      end subroutine as_tsr_cst_phys_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_cst_to_ele_1st
