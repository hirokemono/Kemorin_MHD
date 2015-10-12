!nodal_fld_cst_to_element.f90
!      module nodal_fld_cst_to_element
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine scalar_const_2_each_ele                               &
!     &         (node, ele, k2, scalar, coef, scalar_e)
!      subroutine vector_const_2_each_ele                               &
!     &         (node, ele, k2, vector, coef, vector_e)
!      subroutine tensor_const_2_each_ele                               &
!     &         (node, ele, k2, tensor, coef, tensor_e)
!
!      subroutine scalar_cst_phys_2_each_ele(node, ele, nod_fld,        &
!     &          k2, i_fld, coef, scalar_e)
!      subroutine vector_cst_phys_2_each_ele(node, ele, nod_fld,        &
!     &          k2, i_fld, coef, vector_e)
!      subroutine tensor_cst_phys_2_each_ele(node, ele, nod_fld,        &
!     &          k2, i_fld, coef, tensor_e)
!
!      subroutine tensor_cst_phys_2_vec_each_ele(k2, i_flux, nd,        &
!     &          coef, vector_e)
!      subroutine as_tsr_cst_phys_2_vec_each_ele(node, ele, nod_fld,    &
!     &           k2, i_flux, nd, coef, vector_e)
!        type(node_data), intent(in) ::    node
!        type(element_data), intent(in) :: ele
!        type(phys_data), intent(in) :: nod_fld
!
      module nodal_fld_cst_to_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_geometry_data
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
      subroutine scalar_const_2_each_ele                                &
     &         (node, ele, k2, scalar, coef, scalar_e)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: scalar(node%numnod)
      real (kind=kreal), intent(inout) :: scalar_e(ele%numele)
!
!
      call const_scalar_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, ione, ione, scalar(1), coef, scalar_e)
!
      end subroutine scalar_const_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine vector_const_2_each_ele                                &
     &         (node, ele, k2, vector, coef, vector_e)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vector(node%numnod,3)
      real (kind=kreal), intent(inout) :: vector_e(ele%numele,3)
!
!
      call const_vector_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, ione, ithree, vector, coef, vector_e)
!
      end subroutine vector_const_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_const_2_each_ele                                &
     &         (node, ele, k2, tensor, coef, tensor_e)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: tensor(node%numnod,6)
      real (kind=kreal), intent(inout) :: tensor_e(ele%numele,6)
!
!
      call const_tensor_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, ione, isix, tensor, coef, tensor_e)
!
      end subroutine tensor_const_2_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_cst_phys_2_each_ele(node, ele, nod_fld,         &
     &          k2, i_fld, coef, scalar_e)
!
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(ele%numele)
!
!
      call const_scalar_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, i_fld, nod_fld%ntot_phys, nod_fld%d_fld,                  &
     &    coef, scalar_e)
!
      end subroutine scalar_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine vector_cst_phys_2_each_ele(node, ele, nod_fld,         &
     &          k2, i_fld, coef, vector_e)
!
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele%numele,3)
!
!
      call const_vector_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, i_fld, nod_fld%ntot_phys, nod_fld%d_fld,                  &
     &    coef, vector_e)
!
      end subroutine vector_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_phys_2_each_ele(node, ele, nod_fld,         &
     &          k2, i_fld, coef, tensor_e)
!
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_fld
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: tensor_e(ele%numele,6)
!
!
      call const_tensor_2_each_ele(node%numnod, ele%numele,             &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, i_fld, nod_fld%ntot_phys, nod_fld%d_fld,                  &
     &    coef, tensor_e)
!
      end subroutine tensor_cst_phys_2_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_cst_phys_2_vec_each_ele(node, ele, nod_fld,     &
     &          k2, i_flux, nd, coef, vector_e)
!
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_flux, nd
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele%numele,3)
!
!
      call const_tensor_2_vec_each_ele(node%numnod, ele%numele,         &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, i_flux, nd, nod_fld%ntot_phys, nod_fld%d_fld,             &
     &    coef, vector_e)
!
      end subroutine tensor_cst_phys_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine as_tsr_cst_phys_2_vec_each_ele(node, ele, nod_fld,     &
     &          k2, i_flux, nd, coef, vector_e)
!
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: k2, i_flux, nd
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(ele%numele,3)
!
!
      call const_as_tsr_2_vec_each_ele(node%numnod, ele%numele,         &
     &    ele%nnod_4_ele, ele%ie, np_smp, ele%istack_ele_smp,           &
     &    k2, i_flux, nd, nod_fld%ntot_phys, nod_fld%d_fld,             &
     &    coef, vector_e)
!
      end subroutine as_tsr_cst_phys_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      end module nodal_fld_cst_to_element
