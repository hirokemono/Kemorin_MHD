!sgs_terms_to_each_ele.f90
!      module sgs_terms_to_each_ele
!
!      Written by H. Matsui on July, 2005
!
!!      subroutine SGS_vector_each_ele(node, ele, nod_fld,              &
!!     &          k2, i_vect, i_scalar, i_sgs, sgs_e, flux_e)
!!      subroutine SGS_tensor_each_ele(node, ele, nod_fld,              &
!!     &          k2, i_vect, i_sgs, sgs_e, flux_e)
!!      subroutine SGS_induction_each_ele(node, ele, nod_fld,           &
!!     &          k2, i_b, i_v, i_sgs, sgs_e, flux_e)
!!
!!      subroutine SGS_flux_vector_each_ele(node, ele, nod_fld,         &
!!     &          k2, nd, i_vect, i_field, i_sgs, sgs_e, flux_e)
!!      subroutine SGS_induct_vector_ele(node, ele, nod_fld,            &
!!     &          k2, nd, i_b, i_v, i_sgs, sgs_e, flux_e)
!!
!!
!!      subroutine SGS_coef_vector_each_ele(node, ele, nod_fld,         &
!!     &          k2, i_vect, i_scalar, i_sgs, ak_e, sgs_e, flux_e)
!!      subroutine SGS_coef_tensor_each_ele(node, ele, nod_fld,         &
!!     &          k2, i_vect, i_sgs, ak_e, sgs_e, flux_e)
!!      subroutine SGS_coef_induct_each_ele(node, ele, nod_fld,         &
!!     &          k2, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!!
!!      subroutine SGS_coef_flux_vect_each_ele(node, ele, nod_fld,      &
!!     &          k2, nd, i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!!      subroutine SGS_coef_induct_vec_ele(node, ele, nod_fld,          &
!!     &          k2, nd, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!!
!!
!!      subroutine SGS_const_vector_each_ele(node, ele, nod_fld,        &
!!     &          k2, i_vect, i_scalar, i_sgs, coef, sgs_e, flux_e)
!!      subroutine SGS_const_tensor_each_ele(node, ele, nod_fld,        &
!!     &          k2, i_vect, i_sgs, coef, sgs_e, flux_e)
!!      subroutine SGS_const_induct_each_ele(node, ele, nod_fld,        &
!!     &          k2, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!!
!!      subroutine SGS_flux_cst_each_ele_vector(node, ele, nod_fld,     &
!!     &          k2, nd, i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!!      subroutine SGS_induct_vec_cst_ele(node, ele, nod_fld,           &
!!     &          k2, nd, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!!
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: k2
!!        integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
!!        real (kind=kreal), intent(in) :: ak_e(ele%numele)
!!        real (kind=kreal), intent(in) :: coef
!!
!!        real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
!!        real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
      module sgs_terms_to_each_ele
!
      use m_precision
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_each_ele(node, ele, nod_fld,                &
     &          k2, i_vect, i_scalar, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_vector_2_each_element                                    &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_vect, i_scalar, i_sgs,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, sgs_e, flux_e)
!
      end subroutine SGS_vector_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_each_ele(node, ele, nod_fld,                &
     &          k2, i_vect, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,6)
!
!
      call SGS_tensor_2_each_element                                    &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_vect, i_sgs,                &
     &    nod_fld%ntot_phys, nod_fld%d_fld, sgs_e, flux_e)
!
      end subroutine SGS_tensor_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induction_each_ele(node, ele, nod_fld,             &
     &          k2, i_b, i_v, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_to_each_element                                   &
     &    (node%numnod, ele%numele, ele%nnod_4_ele,                     &
     &     ele%ie, np_smp, ele%istack_ele_smp, k2, i_b, i_v, i_sgs,     &
     &     nod_fld%ntot_phys, nod_fld%d_fld, sgs_e, flux_e)
!
      end subroutine SGS_induction_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_vector_each_ele(node, ele, nod_fld,           &
     &          k2, nd, i_vect, i_field, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_flux_2_each_element_vec                                  &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_vect, i_field, i_sgs,   &
     &    nod_fld%ntot_phys, nod_fld%d_fld, sgs_e, flux_e)
!
      end subroutine SGS_flux_vector_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vector_ele(node, ele, nod_fld,              &
     &          k2, nd, i_b, i_v, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_vec_2_each_element                                &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_b, i_v, i_sgs,          &
     &    nod_fld%ntot_phys, nod_fld%d_fld, sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vector_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_coef_vector_each_ele(node, ele, nod_fld,           &
     &          k2, i_vect, i_scalar, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_vector_coef_each_ele                                     &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_vect, i_scalar, i_sgs,      &
     &    nod_fld%ntot_phys, nod_fld%d_fld, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_coef_vector_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_coef_tensor_each_ele(node, ele, nod_fld,           &
     &          k2, i_vect, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,6)
!
!
      call SGS_tensor_coef_each_ele                                     &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_vect, i_sgs,                &
     &    nod_fld%ntot_phys, nod_fld%d_fld, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_coef_tensor_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_coef_induct_each_ele(node, ele, nod_fld,           &
     &          k2, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_coef_each_ele                                     &
     &    (node%numnod, ele%numele, ele%nnod_4_ele,                     &
     &     ele%ie, np_smp, ele%istack_ele_smp, k2, i_b, i_v, i_sgs,     &
     &     nod_fld%ntot_phys, nod_fld%d_fld, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_coef_induct_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_coef_flux_vect_each_ele(node, ele, nod_fld,        &
     &          k2, nd, i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_flux_coef_each_ele_vec                                   &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_vect, i_field, i_sgs,   &
     &    nod_fld%ntot_phys, nod_fld%d_fld, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_coef_flux_vect_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_coef_induct_vec_ele(node, ele, nod_fld,            &
     &          k2, nd, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_vec_coef_each_ele                                 &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_b, i_v, i_sgs,          &
     &    nod_fld%ntot_phys, nod_fld%d_fld, ak_e, sgs_e, flux_e)
!
!
      end subroutine SGS_coef_induct_vec_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_const_vector_each_ele(node, ele, nod_fld,          &
     &          k2, i_vect, i_scalar, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_vector_cst_each_ele                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_vect, i_scalar,             &
     &    i_sgs, nod_fld%ntot_phys, nod_fld%d_fld, coef, sgs_e, flux_e)
!
      end subroutine SGS_const_vector_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_const_tensor_each_ele(node, ele, nod_fld,          &
     &          k2, i_tensor, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_tensor, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,6)
!
!
      call SGS_tensor_cst_each_ele                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_tensor, i_sgs,              &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, sgs_e, flux_e)
!
      end subroutine SGS_const_tensor_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_const_induct_each_ele(node, ele, nod_fld,          &
     &          k2, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_cst_each_ele                                      &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, i_b, i_v, i_sgs,              &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, sgs_e, flux_e)
!
      end subroutine SGS_const_induct_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_cst_each_ele_vector(node, ele, nod_fld,       &
     &          k2, nd, i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_flux_cst_each_ele_vec                                    &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_vect, i_field, i_sgs,   &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, sgs_e, flux_e)
!
      end subroutine SGS_flux_cst_each_ele_vector
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_cst_ele(node, ele, nod_fld,             &
     &          k2, nd, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(ele%numele,3)
!
!
      call SGS_induct_vec_cst_each_ele                                  &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    np_smp, ele%istack_ele_smp, k2, nd, i_b, i_v, i_sgs,          &
     &    nod_fld%ntot_phys, nod_fld%d_fld, coef, sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_cst_ele
!
!  ---------------------------------------------------------------------
!
      end module sgs_terms_to_each_ele
