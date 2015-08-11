!sgs_terms_to_each_ele_1st.f90
!      module sgs_terms_to_each_ele_1st
!
!      Written by H. Matsui on July, 2005
!
!
!      subroutine SGS_vector_each_ele_1st(k2, i_vect, i_scalar,         &
!     &          i_sgs, sgs_e, flux_e)
!      subroutine SGS_tensor_each_ele_1st(k2, i_vect, i_sgs,            &
!     &          sgs_e, flux_e)
!      subroutine SGS_induct_each_ele_1st(k2, i_b, i_v, i_sgs,          &
!     &          sgs_e, flux_e)
!
!      subroutine SGS_flux_ele_vec_1st(k2, nd,                          &
!     &          i_vect, i_field, i_sgs, sgs_e, flux_e)
!      subroutine SGS_induct_vec_ele_1st(k2, nd,                        &
!     &          i_b, i_v, i_sgs, sgs_e, flux_e)
!
!
!      subroutine SGS_vector_coef_each_ele_1st(k2, i_vect, i_scalar,    &
!     &          i_sgs, ak_e, sgs_e, flux_e)
!      subroutine SGS_tensor_coef_each_ele_1st(k2, i_vect, i_sgs,       &
!     &          ak_e, sgs_e, flux_e)
!      subroutine SGS_induct_coef_each_ele_1st(k2, i_b, i_v, i_sgs,     &
!     &          ak_e, sgs_e, flux_e)
!
!      subroutine SGS_flux_coef_ele_vec_1st(k2, nd,                     &
!     &          i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!      subroutine SGS_induct_vec_coef_ele_1st(k2, nd,                   &
!     &          i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
!
!      subroutine SGS_vector_cst_each_ele_1st(k2, i_vect, i_scalar,     &
!     &          i_sgs, coef, sgs_e, flux_e)
!      subroutine SGS_tensor_cst_each_ele_1st(k2, i_vect, i_sgs,        &
!     &          coef, sgs_e, flux_e)
!      subroutine SGS_induct_cst_each_ele_1st(k2, i_b, i_v, i_sgs,      &
!     &          coef, sgs_e, flux_e)
!
!      subroutine SGS_flux_cst_ele_vec_1st(k2, nd,                      &
!     &          i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!      subroutine SGS_induct_vec_cst_ele_1st(k2, nd,                    &
!     &          i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      module sgs_terms_to_each_ele_1st
!
      use m_precision
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_each_ele_1st(k2, i_vect, i_scalar,          &
     &          i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_vector_2_each_element(node1%numnod, numele, nnod_4_ele,  &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_scalar,       &
     &          i_sgs, num_tot_nod_phys, d_nod, sgs_e, flux_e)
!
      end subroutine SGS_vector_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_each_ele_1st(k2, i_vect, i_sgs,             &
     &          sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(numele,6)
!
!
      call SGS_tensor_2_each_element(node1%numnod, numele, nnod_4_ele,  &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_sgs,          &
     &          num_tot_nod_phys, d_nod, sgs_e, flux_e)
!
      end subroutine SGS_tensor_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_each_ele_1st(k2, i_b, i_v, i_sgs,          &
     &          sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_to_each_element(node1%numnod, numele, nnod_4_ele, &
     &          ie, np_smp, iele_smp_stack, k2, i_b, i_v, i_sgs,        &
     &          num_tot_nod_phys, d_nod, sgs_e, flux_e)
!
      end subroutine SGS_induct_each_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_ele_vec_1st(k2, nd,                           &
     &          i_vect, i_field, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_flux_2_each_element_vec(node1%numnod, numele,            &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_vect, i_field, i_sgs, num_tot_nod_phys, d_nod,        &
     &          sgs_e, flux_e)
!
      end subroutine SGS_flux_ele_vec_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_ele_1st(k2, nd,                         &
     &          i_b, i_v, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_vec_2_each_element(node1%numnod, numele,          &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_b, i_v, i_sgs, num_tot_nod_phys, d_nod,               &
     &          sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_coef_each_ele_1st(k2, i_vect, i_scalar,     &
     &          i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_vector_coef_each_ele(node1%numnod, numele, nnod_4_ele,   &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_scalar,       &
     &          i_sgs, num_tot_nod_phys, d_nod, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_vector_coef_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_coef_each_ele_1st(k2, i_vect, i_sgs,       &
     &          ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: flux_e(numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(numele,6)
!
!
      call SGS_tensor_coef_each_ele(node1%numnod, numele, nnod_4_ele,   &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_sgs,          &
     &          num_tot_nod_phys, d_nod, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_tensor_coef_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_coef_each_ele_1st(k2, i_b, i_v, i_sgs,     &
     &          ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_coef_each_ele(node1%numnod, numele, nnod_4_ele,   &
     &          ie, np_smp, iele_smp_stack, k2, i_b, i_v, i_sgs,        &
     &          num_tot_nod_phys, d_nod, ak_e, sgs_e, flux_e)
!
      end subroutine SGS_induct_coef_each_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_coef_ele_vec_1st(k2, nd,                      &
     &          i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_flux_coef_each_ele_vec(node1%numnod, numele,             &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_vect, i_field, i_sgs, num_tot_nod_phys, d_nod, ak_e,  &
     &          sgs_e, flux_e)
!
      end subroutine SGS_flux_coef_ele_vec_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_coef_ele_1st(k2, nd,                    &
     &          i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_vec_coef_each_ele(node1%numnod, numele,           &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_b, i_v, i_sgs, num_tot_nod_phys, d_nod, ak_e,         &
     &          sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_coef_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_cst_each_ele_1st(k2, i_vect, i_scalar,      &
     &          i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_vector_cst_each_ele(node1%numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_scalar,       &
     &          i_sgs, num_tot_nod_phys, d_nod, coef, sgs_e, flux_e)
!
      end subroutine SGS_vector_cst_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_cst_each_ele_1st(k2, i_vect, i_sgs,        &
     &          coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(numele,6)
!
!
      call SGS_tensor_cst_each_ele(node1%numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_sgs,          &
     &          num_tot_nod_phys, d_nod, coef, sgs_e, flux_e)
!
      end subroutine SGS_tensor_cst_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_cst_each_ele_1st(k2, i_b, i_v, i_sgs,      &
     &          coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_cst_each_ele(node1%numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_b, i_v, i_sgs,        &
     &          num_tot_nod_phys, d_nod, coef, sgs_e, flux_e)
!
      end subroutine SGS_induct_cst_each_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_cst_ele_vec_1st(k2, nd,                       &
     &          i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_flux_cst_each_ele_vec(node1%numnod, numele,              &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_vect, i_field, i_sgs, num_tot_nod_phys, d_nod, coef,  &
     &          sgs_e, flux_e)
!
      end subroutine SGS_flux_cst_ele_vec_1st
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_cst_ele_1st(k2, nd,                     &
     &          i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
!
      call SGS_induct_vec_cst_each_ele(node1%numnod, numele,            &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_b, i_v, i_sgs, num_tot_nod_phys, d_nod, coef,         &
     &          sgs_e, flux_e)
!
      end subroutine SGS_induct_vec_cst_ele_1st
!
!  ---------------------------------------------------------------------
!
      end module sgs_terms_to_each_ele_1st
