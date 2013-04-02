!sgs_terms_to_each_ele_type.f90
!      module sgs_terms_to_each_ele_type
!
!      Written by H. Matsui on July, 2005
!
!      subroutine SGS_vector_each_ele_type(mesh, nod_fld,               &
!     &          k2, i_vect, i_scalar, i_sgs, sgs_e, flux_e)
!      subroutine SGS_tensor_each_ele_type(mesh, nod_fld,               &
!     &          k2, i_vect, i_sgs, sgs_e, flux_e)
!      subroutine SGS_induct_each_ele_type(mesh, nod_fld,               &
!     &          k2, i_b, i_v, i_sgs, sgs_e, flux_e)
!
!      subroutine SGS_flux_each_ele_vec_type(mesh, nod_fld,             &
!     &          k2, nd, i_vect, i_field, i_sgs, sgs_e, flux_e)
!      subroutine SGS_induct_vec_ele_type(mesh, nod_fld,                &
!     &          k2, nd, i_b, i_v, i_sgs, sgs_e, flux_e)
!
!
!      subroutine SGS_vector_coef_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_vect, i_scalar, i_sgs, ak_e, sgs_e, flux_e)
!      subroutine SGS_tensor_coef_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_vect, i_sgs, sgs_e, ak_e, flux_e)
!      subroutine SGS_induct_coef_each_ele_type(mesh, nod_fld,          &
!     &          k2, i_b, i_v, i_sgs, sgs_e, ak_e, flux_e)
!
!      subroutine SGS_flux_coef_each_ele_vec_type(mesh, nod_fld,        &
!     &          k2, nd, i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!      subroutine SGS_induct_vec_coef_ele_type(mesh, nod_fld,           &
!     &          k2, nd, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
!
!      subroutine SGS_vector_cst_each_ele_type(mesh, nod_fld,           &
!     &          k2, i_vect, i_scalar, i_sgs, coef, sgs_e, flux_e)
!      subroutine SGS_tensor_cst_each_ele_type(mesh, nod_fld,           &
!     &          k2, i_vect, i_sgs, coef, sgs_e, flux_e)
!      subroutine SGS_induct_cst_each_ele_type(mesh, nod_fld,           &
!     &          k2, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
!      subroutine SGS_flux_cst_each_ele_vec_type(mesh, nod_fld,         &
!     &          k2, nd, i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!      subroutine SGS_induct_vec_cst_ele_type(mesh, nod_fld,            &
!     &          k2, nd, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
!        type(mesh_geometry), intent(in) :: mesh
!        type(phys_data), intent(in) :: nod_fld
!        integer(kind = kint), intent(in) :: k2
!        integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
!        real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!        real (kind=kreal), intent(in) :: coef
!
!        real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
!        real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
      module sgs_terms_to_each_ele_type
!
      use m_precision
      use m_machine_parameter
      use t_mesh_data
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
      subroutine SGS_vector_each_ele_type(mesh, nod_fld,                &
     &          k2, i_vect, i_scalar, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_vector_2_each_element                                    &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_vect, i_scalar, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,    &
     &    sgs_e, flux_e)
!
      end subroutine SGS_vector_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_each_ele_type(mesh, nod_fld,                &
     &          k2, i_vect, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,6)
!
!
      call SGS_tensor_2_each_element                                    &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_vect, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,              &
     &    sgs_e, flux_e)
!
      end subroutine SGS_tensor_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_each_ele_type(mesh, nod_fld,                &
     &          k2, i_b, i_v, i_sgs, sgs_e, flux_e)
!
      use sgs_terms_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_to_each_element                                   &
     &    (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,      &
     &     mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,            &
     &     i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,           &
     &     sgs_e, flux_e)
!
      end subroutine SGS_induct_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_each_ele_vec_type(mesh, nod_fld,              &
     &          k2, nd, i_vect, i_field, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_flux_2_each_element_vec                                  &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_vect, i_field, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,     &
     &    sgs_e, flux_e)
!
      end subroutine SGS_flux_each_ele_vec_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_ele_type(mesh, nod_fld,                 &
     &          k2, nd, i_b, i_v, i_sgs, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_2_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_vec_2_each_element                                &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,            &
     &    sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_coef_each_ele_type(mesh, nod_fld,           &
     &          k2, i_vect, i_scalar, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_vector_coef_each_ele                                     &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_vect, i_scalar, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,    &
     &    ak_e, sgs_e, flux_e)
!
      end subroutine SGS_vector_coef_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_coef_each_ele_type(mesh, nod_fld,           &
     &          k2, i_vect, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
      real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,6)
!
!
      call SGS_tensor_coef_each_ele                                     &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_vect, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,              &
     &    ak_e, sgs_e, flux_e)
!
      end subroutine SGS_tensor_coef_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_coef_each_ele_type(mesh, nod_fld,           &
     &          k2, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
      use sgs_terms_coef_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_coef_each_ele                                     &
     &    (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,      &
     &     mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,            &
     &     i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,           &
     &     ak_e, sgs_e, flux_e)
!
      end subroutine SGS_induct_coef_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_coef_each_ele_vec_type(mesh, nod_fld,         &
     &          k2, nd, i_vect, i_field, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_flux_coef_each_ele_vec                                   &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_vect, i_field, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,     &
     &    ak_e, sgs_e, flux_e)
!
      end subroutine SGS_flux_coef_each_ele_vec_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_coef_ele_type(mesh, nod_fld,            &
     &          k2, nd, i_b, i_v, i_sgs, ak_e, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_coef_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: ak_e(mesh%ele%numele)
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_vec_coef_each_ele                                 &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,            &
     &    ak_e, sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_coef_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_cst_each_ele_type(mesh, nod_fld,            &
     &          k2, i_vect, i_scalar, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_vector_cst_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_vect, i_scalar, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,    &
     &    coef, sgs_e, flux_e)
!
      end subroutine SGS_vector_cst_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_cst_each_ele_type(mesh, nod_fld,            &
     &          k2, i_tensor, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_tensor, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,6)
!
!
      call SGS_tensor_cst_each_ele                                      &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,             &
     &    i_tensor, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,            &
     &    coef, sgs_e, flux_e)
!
      end subroutine SGS_tensor_cst_each_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_cst_each_ele_type(mesh, nod_fld,            &
     &          k2, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      use sgs_terms_cst_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_cst_each_ele                                      &
     &    (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,      &
     &     mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2,            &
     &     i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,           &
     &     coef, sgs_e, flux_e)
!
      end subroutine SGS_induct_cst_each_ele_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_cst_each_ele_vec_type(mesh, nod_fld,          &
     &          k2, nd, i_vect, i_field, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_flux_cst_each_ele_vec                                    &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_vect, i_field, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,     &
     &    coef, sgs_e, flux_e)
!
      end subroutine SGS_flux_cst_each_ele_vec_type
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_cst_ele_type(mesh, nod_fld,             &
     &          k2, nd, i_b, i_v, i_sgs, coef, sgs_e, flux_e)
!
      use m_phys_constants
      use sgs_terms_cst_each_element
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(mesh%ele%numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(mesh%ele%numele,3)
!
!
      call SGS_induct_vec_cst_each_ele                                  &
     &   (mesh%node%numnod, mesh%ele%numele, mesh%ele%nnod_4_ele,       &
     &    mesh%ele%ie, np_smp, mesh%ele%istack_ele_smp, k2, nd,         &
     &    i_b, i_v, i_sgs, nod_fld%ntot_phys, nod_fld%d_fld,            &
     &    coef, sgs_e, flux_e)
!
!
      end subroutine SGS_induct_vec_cst_ele_type
!
!  ---------------------------------------------------------------------
!
      end module sgs_terms_to_each_ele_type
