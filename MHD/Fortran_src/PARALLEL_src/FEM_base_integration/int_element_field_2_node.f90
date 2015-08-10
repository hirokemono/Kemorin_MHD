!
!      module int_element_field_2_node
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine cal_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine cal_ele_vector_2_node(vector_nod, vector_ele)
!      subroutine cal_ele_sym_tensor_2_node(tensor_nod, tensor_ele)
!
!      subroutine int_area_ele_scalar_2_node(iele_fsmp_stack,           &
!     &          scalar_ele)
!      subroutine int_area_ele_vector_2_node(iele_fsmp_stack,           &
!     &          vector_nod, vector_ele)
!      subroutine int_area_ele_sym_tensor_2_node(iele_fsmp_stack,       &
!     &          tensor_ele)
!
!      subroutine int_grp_ele_scalar_2_node(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, scalar_ele)
!      subroutine int_grp_ele_vector_2_node(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, vector_ele)
!      subroutine int_grp_ele_sym_tensor_2_node(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, tensor_ele)
!
      module int_element_field_2_node
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_finite_element_matrix
!
      use fem_skv_mass_mat_1st
      use fem_skv_nodal_field_1st
      use cal_skv_to_ff_smp_1st
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in) :: scalar_ele(numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node1%numnod)
!
!
      call int_area_ele_scalar_2_node(iele_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar (scalar_nod, ff_smp, ml)
!
      end subroutine cal_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_vector_2_node(vector_nod, vector_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
      real(kind = kreal), intent(inout) :: vector_nod(node1%numnod,3)
!
!
      call int_area_ele_vector_2_node(iele_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(vector_nod, ff_smp, ml)
!
      end subroutine cal_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_sym_tensor_2_node(tensor_nod, tensor_ele)
!
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(in) :: tensor_ele(numele,6)
      real(kind = kreal), intent(inout) :: tensor_nod(node1%numnod,6)
!
!
      call int_area_ele_sym_tensor_2_node(iele_smp_stack, tensor_ele)
      call cal_ff_smp_2_tensor(tensor_nod, ff_t_smp, ml)
!
      end subroutine cal_ele_sym_tensor_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_scalar_2_node(iele_fsmp_stack,            &
     &          scalar_ele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(numele)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_scalar)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_1st(iele_fsmp_stack,                 &
     &      max_int_point, scalar_ele, sk6)
      else
        call fem_skv_mass_mat_diag_HRZ_1st(iele_fsmp_stack,             &
     &      max_int_point, sk6)
        call fem_skv_scalar_on_ele_HRZ_1st(iele_fsmp_stack,             &
     &      ml_ele_diag, scalar_ele, sk6)
      end if
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_area_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_vector_2_node(iele_fsmp_stack,            &
     &          vector_ele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_vector)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_1st(iele_fsmp_stack,                 &
     &        max_int_point, vector_ele, sk6)
      else
        call fem_skv_mass_mat_diag_HRZ_1st(iele_fsmp_stack,             &
     &        max_int_point, sk6)
        call fem_skv_vector_on_ele_HRZ_1st(iele_fsmp_stack,             &
     &        ml_ele_diag, vector_ele, sk6 )
      end if
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_area_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_sym_tensor_2_node(iele_fsmp_stack,        &
     &          tensor_ele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: tensor_ele(numele,6)
!
!
      ff_t_smp = 0.0d0
      call reset_sk6(n_sym_tensor)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_tensor_on_ele_1st(iele_fsmp_stack,                 &
     &          max_int_point, tensor_ele, sk6)
      else
        call fem_skv_mass_mat_diag_HRZ_1st(iele_fsmp_stack,             &
     &          max_int_point, sk6)
        call fem_skv_tensor_on_ele_HRZ_1st(iele_fsmp_stack,             &
     &          ml_ele_diag, tensor_ele, sk6)
      end if
!
      call add6_skv_to_ff_t_smp_1st(ff_t_smp, sk6)
!
      end subroutine int_area_ele_sym_tensor_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_scalar_2_node(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, scalar_ele)
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(numele)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_scalar)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_grp_1st(iele_fsmp_stack,             &
     &      nele_grp, iele_grp, max_int_point, scalar_ele, sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ_1(iele_fsmp_stack,           &
     &      nele_grp, iele_grp, max_int_point, sk6)
        call fem_skv_scalar_on_egrp_HRZ_1st(iele_fsmp_stack,            &
     &      nele_grp, iele_grp, ml_ele_diag, scalar_ele, sk6)
      end if
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_grp_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_vector_2_node(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, vector_ele)
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(numele,3)
!
!
      ff_smp = 0.0d0
      call reset_sk6(n_vector)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_grp_1st(iele_fsmp_stack,             &
     &      nele_grp, iele_grp, max_int_point, vector_ele, sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ_1(iele_fsmp_stack,           &
     &      nele_grp, iele_grp,  max_int_point, sk6)
        call fem_skv_vector_on_egrp_HRZ_1st(iele_fsmp_stack,            &
     &      nele_grp, iele_grp, ml_ele_diag, vector_ele, sk6 )
      end if
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_grp_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_sym_tensor_2_node(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, tensor_ele)
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: tensor_ele(numele,6)
!
!
      ff_t_smp = 0.0d0
      call reset_sk6(n_sym_tensor)
!
      if (nnod_4_ele .eq. num_t_linear) then
        call fem_skv_tensor_on_ele_grp_1st(iele_fsmp_stack,             &
     &      nele_grp, iele_grp, max_int_point, tensor_ele, sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ_1(iele_fsmp_stack,           &
     &      nele_grp, iele_grp, max_int_point, sk6)
        call fem_skv_tensor_on_egrp_HRZ_1st(iele_fsmp_stack,            &
     &      nele_grp, iele_grp, ml_ele_diag, tensor_ele, sk6)
      end if
!
      call add6_skv_to_ff_t_smp_1st(ff_t_smp, sk6)
!
      end subroutine int_grp_ele_sym_tensor_2_node
!
!-----------------------------------------------------------------------
!
      end module int_element_field_2_node
