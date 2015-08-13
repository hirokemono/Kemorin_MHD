!cal_skv_to_ff_smp_1st.f90
!     module cal_skv_to_ff_smp_1st
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!
!  -------- Assemble routines from skv to ff_smp for scalar
!      subroutine add1_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!      subroutine add1_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!      subroutine sub1_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!      subroutine sub1_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
!  -------- Assemble routines from skv to ff_smp for vector
!      subroutine add3_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!      subroutine add3_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!      subroutine sub3_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!      subroutine sub3_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
!  -------- Assemble routines from skv to ff_smp for tensor
!      subroutine add6_skv_to_ff_t_smp_1st(ff_v_smp, sk_v)
!      subroutine add6_skv_coef_to_ff_t_smp_1st(coef, ff_v_smp, sk_v)
!      subroutine sub6_skv_to_ff_t_smp_1st(ff_v_smp, sk_v)
!      subroutine sub6_skv_coef_to_ff_t_smp_1st(coef, ff_v_smp, sk_v)
!
      module cal_skv_to_ff_smp_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_sorted_node
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine add1_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call add_skv_scalar_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_v_smp, sk_v)
!
      end subroutine add1_skv_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine add1_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call add_skv_scalar_coef_2_ff_smp                                 &
     &   (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_v_smp, sk_v)
!
      end subroutine add1_skv_coef_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call sub_skv_scalar_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_v_smp, sk_v)
!
      end subroutine sub1_skv_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine sub1_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
      use cal_skv_to_ff_scalar_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call sub_skv_scalar_coef_2_ff_smp                                 &
     &   (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_v_smp, sk_v)
!
      end subroutine sub1_skv_coef_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add3_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call add_skv_vector_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_v_smp, sk_v)
!
      end subroutine add3_skv_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine add3_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call add_skv_vector_coef_2_ff_smp                                 &
         (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_v_smp, sk_v)
!
      end subroutine add3_skv_coef_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_to_ff_v_smp_1st(ff_v_smp, sk_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call sub_skv_vector_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_v_smp, sk_v)
!
      end subroutine sub3_skv_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine sub3_skv_coef_to_ff_v_smp_1st(coef, ff_v_smp, sk_v)
!
      use cal_skv_to_ff_vector_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
!
      call sub_skv_vector_coef_2_ff_smp                                 &
     &   (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_v_smp, sk_v)
!
      end subroutine sub3_skv_coef_to_ff_v_smp_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add6_skv_to_ff_t_smp_1st(ff_t_smp, sk_v)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(maxnod_4_smp,n_sym_tensor,np_smp)
!
!
      call add_skv_tensor_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_t_smp, sk_v)
!
      end subroutine add6_skv_to_ff_t_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine add6_skv_coef_to_ff_t_smp_1st(coef, ff_t_smp, sk_v)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(maxnod_4_smp,n_sym_tensor,np_smp)
!
!
      call add_skv_tensor_coef_2_ff_smp                                 &
     &   (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_t_smp, sk_v)
!
      end subroutine add6_skv_coef_to_ff_t_smp_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_to_ff_t_smp_1st(ff_t_smp, sk_v)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(maxnod_4_smp,n_sym_tensor,np_smp)
!
!
      call sub_skv_tensor_2_ff_smp (ele1%numele, ele1%nnod_4_ele,       &
     &    np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,             &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp,                 &
     &    ff_t_smp, sk_v)
!
      end subroutine sub6_skv_to_ff_t_smp_1st
!
! ----------------------------------------------------------------------
!
      subroutine sub6_skv_coef_to_ff_t_smp_1st(coef, ff_t_smp, sk_v)
!
      use cal_skv_to_ff_tensor_smp
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)                                     &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
      real (kind=kreal), intent(inout)                                  &
     &            :: ff_t_smp(maxnod_4_smp,n_sym_tensor,np_smp)
!
!
      call sub_skv_tensor_coef_2_ff_smp                                 &
     &   (ele1%numele, ele1%nnod_4_ele, np_smp,                         &
     &    maxnod_4_smp, inod_ele_max, num_sort_smp,                     &
     &    nod_stack_smp, iele_sort_smp, iconn_sort_smp, coef,           &
     &    ff_t_smp, sk_v)
!
      end subroutine sub6_skv_coef_to_ff_t_smp_1st
!
! ----------------------------------------------------------------------
!
      end module cal_skv_to_ff_smp_1st
