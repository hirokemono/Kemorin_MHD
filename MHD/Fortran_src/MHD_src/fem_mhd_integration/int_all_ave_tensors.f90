!
!      module int_all_ave_tensors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_all_4_sym_tensor                                 &
!!     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,       &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,               &
!!     &          fem_wk, fem_msq)
!!      subroutine int_all_4_asym_tensor                                &
!!     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,       &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,               &
!!     &          fem_wk, fem_msq)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(mean_square_values), intent(inout)  :: fem_msq
!
      module int_all_ave_tensors
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_finite_element_mat
!
      implicit none
!
      private :: int_vol_ave_rms_sym_tensor
      private :: int_vol_ave_rms_asym_tensor
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_sym_tensor                                   &
     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,         &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          fem_wk, fem_msq)
!
      use t_mean_square_values
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout)  :: fem_msq
!
!
      if( (ir_rms*i_vect) .gt. 0) then
        call int_vol_ave_rms_sym_tensor(iele_fsmp_stack, n_int,         &
     &      i_vect, node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,     &
     &      fem_msq%rms_local(ir_rms), fem_msq%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_asym_tensor                                  &
     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,         &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          fem_wk, fem_msq)
!
      use t_mean_square_values
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout)  :: fem_msq
!
!
      if( (ir_rms*i_vect) .gt. 0) then
        call int_vol_ave_rms_asym_tensor(iele_fsmp_stack, n_int,        &
     &      i_vect, node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,     &
     &      fem_msq%rms_local(ir_rms), fem_msq%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_asym_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_sym_tensor(iele_fsmp_stack, n_int,     &
     &          i_vect, node, ele, nod_fld, jac_3d_q, jac_3d_l,         &
     &          fem_wk, rms_local, ave_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_tensors
!
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: n_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(6)
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vect, fem_wk%vector_1)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_sym_tensor_1(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_sym_tensor_1(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        end if
!
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, (i_vect+3), fem_wk%vector_1)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_sym_tensor_2(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_sym_tensor_2(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_ave_rms_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_asym_tensor(iele_fsmp_stack, n_int,    &
     &          i_vect, node, ele, nod_fld, jac_3d_q, jac_3d_l,         &
     &          fem_wk, rms_local, ave_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_tensors
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: n_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(3)
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vect, fem_wk%vector_1)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_asym_tensor(ele%numele, ele%nnod_4_ele,      &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_asym_tensor(ele%numele, ele%nnod_4_ele,      &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        max_int_point, maxtot_int_3d, int_start3, owe3d,          &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_ave_rms_asym_tensor
!
! ----------------------------------------------------------------------
!
      end module int_all_ave_tensors
