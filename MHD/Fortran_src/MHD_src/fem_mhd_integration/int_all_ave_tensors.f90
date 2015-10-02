!
!      module int_all_ave_tensors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_all_4_sym_tensor(iele_fsmp_stack, n_int,         &
!!     &          ir_rms, ja_ave, i_vect)
!!      subroutine int_all_4_asym_tensor(iele_fsmp_stack, n_int,        &
!!     &          ir_rms, ja_ave, i_vect)
!
      module int_all_ave_tensors
!
      use m_precision
      use m_machine_parameter
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
      subroutine int_all_4_sym_tensor(iele_fsmp_stack, n_int,           &
     &          ir_rms, ja_ave, i_vect)
!
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
!
      if( (ir_rms*i_vect) .gt. 0) then
        call int_vol_ave_rms_sym_tensor(iele_fsmp_stack, n_int,         &
     &      i_vect, rms_local(ir_rms), bulk_local(ja_ave))
      end if
!
      end subroutine int_all_4_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_asym_tensor(iele_fsmp_stack, n_int,          &
     &          ir_rms, ja_ave, i_vect)
!
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
!
      if( (ir_rms*i_vect) .gt. 0) then
        call int_vol_ave_rms_asym_tensor(iele_fsmp_stack, n_int,        &
     &      i_vect, rms_local(ir_rms), bulk_local(ja_ave))
      end if
!
      end subroutine int_all_4_asym_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_sym_tensor(iele_fsmp_stack, n_int,     &
     &          i_vect, rms_local, ave_local)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_finite_element_matrix
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_tensors
!
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: n_int
!
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(6)
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_vect, fem1_wk%vector_1)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_sym_tensor_1(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_sym_tensor_1(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        end if
!
        call vector_phys_2_each_element                                 &
     &     (k2, (i_vect+3), fem1_wk%vector_1)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_sym_tensor_2(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_sym_tensor_2(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_ave_rms_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_asym_tensor(iele_fsmp_stack, n_int,    &
     &          i_vect, rms_local, ave_local)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_finite_element_matrix
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_tensors
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: n_int
!
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(3)
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_vect, fem1_wk%vector_1)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_asym_tensor(ele1%numele, ele1%nnod_4_ele,    &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_ave_asym_tensor(ele1%numele, ele1%nnod_4_ele,    &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_ave_rms_asym_tensor
!
! ----------------------------------------------------------------------
!
      end module int_all_ave_tensors
