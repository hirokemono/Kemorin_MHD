!
!      module int_all_energy
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_all_4_vector                                     &
!!     &         (iele_fsmp_stack, n_int,  ir_rms, ja_ave, i_vect,      &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!!      subroutine int_all_4_scalar                                     &
!!     &     (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_comp,           &
!!     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!!      subroutine int_all_angular_mom                                  &
!!     &         (iele_fsmp_stack, n_int, ja_ave, i_vect,               &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,               &
!!     &          mhd_fem_wk, fem_wk)
!!      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int,         &
!!     &          i_fld, node, ele, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          fem_wk, rms_local, ave_local)
!
      module int_all_energy
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
      private :: int_vol_all_energy
      private :: int_vol_ave_rms_4_scalar, int_vol_angular_mom
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_vector                                       &
     &         (iele_fsmp_stack, n_int,  ir_rms, ja_ave, i_vect,        &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      use m_mean_square_values
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
!
!
      if( (ir_rms*i_vect) .gt. 0) then
        call int_vol_all_energy(iele_fsmp_stack, n_int, i_vect,         &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_local(ir_rms), fem_msq1%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_scalar                                       &
     &     (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_comp,             &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      use m_mean_square_values
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_comp
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      if( (ir_rms*i_comp) .gt. 0) then
        call int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int, i_comp,   &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_local(ir_rms), fem_msq1%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_scalar
!
! ----------------------------------------------------------------------
!
      subroutine int_all_angular_mom                                    &
     &         (iele_fsmp_stack, n_int, ja_ave, i_vect,                 &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          mhd_fem_wk, fem_wk)
!
      use m_mean_square_values
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if( (ja_ave*i_vect) .gt. 0) then
        call int_vol_angular_mom(iele_fsmp_stack, n_int, i_vect,        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l,                     &
     &      fem_wk, mhd_fem_wk, fem_msq1%ave_local(ja_ave))
      end if
!
      end subroutine int_all_angular_mom
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_all_energy(iele_fsmp_stack, n_int, i_fld,      &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,         &
     &          rms_local, ave_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_fld
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(3)
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,            &
     &      k2, i_fld, fem_wk%vector_1)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_all_energy(ele%numele, ele%nnod_4_ele,           &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_all_energy(ele%numele, ele%nnod_4_ele,           &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, fem_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_all_energy
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_4_scalar                               &
     &         (iele_fsmp_stack, n_int, i_fld,                          &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,         &
     &          rms_local, ave_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_fld
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_fld, fem_wk%scalar_1)
!
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, fem_wk%scalar_1, rms_local, ave_local)
        else
          call fem_vol_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, fem_wk%scalar_1, rms_local, ave_local)
        end if
      end do
!
      end subroutine int_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_angular_mom(iele_fsmp_stack, n_int, i_fld,     &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          fem_wk, mhd_fem_wk, amom_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_fld
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      real (kind=kreal), intent(inout) :: amom_local(3)
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_fld, fem_wk%vector_1)
        call position_2_each_element(node, ele,                         &
     &      k2, mhd_fem_wk%xx_e, mhd_fem_wk%rr_e)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_angular_momentum(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,     &
     &        k2, mhd_fem_wk%xx_e, fem_wk%vector_1, amom_local)
        else
          call fem_vol_angular_momentum(ele%numele, ele%nnod_4_ele,     &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%xjac, jac_3d_l%an,     &
     &        k2, mhd_fem_wk%xx_e, fem_wk%vector_1, amom_local)
        end if
      end do
!
      end subroutine int_vol_angular_mom
!
! ----------------------------------------------------------------------
!
      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int,           &
     &          i_fld, node, ele, nod_fld, jac_3d_q, jac_3d_l,          &
     &          fem_wk, rms_local, ave_local)
!
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_fld
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_fld, fem_wk%scalar_1)
!
        if (ele%nnod_4_ele .eq. num_t_quad) then
          call fem_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,         &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_q%ntot_int, n_int, jac_3d_q%an, k2,                &
     &        fem_wk%scalar_1, rms_local, ave_local)
        else
          call fem_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,         &
     &        iele_fsmp_stack, ele%interior_ele,                        &
     &        jac_3d_l%ntot_int, n_int, jac_3d_l%an, k2,                &
     &        fem_wk%scalar_1, rms_local, ave_local)
        end if
      end do
!
      end subroutine int_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      end module int_all_energy
