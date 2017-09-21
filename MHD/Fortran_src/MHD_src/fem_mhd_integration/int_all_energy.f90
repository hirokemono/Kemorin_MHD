!
!      module int_all_energy
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_all_4_vector                                     &
!!     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,       &
!!     &          mesh, nod_fld, jacs, fem_wk, fem_msq)
!!      subroutine int_all_4_scalar                                     &
!!     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_comp,       &
!!     &          mesh, nod_fld, jacs, fem_wk, fem_msq)
!!      subroutine int_all_angular_mom                                  &
!!     &         (iele_fsmp_stack, n_int, ja_ave, i_vect,               &
!!     &          mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!!
!!      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int, i_fld,  &
!!     &          mesh, nod_fld, jacs, fem_wk, rms_local, ave_local)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(mean_square_values), intent(inout)  :: fem_msq
!
      module int_all_energy
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
      private :: int_vol_all_energy
      private :: int_vol_ave_rms_4_scalar, int_vol_angular_mom
      private :: int_ave_rms_4_scalar_q
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_vector                                       &
     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_vect,         &
     &          mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      use t_mean_square_values
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
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
        call int_vol_all_energy                                         &
     &     (iele_fsmp_stack, n_int, i_vect, mesh%node, mesh%ele,        &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%rms_local(ir_rms), fem_msq%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_scalar                                       &
     &         (iele_fsmp_stack, n_int, ir_rms, ja_ave, i_comp,         &
     &          mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      use t_mean_square_values
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_comp
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout)  :: fem_msq
!
!
      if( (ir_rms*i_comp) .gt. 0) then
        call int_vol_ave_rms_4_scalar                                   &
     &     (iele_fsmp_stack, n_int, i_comp, mesh%node, mesh%ele,        &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%rms_local(ir_rms), fem_msq%ave_local(ja_ave))
      end if
!
      end subroutine int_all_4_scalar
!
! ----------------------------------------------------------------------
!
      subroutine int_all_angular_mom                                    &
     &         (iele_fsmp_stack, n_int, ja_ave, i_vect,                 &
     &          mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
      use t_mean_square_values
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(mean_square_values), intent(inout)  :: fem_msq
!
!
      if( (ja_ave*i_vect) .gt. 0) then
        call int_vol_angular_mom(iele_fsmp_stack, n_int, i_vect,        &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      fem_wk, mhd_fem_wk, fem_msq%ave_local(ja_ave))
      end if
!
      end subroutine int_all_angular_mom
!
! ----------------------------------------------------------------------
!
      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int, i_fld,    &
     &          mesh, nod_fld, jacs, fem_wk, rms_local, ave_local)
!
      use m_geometry_constants
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_fld
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
!
      call int_ave_rms_4_scalar_q(iele_fsmp_stack, n_int, i_fld,        &
     &    mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,        &
     &   fem_wk, rms_local, ave_local)
!
      end subroutine int_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_all_energy(iele_fsmp_stack, n_int, i_fld,      &
     &          node, ele, nod_fld, g_FEM, jac_3d_q, fem_wk,            &
     &          rms_local, ave_local)
!
      use m_geometry_constants
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
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
        call fem_vol_all_energy(ele%numele, ele%nnod_4_ele,             &
     &      iele_fsmp_stack, ele%interior_ele, g_FEM%max_int_point,     &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,       &
     &      k2, fem_wk%vector_1, rms_local, ave_local)
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
     &          node, ele, nod_fld, g_FEM, jac_3d_q, fem_wk,            &
     &          rms_local, ave_local)
!
      use m_geometry_constants
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
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
        call fem_vol_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,       &
     &      iele_fsmp_stack, ele%interior_ele, g_FEM%max_int_point,     &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,       &
     &      k2, fem_wk%scalar_1, rms_local, ave_local)
      end do
!
      end subroutine int_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_angular_mom(iele_fsmp_stack, n_int, i_fld,     &
     &          node, ele, nod_fld, g_FEM, jac_3d_q,                    &
     &          fem_wk, mhd_fem_wk, amom_local)
!
      use m_geometry_constants
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
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
        call fem_vol_angular_momentum(ele%numele, ele%nnod_4_ele,       &
     &      iele_fsmp_stack, ele%interior_ele, g_FEM%max_int_point,     &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d_q%ntot_int, n_int, jac_3d_q%xjac, jac_3d_q%an,       &
     &      k2, mhd_fem_wk%xx_e, fem_wk%vector_1, amom_local)
      end do
!
      end subroutine int_vol_angular_mom
!
! ----------------------------------------------------------------------
!
      subroutine int_ave_rms_4_scalar_q(iele_fsmp_stack, n_int,         &
     &          i_fld, node, ele, nod_fld, g_FEM, jac_3d_q,             &
     &          fem_wk, rms_local, ave_local)
!
      use m_geometry_constants
!
      use nodal_fld_2_each_element
      use fem_vol_average_energy
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
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
        call fem_ave_rms_4_scalar(ele%numele, ele%nnod_4_ele,           &
     &      iele_fsmp_stack, ele%interior_ele, g_FEM%max_int_point,     &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d_q%ntot_int, n_int, jac_3d_q%an, k2,                  &
     &      fem_wk%scalar_1, rms_local, ave_local)
      end do
!
      end subroutine int_ave_rms_4_scalar_q
!
! ----------------------------------------------------------------------
!
      end module int_all_energy
