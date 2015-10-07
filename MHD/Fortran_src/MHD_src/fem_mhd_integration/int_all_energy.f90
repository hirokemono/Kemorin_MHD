!
!      module int_all_energy
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_all_4_vector(iele_fsmp_stack, n_int,             &
!!     &          ir_rms, ja_ave, i_vect)
!!      subroutine int_all_angular_mom(iele_fsmp_stack, n_int,          &
!!     &          ja_ave, i_vect)
!!      subroutine int_all_4_scalar(iele_fsmp_stack, n_int,             &
!!     &          ir_rms, ja_ave, i_comp)
!
      module int_all_energy
!
      use m_precision
      use m_machine_parameter
      use m_finite_element_matrix
!
      implicit none
!
      private :: int_vol_all_energy
      private :: int_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_vector(iele_fsmp_stack, n_int,               &
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
        call int_vol_all_energy(iele_fsmp_stack, n_int,                 &
     &      i_vect, rms_local(ir_rms), bulk_local(ja_ave))
      end if
!
      end subroutine int_all_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine int_all_angular_mom(iele_fsmp_stack, n_int,            &
     &          ja_ave, i_vect)
!
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ja_ave
      integer (kind=kint), intent(in) :: n_int
!
!
      if( (ja_ave*i_vect) .gt. 0) then
        call int_vol_angular_mom(iele_fsmp_stack, n_int,                &
     &      i_vect, bulk_local(ja_ave))
      end if
!
      end subroutine int_all_angular_mom
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_scalar(iele_fsmp_stack, n_int,               &
     &          ir_rms, ja_ave, i_comp)
!
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_comp
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
!
      if( (ir_rms*i_comp) .gt. 0) then
        call int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int,           &
     &      i_comp, rms_local(ir_rms), bulk_local(ja_ave) )
      end if
!
      end subroutine int_all_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_all_energy(iele_fsmp_stack, n_int,             &
     &          i_fld, rms_local, ave_local)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_energy
!
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_fld
      integer (kind=kint), intent(in) :: n_int
!
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(3)
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_fld, fem1_wk%vector_1)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_all_energy(ele1%numele, ele1%nnod_4_ele,         &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        else
          call fem_vol_all_energy(ele1%numele, ele1%nnod_4_ele,         &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, fem1_wk%vector_1, rms_local, ave_local)
        end if
      end do
!
!
      end subroutine int_vol_all_energy
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int,       &
     &          i_fld, rms_local, ave_local)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_energy
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_fld
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
!
      integer (kind=kint) :: k2
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_phys_2_each_element(k2, i_fld, fem1_wk%scalar_1)
!
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_ave_rms_4_scalar(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, fem1_wk%scalar_1, rms_local, ave_local)
        else
          call fem_vol_ave_rms_4_scalar(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, fem1_wk%scalar_1, rms_local, ave_local)
        end if
      end do
!
      end subroutine int_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_angular_mom(iele_fsmp_stack, n_int,            &
     &          i_fld, amom_local)
!
      use m_geometry_constants
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_bulk_values
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_energy
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_fld
      integer (kind=kint), intent(in) :: n_int
!
      real (kind=kreal), intent(inout) :: amom_local(3)
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, i_fld, fem1_wk%vector_1)
        call position_2_each_element                                    &
     &     (k2, mhd_fem1_wk%xx_e, mhd_fem1_wk%rr_e)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_vol_angular_momentum(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,  &
     &        k2, mhd_fem1_wk%xx_e, fem1_wk%vector_1, amom_local)
        else
          call fem_vol_angular_momentum(ele1%numele, ele1%nnod_4_ele,   &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%xjac, jac1_3d_l%an,  &
     &        k2, mhd_fem1_wk%xx_e, fem1_wk%vector_1, amom_local)
        end if
      end do
!
      end subroutine int_vol_angular_mom
!
! ----------------------------------------------------------------------
!
      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int,           &
     &          i_fld, rms_local, ave_local)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
      use fem_vol_average_energy
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_fld
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: k2
!
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_phys_2_each_element(k2, i_fld, fem1_wk%scalar_1)
!
        if (ele1%nnod_4_ele .eq. num_t_quad) then
          call fem_ave_rms_4_scalar(ele1%numele, ele1%nnod_4_ele,       &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_q%ntot_int, n_int, jac1_3d_q%an, k2,              &
     &        fem1_wk%scalar_1, rms_local, ave_local)
        else
          call fem_ave_rms_4_scalar(ele1%numele, ele1%nnod_4_ele,       &
     &        iele_fsmp_stack, ele1%interior_ele,                       &
     &        jac1_3d_l%ntot_int, n_int, jac1_3d_l%an, k2,              &
     &        fem1_wk%scalar_1, rms_local, ave_local)
        end if
      end do
!
      end subroutine int_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      end module int_all_energy
