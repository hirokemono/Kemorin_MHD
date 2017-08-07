!
!      module cal_rms_potentials
!
!     Written by H. Matsui on March, 2006
!
!      subroutine cal_rms_pressure_4_loop(iloop, rsig,                  &
!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!      subroutine cal_rms_scalar_potential(iloop, rsig)
!
      module cal_rms_potentials
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_mean_square_values
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_scalar_potential                               &
     &         (iloop, iele_fsmp_stack, i_phi, ir_phi, ja_phi,          &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,         &
     &          rsig, ave_0, rms_0)
!
      use int_all_energy
!
      integer(kind = kint), intent(in) :: iloop
      integer(kind = kint), intent(in) :: i_phi, ir_phi, ja_phi
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real(kind = kreal), intent(inout) :: rsig, ave_0, rms_0
!
      integer(kind = kint) :: num_int
!
      real(kind = kreal) :: ave_mp, rms_mp
!
!
      num_int = ione
!
      fem_msq1%rms_local(ir_phi) = zero
      fem_msq1%ave_local(ja_phi) = zero
      call int_all_4_scalar                                             &
     &   (iele_fsmp_stack, num_int, ir_phi, ja_phi, i_phi,              &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call MPI_allREDUCE(fem_msq1%ave_local(ja_phi) , ave_mp, ione,     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(fem_msq1%rms_local(ir_phi) , rms_mp, ione,     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (iloop .eq. 0) then
        ave_0 = ave_mp
        rms_0 = rms_mp
      end if
!
      if(rms_0 .eq. 0.0d0) then
        rsig = (rms_mp - rms_0)
      else
        rsig = (rms_mp - rms_0) / rms_0
      end if
!
      end subroutine cal_rms_scalar_potential
!
! ----------------------------------------------------------------------
!
      end module cal_rms_potentials
