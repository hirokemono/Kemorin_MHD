!
!      module cal_rms_potentials
!
!     Written by H. Matsui on March, 2006
!
!!      subroutine cal_rms_scalar_potential                             &
!!     &         (iloop, iele_fsmp_stack, i_phi, ir_phi, ja_phi,        &
!!     &          mesh, nod_fld, jacs, fem_wk, fem_msq, rsig,           &
!!     &          ave_0, rms_0)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(mean_square_values), intent(inout)  :: fem_msq
!
      module cal_rms_potentials
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
      use t_finite_element_mat
      use t_mean_square_values
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
     &          mesh, nod_fld, jacs, fem_wk, fem_msq, rsig,             &
     &          ave_0, rms_0)
!
      use int_all_energy
!
      integer(kind = kint), intent(in) :: iloop
      integer(kind = kint), intent(in) :: i_phi, ir_phi, ja_phi
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout)  :: fem_msq
      real(kind = kreal), intent(inout) :: rsig, ave_0, rms_0
!
      integer(kind = kint) :: num_int
!
      real(kind = kreal) :: ave_mp, rms_mp
!
!
      num_int = ione
!
      fem_msq%rms_local(ir_phi) = zero
      fem_msq%ave_local(ja_phi) = zero
      call int_all_4_scalar                                             &
     &   (iele_fsmp_stack, num_int, ir_phi, ja_phi, i_phi,              &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call MPI_allREDUCE(fem_msq%ave_local(ja_phi) , ave_mp, 1,         &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(fem_msq%rms_local(ir_phi) , rms_mp, 1,         &
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
