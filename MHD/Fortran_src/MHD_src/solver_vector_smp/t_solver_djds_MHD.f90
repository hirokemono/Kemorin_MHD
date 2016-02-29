!>@file   t_solver_djds_MHD.f90
!!@brief  module t_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine deallocate_comm_table_fluid
!
      module t_solver_djds_MHD
!
      use m_precision
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
!
      implicit none
!
!
!>        Structure of matrices for MHD dynamo simulation
      type MHD_MG_matrices
!>        Structure of matrix for time evolution of velocity
        type(DJDS_MATRIX), pointer :: Vmat_MG_DJDS(:)
!>        Structure of matrix for time evolution of magnetic field
        type(DJDS_MATRIX), pointer :: Bmat_MG_DJDS(:)
!
!>        Structure of matrix for  pressure
        type(DJDS_MATRIX), pointer :: Pmat_MG_DJDS(:)
!>        Structure of matrix for  scalar potential
        type(DJDS_MATRIX), pointer :: Fmat_MG_DJDS(:)
!
!>        Structure of matrix for time evolution of temperature
        type(DJDS_MATRIX), pointer :: Tmat_MG_DJDS(:)
!>        Structure of matrix for time evolution of composition
        type(DJDS_MATRIX), pointer :: Cmat_MG_DJDS(:)
!
!>        DJDS ordering structures for entire domain
        type(DJDS_ordering_table), pointer :: MG_DJDS_table(:)
!>        DJDS ordering structures for linear entire domain
        type(DJDS_ordering_table), pointer :: MG_DJDS_linear(:)
!>        Communication table structure for entire domain
        type(communication_table), pointer :: MG_comm_table(:)
!
!>        DJDS ordering structures for fluid
        type(DJDS_ordering_table), pointer :: MG_DJDS_fluid(:)
!>        DJDS ordering structures for linear fluid
        type(DJDS_ordering_table), pointer :: MG_DJDS_lin_fl(:)
!>        Communication table structure for entire domain
        type(communication_table), pointer :: MG_comm_fluid(:)
!      end type MHD_MG_matrices
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_MHD_MG_DJDS_mat(num_MG_level, matrices)
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MHD_MG_matrices), intent(inout) :: matrices
!
!
      allocate(matrices%Vmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Bmat_MG_DJDS(0:num_MG_level))
!
      allocate(matrices%Pmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Fmat_MG_DJDS(0:num_MG_level))
!
      allocate(matrices%Tmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Cmat_MG_DJDS(0:num_MG_level))
!
!
      allocate(matrices%MG_DJDS_table(0:num_MG_level))
      allocate(matrices%MG_DJDS_linear(0:num_MG_level))
      allocate(matrices%MG_comm_table(0:num_MG_level))
!
      allocate(matrices%MG_DJDS_fluid(0:num_MG_level))
      allocate(matrices%MG_DJDS_lin_fl(0:num_MG_level))
      allocate(matrices%MG_comm_fluid(0:num_MG_level))
!
      end subroutine alloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_MHD_MG_DJDS_mat(matrices)
!
      type(MHD_MG_matrices), intent(inout) :: matrices
!
!
      deallocate(matrices%Vmat_MG_DJDS, matrices%Bmat_MG_DJDS)
      deallocate(matrices%Pmat_MG_DJDS, matrices%Fmat_MG_DJDS)
      deallocate(matrices%Tmat_MG_DJDS, matrices%Cmat_MG_DJDS)
!
      deallocate(matrices%MG_DJDS_table, matrices%MG_DJDS_linear)
      deallocate(matrices%MG_comm_table)
      deallocate(matrices%MG_DJDS_fluid, matrices%MG_DJDS_lin_fl)
      deallocate(matrices%MG_comm_fluid)
!
      end subroutine dealloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      end module t_solver_djds_MHD