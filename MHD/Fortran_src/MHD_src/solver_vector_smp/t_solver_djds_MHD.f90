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
      end type MHD_MG_matrices
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
!
      end subroutine dealloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      end module t_solver_djds_MHD