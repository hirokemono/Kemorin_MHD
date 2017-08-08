!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
      module m_solver_djds_MHD
!
      use m_precision
      use t_FEM_MHD_solvers
!
!
      implicit none
!
!>      Matrix structure for FEM_MHD
!      type(FEM_MHD_solvers), save :: MHD_CG1
!MHD_CG1%MGCG_MHD_FEM
!
!>        Structure of matrices for MHD dynamo simulation
      type(MHD_MG_matrices), save :: MHD1_matrices
!
!>        Structure of matrices for all fields
      type(MHD_matrices_pack), save :: solver_pack1
!
!
!>      Structure for MPI communicator
      type(mpi_4_solver), save :: solver_C
!
!>      Communication table structure for fluid
      type(communication_table), save :: DJDS_comm_fl
!
!
      end module m_solver_djds_MHD
