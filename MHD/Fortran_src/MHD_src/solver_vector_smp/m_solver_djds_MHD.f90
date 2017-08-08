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
      type(FEM_MHD_solvers), save :: MHD_CG1
!
      end module m_solver_djds_MHD
