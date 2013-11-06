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
      use t_comm_table
      use t_crs_connect
      use t_solver_djds
!
      implicit none
!
!
!>      DJDS ordering structures for entire domain
      type(DJDS_ordering_table), save :: DJDS_entire
!>      DJDS ordering structures for linear entire domain
      type(DJDS_ordering_table), save :: DJDS_linear
!>      Communication table structure for entire domain
      type(communication_table), save :: DJDS_comm_etr
!
!>      DJDS ordering structures for fluid region
      type(DJDS_ordering_table), save :: DJDS_fluid
!>      DJDS ordering structures for linear fluid region
      type(DJDS_ordering_table), save :: DJDS_fl_l
!>      Communication table structure for fluid
      type(communication_table), save :: DJDS_comm_fl
!
!>      Communication table structure for conductor
      type(communication_table), save :: DJDS_comm_cd
!
!>      Communication table structure for insulator
      type(communication_table), save :: DJDS_comm_ins
!
!>      Matrix connectivity with CRS format
      type(CRS_matrix_connect), save :: MHD_CRS
!
      end module m_solver_djds_MHD
