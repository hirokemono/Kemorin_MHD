!>@file   m_solver_djds_MHD.f90
!!@brief  module m_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine deallocate_comm_table_fluid
!
      module m_solver_djds_MHD
!
      use m_precision
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
!
      implicit none
!
!
!>      Structure for MPI communicator
      type(mpi_4_solver), save :: solver_C
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
!>      DJDS ordering structures for conductor region
      type(DJDS_ordering_table), save :: DJDS_conduct
!>      DJDS ordering structures for linear conductor region
      type(DJDS_ordering_table), save :: DJDS_cd_l
!>      Communication table structure for conductor
      type(communication_table), save :: DJDS_comm_cd
!
!>      DJDS ordering structures for conductor region
      type(DJDS_ordering_table), save :: DJDS_insulator
!>      DJDS ordering structures for linear conductor region
      type(DJDS_ordering_table), save :: DJDS_ins_l
!>      Communication table structure for insulator
      type(communication_table), save :: DJDS_comm_ins
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine deallocate_comm_table_fluid
!
!
      call deallocate_type_comm_tbl(DJDS_comm_fl)
!
      end subroutine deallocate_comm_table_fluid
!
!  ---------------------------------------------------------------------
!
      end module m_solver_djds_MHD
