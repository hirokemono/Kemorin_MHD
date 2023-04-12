!>@file  t_repartition_tables_IO.f90
!!      module t_repartition_tables_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief Structures for repartition table IO
!!
!!@verbatim
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module t_repartition_tables_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
!
      implicit none
!
!>      Structures for repartition table IO
      type repartition_tables_IO
!>        Send table to repartitioned node address
        type(communication_table) :: nod_repart_import
!>        Send table from original node address
        type(communication_table) :: nod_repart_export
!
!>        Recieve table to repartitioned element address
        type(communication_table) :: ele_repart_import
!>        Recieve table from original element address
        type(communication_table) :: ele_repart_export
!
!>        Communication table fe repartitioned node
        type(communication_table) :: nod_comm_IO
!>        Communication table fe repartitioned element
        type(communication_table) :: ele_comm_IO
      end type repartition_tables_IO
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      end module t_repartition_tables_IO
