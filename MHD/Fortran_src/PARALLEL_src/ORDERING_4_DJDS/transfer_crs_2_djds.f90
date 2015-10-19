!>@file   transfer_crs_2_djds.f90
!!@brief  module transfer_crs_2_djds
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>      DJDS matrix connection from CRS matrix
!!
!!@verbatim
!!      subroutine transfer_crs_2_djds_matrix
!!      subroutine set_new_comm_table_entire
!!@endverbatim
!
      module transfer_crs_2_djds
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs)
!
      use t_crs_matrix
      use m_matrix_data_4_djds
      use m_solver_djds
      use set_size_4_smp_types
      use copy_matrix_2_djds_array
      use DJDS_const_solver_list
      use DJDS_new_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      call copy_paramters_4_djds
!
      call count_node_4_smp_mesh_type(node)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
       if (iflag_debug.eq.1) write(*,*) 'reordering_djds_smp'
      call reordering_djds_smp(node)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call allocate_new_comm_table                                      &
     &   (nod_comm%istack_export(nod_comm%num_neib))
      call set_new_comm_table                                           &
     &   (node%numnod, OLDtoNEW, nod_comm%num_neib,                     &
     &    nod_comm%istack_export, nod_comm%item_export, NOD_EXPORT_NEW)
!
!C +-------------+
!C | copy matrix |
!C +-------------+
!
      call allocate_vector_data_4_djds(node%numnod)
      call allocate_matrix_data_4_djds                                  &
     &   (node%internal_node, node%numnod)
!
       if (iflag_debug.eq.1) write(*,*) 'copy_matrix_2_djds_NN'
      call copy_matrix_2_djds_NN(node%internal_node, node%numnod)
       if (iflag_debug.eq.1) write(*,*) 'copy_RH_vect_2_crs_nn'
      call copy_RH_vect_2_crs_nn(node%numnod)
!
      call dealloc_crs_connect(tbl_crs)
!
      end subroutine transfer_crs_2_djds_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_entire(node, nod_comm)
!
      use m_solver_djds
      use DJDS_new_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
!
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_new_comm_table'
      call allocate_new_comm_table(nod_comm%ntot_export)
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_comm_table'
      call set_new_comm_table                                           &
     &   (node%numnod, OLDtoNEW, nod_comm%num_neib,                     &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    NOD_EXPORT_NEW)
!
      end subroutine set_new_comm_table_entire
!
!-----------------------------------------------------------------------
!
      end module transfer_crs_2_djds
