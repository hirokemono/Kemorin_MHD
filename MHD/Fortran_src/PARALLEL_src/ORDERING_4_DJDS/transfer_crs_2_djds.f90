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
!!      subroutine set_smp_data_4_node
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
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine transfer_crs_2_djds_matrix
!
      use m_matrix_data_4_djds
      use m_crs_connect
      use m_nod_comm_table
      use m_solver_djds
      use copy_matrix_2_djds_array
      use DJDS_const_solver_list
      use DJDS_new_comm_table
!
!
      call copy_paramters_4_djds
!
      call set_smp_data_4_node
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
       if (iflag_debug.eq.1) write(*,*) 'reordering_djds_smp'
      call reordering_djds_smp
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call allocate_new_comm_table(istack_export(num_neib))
      call set_new_comm_table(numnod, OLDtoNEW, num_neib,               &
     &    istack_export, item_export, NOD_EXPORT_NEW)
!
!C +-------------+
!C | copy matrix |
!C +-------------+
!
      call allocate_vector_data_4_djds
      call allocate_matrix_data_4_djds
!
       if (iflag_debug.eq.1) write(*,*) 'copy_matrix_2_djds_NN'
      call copy_matrix_2_djds_NN
       if (iflag_debug.eq.1) write(*,*) 'copy_RH_vect_2_crs_nn'
      call copy_RH_vect_2_crs_nn
!
      call deallocate_crs_connect
!
      end subroutine transfer_crs_2_djds_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_smp_data_4_node
!
      use m_geometry_parameter
      use cal_minmax_and_stacks
!
       call allocate_geometry_param_smp
       if (iflag_debug.eq.1)  write(*,*) 'count_number_4_smp'
       call count_number_4_smp(np_smp, ione, internal_node,             &
     &       inter_smp_stack, max_in_nod_4_smp)
!
      end subroutine set_smp_data_4_node
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_entire
!
      use m_geometry_parameter
      use m_nod_comm_table
      use m_solver_djds
      use DJDS_new_comm_table
!
       if (iflag_debug.eq.1) write(*,*) 'allocate_new_comm_table'
       call allocate_new_comm_table(ntot_export)
!
       if (iflag_debug.eq.1) write(*,*) 'set_new_comm_table'
       call set_new_comm_table(numnod, OLDtoNEW, num_neib,              &
                istack_export, item_export, NOD_EXPORT_NEW)
!
      end subroutine set_new_comm_table_entire
!
!-----------------------------------------------------------------------
!
      end module transfer_crs_2_djds
