!>@file   set_MHD_connectivity.f90
!!@brief  module set_MHD_connectivity
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in Jan., 2006
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief  Construct index table for DJDS solver
!!
!!@verbatim
!!      subroutine set_djds_whole_connectivity(nod_comm, node,          &
!!     &          solver_C, neib_nod, DJDS_tbl)
!!      subroutine set_djds_layer_connectivity(node, ele, nnod_1ele,    &
!!     &          iele_start, iele_end, layer_comm, solver_C, DJDS_tbl)
!!@endverbatim
!
      module set_MHD_connectivity
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_next_node_ele_4_node
      use t_crs_connect
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_djds_whole_connectivity(nod_comm, node,            &
     &          solver_C, neib_nod, DJDS_tbl)
!
      use t_geometry_data
      use t_comm_table
      use t_solver_djds
      use t_table_FEM_const
      use t_solver_djds
      use t_vector_for_solver
!
      use reordering_djds_smp_type
      use DJDS_new_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mpi_4_solver), intent(in) :: solver_C
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
      type(CRS_matrix_connect), save :: MHD_CRS
!
!
!C +-------------------------------+
!C | set connectivity in CRS array |
!C +-------------------------------+
!C===
      call s_set_crs_connection(node, neib_nod, MHD_CRS)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      call s_reordering_djds_smp(np_smp, node%numnod,                   &
     &    node%internal_node, node%istack_internal_smp,                 &
     &    solver_C, MHD_CRS, DJDS_tbl)
!C
!      write(*,*) 'STACKmc', size(DJDS_tbl%STACKmc)
!      write(*,*) 'NLmaxHYP', size(DJDS_tbl%NLmaxHYP),                  &
!     &          DJDS_tbl%NHYPDJDS_tbl
!      write(*,*) 'NUmaxHYP', size(DJDS_tbl%NUmaxHYP),                  &
!     &          DJDS_tbl%NHYP
!      write(*,*) 'OLDtoNEW', size(DJDS_tbl%OLDtoNEW),                  &
!     &          DJDS_tbl%NP
!      write(*,*) 'OLDtoNEW_DJDS_L', size(DJDS_tbl%OLDtoNEW_DJDS_L)
!      write(*,*) 'OLDtoNEW_DJDS_U', size(DJDS_tbl%OLDtoNEW_DJDS_U)
!      write(*,*) 'indexDJDS_L', size(DJDS_tbl%indexDJDS_L),            &
!     &          DJDS_tbl%PEsmpTOT, DJDS_tbl%NLmax,NHYP
!      write(*,*) 'indexDJDS_U', size(DJDS_tbl%indexDJDS_U),            &
!     &          DJDS_tbl%PEsmpTOT, DJDS_tbl%NUmax,NHYP
!      write(*,*) 'itemDJDS_L', size(DJDS_tbl%itemDJDS_L),              &
!     &          DJDS_tbl%itotal_l
!      write(*,*) 'itemDJDS_U', size(DJDS_tbl%itemDJDS_U),              &
!     &          DJDS_tbl%itotal_u
!      write(*,*) 'PEon', size(DJDS_tbl%PEon)
!      write(*,*) 'COLORon', size(DJDS_tbl%COLORon)
!
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_new_comm_table_type(node%numnod, nod_comm, DJDS_tbl)
!
      call dealloc_crs_connect(MHD_CRS)
!
      end subroutine set_djds_whole_connectivity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_djds_layer_connectivity(node, ele, nnod_1ele,      &
     &          iele_start, iele_end, layer_comm, solver_C, DJDS_tbl)
!
      use t_geometry_data
      use t_comm_table
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
!
      use set_ele_id_4_node_type
      use reordering_djds_smp_type
      use DJDS_new_comm_table
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: layer_comm
      type(mpi_4_solver), intent(in) ::       solver_C
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
      type(element_around_node), save :: ele_4_nod
      type(next_nod_id_4_nod), save :: neib_nod
      type(CRS_matrix_connect) :: MHD_CRS
!
!
      call set_layerd_ele_id_4_node(nnod_1ele, iele_start, iele_end,    &
     &    node, ele, ele_4_nod)
      call const_next_nod_id_4_node(node, ele, ele_4_nod, neib_nod)
!
      call s_set_crs_connection(node, neib_nod, MHD_CRS)
!
      call s_reordering_djds_smp(np_smp, node%numnod,                   &
     &    node%internal_node, node%istack_internal_smp,                 &
     &    solver_C, MHD_CRS, DJDS_tbl)
      call set_new_comm_table_type(node%numnod, layer_comm, DJDS_tbl)
!
      call dealloc_crs_connect(MHD_CRS)
      call dealloc_iele_belonged(ele_4_nod)
      call dealloc_inod_next_node(neib_nod)
!
      end subroutine set_djds_layer_connectivity
!
!-----------------------------------------------------------------------
!
      end module set_MHD_connectivity
