!>@file   set_djds_connectivity_type.f90
!!@brief  module set_djds_connectivity_type
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in Dec., 2008
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief     Construct index table for DJDS solver
!!
!!@verbatim
!!      subroutine s_set_djds_connectivity(nod_comm, node, solver_C,    &
!!     &          next_tbl, DJDS_param, DJDS_tbl)
!!        type(node_data), intent(in) :: node
!!        type(mpi_4_solver),        intent(in) :: solver_C
!!        type(next_nod_ele_table),  intent(in) :: next_tbl
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!!
!!      subroutine set_djds_layer_connectivity                          &
!!     &          (nnod_1ele, node, ele, iele_start, iele_end,          &
!!     &           layer_comm, solver_C, DJDS_param, DJDS_tbl)
!!        integer(kind = kint), intent(in) :: iele_start, iele_end
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: layer_comm
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!!
!!      subroutine empty_djds_connectivity(nod_comm, node, DJDS_tbl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!!@endverbatim
!
      module set_djds_connectivity_type
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_comm_table
      use t_next_node_ele_4_node
      use t_iccg_parameter
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
!
      use DJDS_new_comm_table
      use reordering_djds_smp_type
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_djds_connectivity(nod_comm, node, solver_C,      &
     &          next_tbl, DJDS_param, DJDS_tbl)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(mpi_4_solver),       intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
      type(CRS_matrix_connect) :: MHD_crs
!
!C +-------------------------------+
!C | set connectivity in CRS array |
!C +-------------------------------+
!C===
      call s_set_crs_connection(node, next_tbl%neib_nod, MHD_crs)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      call s_reordering_djds_smp(np_smp, node%numnod,                   &
     &    node%internal_node, node%istack_internal_smp,                 &
     &    solver_C, MHD_crs, DJDS_param, DJDS_tbl)
!
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
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_new_comm_table_type(node%numnod, nod_comm, DJDS_tbl)
      call dealloc_crs_connect(MHD_crs)
!
      end subroutine s_set_djds_connectivity
!
!-----------------------------------------------------------------------
!
      subroutine set_djds_layer_connectivity                            &
     &          (nnod_1ele, node, ele, iele_start, iele_end,            &
     &           layer_comm, solver_C, DJDS_param, DJDS_tbl)
!
      use t_comm_table
      use set_ele_id_4_node_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: layer_comm
      type(mpi_4_solver),        intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
      type(next_nod_ele_table) :: FEM_next
      type(CRS_matrix_connect) :: CRS_table
!
!
      call set_layerd_ele_id_4_node(nnod_1ele, iele_start, iele_end,    &
     &    node, ele, FEM_next%neib_ele)
!
      call const_next_nod_id_4_node(node, ele,                          &
     &    FEM_next%neib_ele, FEM_next%neib_nod)
!
      call s_set_crs_connection(node, FEM_next%neib_nod, CRS_table)
!
      call s_reordering_djds_smp(np_smp, node%numnod,                   &
     &    node%internal_node, node%istack_internal_smp,                 &
     &    solver_C, CRS_table, DJDS_param, DJDS_tbl)
!
      call set_new_comm_table_type                                      &
     &   (node%numnod, layer_comm, DJDS_tbl)
!
      call dealloc_crs_connect(CRS_table)
      call dealloc_iele_belonged(FEM_next%neib_ele)
      call dealloc_inod_next_node(FEM_next%neib_nod)
!
      end subroutine set_djds_layer_connectivity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_djds_connectivity(nod_comm, node, DJDS_tbl)
!
      use m_machine_parameter
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(DJDS_ordering_table), intent(inout) :: DJDS_tbl
!
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      DJDS_tbl%NHYP =     0
      DJDS_tbl%itotal_l = 0
      DJDS_tbl%itotal_u = 0
      call alloc_type_4_RCM(node%numnod, DJDS_tbl)
      call alloc_type_number_4_djds(DJDS_tbl)
      call alloc_type_lists_4_DJDS(np_smp, node%numnod, DJDS_tbl)
      call alloc_type_address_4_DJDS(DJDS_tbl)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call alloc_type_new_comm_table(nod_comm%ntot_export, DJDS_tbl)
!
      end subroutine empty_djds_connectivity
!
!-----------------------------------------------------------------------
!
      end module set_djds_connectivity_type
