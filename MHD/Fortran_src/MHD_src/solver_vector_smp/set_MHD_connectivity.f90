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
!!      subroutine set_MHD_whole_connectivity
!!      subroutine set_MHD_layerd_connectivity
!!@endverbatim
!
      module set_MHD_connectivity
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
!
      use m_element_id_4_node
      use m_next_node_id_4_node
!
      implicit none
!
      private :: set_djds_whole_connectivity
      private ::  set_djds_layer_connectivity
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_whole_connectivity
!
      use const_RHS_assemble_list
!
!
!      set RHS assemble table
      call set_connect_RHS_assemble
!
!      set Matrix assemble table
      call set_djds_whole_connectivity
!
      call dealloc_iele_belonged(ele_4_nod1)
      call dealloc_inod_next_node(neib_nod1)
!
      end subroutine set_MHD_whole_connectivity
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_layerd_connectivity
!
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
!
      call set_djds_layer_connectivity                                  &
     &    (ele1%nnod_4_ele, iele_fl_start, iele_fl_end,                 &
     &     DJDS_comm_fl, solver_C, DJDS_fluid)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call set_djds_layer_connectivity(num_t_linear,                  &
     &     ione, ele1%numele, DJDS_comm_etr, solver_C, DJDS_linear)
        call set_djds_layer_connectivity                                &
     &     (num_t_linear, iele_fl_start, iele_fl_end,                   &
     &      DJDS_comm_fl, solver_C, DJDS_fl_l)
      else
        call link_djds_connect_structs(DJDS_entire, DJDS_linear)
        call link_djds_connect_structs(DJDS_fluid, DJDS_fl_l)
      end if
!
!
!      call set_djds_layer_connectivity                                 &
!     &   (ele1%nnod_4_ele, iele_cd_start, iele_cd_end,                 &
!     &    DJDS_comm_etr, solver_C, DJDS_conduct)
!      call set_djds_layer_connectivity                                 &
!     &   (ele1%nnod_4_ele, iele_ins_start, iele_ins_end,               &
!     &    DJDS_comm_etr, solver_C, DJDS_insulator)
!
!      if ( ele1%nnod_4_ele .ne. num_t_linear) then
!        call set_djds_layer_connectivity                               &
!     &     (num_t_linear, iele_cd_start, iele_cd_end,                  &
!     &      DJDS_comm_etr, solver_C, DJDS_cd_l)
!        call set_djds_layer_connectivity                               &
!     &     (num_t_linear, iele_ins_start, iele_ins_end,                &
!     &      DJDS_comm_etr, solver_C, DJDS_ins_l)
!      else
!        call link_djds_connect_structs(DJDS_conduct, DJDS_cd_l)
!        call link_djds_connect_structs(DJDS_insulator, DJDS_ins_l)
!      end if
!
      end subroutine set_MHD_layerd_connectivity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_djds_whole_connectivity
!
      use t_crs_connect
      use t_solver_djds
!
      use m_solver_djds_MHD
      use set_crs_connect_type
      use set_geometry_to_types
      use reordering_djds_smp_type
      use set_djds_smp_ordering_type
!
      type(CRS_matrix_connect) :: MHD_CRS
!
!C +-------------------------------+
!C | set connectivity in CRS array |
!C +-------------------------------+
!C===
      call s_set_crs_connect_type                                       &
     &         (np_smp, node1%numnod, node1%istack_nod_smp,             &
     &          neib_nod1%ntot, neib_nod1%istack_next,                  &
     &          neib_nod1%inod_next, MHD_CRS)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      call s_reordering_djds_smp_type                                   &
     &   (np_smp, node1%numnod, internal_node,                          &
     &    inter_smp_stack, solver_C, MHD_CRS, DJDS_entire)
!C
!      write(*,*) 'STACKmc', size(DJDS_entire%STACKmc)
!      write(*,*) 'NLmaxHYP', size(DJDS_entire%NLmaxHYP),               &
!     &          DJDS_entire%NHYP
!      write(*,*) 'NUmaxHYP', size(DJDS_entire%NUmaxHYP),               &
!     &          DJDS_entire%NHYP
!      write(*,*) 'OLDtoNEW', size(DJDS_entire%OLDtoNEW),               &
!     &          DJDS_entire%NP
!      write(*,*) 'OLDtoNEW_DJDS_L', size(DJDS_entire%OLDtoNEW_DJDS_L)
!      write(*,*) 'OLDtoNEW_DJDS_U', size(DJDS_entire%OLDtoNEW_DJDS_U)
!      write(*,*) 'indexDJDS_L', size(DJDS_entire%indexDJDS_L),         &
!     &          DJDS_entire%PEsmpTOT, DJDS_entire%NLmax,NHYP
!      write(*,*) 'indexDJDS_U', size(DJDS_entire%indexDJDS_U),         &
!     &          DJDS_entire%PEsmpTOT, DJDS_entire%NUmax,NHYP
!      write(*,*) 'itemDJDS_L', size(DJDS_entire%itemDJDS_L),           &
!     &          DJDS_entire%itotal_l
!      write(*,*) 'itemDJDS_U', size(DJDS_entire%itemDJDS_U),           &
!     &          DJDS_entire%itotal_u
!      write(*,*) 'PEon', size(DJDS_entire%PEon)
!      write(*,*) 'COLORon', size(DJDS_entire%COLORon)
!
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_nod_comm_tbl_2_type(DJDS_comm_etr)
      call set_new_comm_table_type                                      &
     &   (node1%numnod, DJDS_comm_etr, DJDS_entire)
!
      call dealloc_type_crs_connect(MHD_CRS)
!
      end subroutine set_djds_whole_connectivity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_djds_layer_connectivity(nnod_1ele,                 &
     &    iele_start, iele_end, layer_comm, solver_C, djds_tbl)
!
      use t_comm_table
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
      use m_next_node_id_4_node
      use m_element_id_4_node
!
      use reordering_djds_smp_type
      use set_djds_smp_ordering_type
      use set_crs_connect_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(communication_table), intent(in) :: layer_comm
      type(mpi_4_solver), intent(in) ::       solver_C
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      type(CRS_matrix_connect) :: MHD_CRS
!
!
      call set_layerd_ele_id_4_node(nnod_1ele, iele_start, iele_end)
      call const_next_nod_id_4_node
!
      call s_set_crs_connect_type                                       &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    neib_nod1%ntot, neib_nod1%istack_next,                        &
     &    neib_nod1%inod_next, MHD_CRS)
!
      call s_reordering_djds_smp_type                                   &
     &   (np_smp, node1%numnod, internal_node,                          &
     &    inter_smp_stack, solver_C, MHD_CRS, djds_tbl)
      call set_new_comm_table_type(node1%numnod, layer_comm, djds_tbl)
!
      call dealloc_type_crs_connect(MHD_CRS)
      call dealloc_iele_belonged(ele_4_nod1)
      call dealloc_inod_next_node(neib_nod1)
!
      end subroutine set_djds_layer_connectivity
!
!-----------------------------------------------------------------------
!
      end module set_MHD_connectivity
