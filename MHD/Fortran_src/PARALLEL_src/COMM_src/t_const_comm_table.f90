!>@file   t_const_comm_table.f90
!!@brief  module t_const_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2021
!
!>@brief  Routines to Construct communication table for elements
!!
!!@verbatim
!!      subroutine const_comm_table_by_connenct                         &
!!     &         (txt, numele, nnod_4_ele, ie, x_ele, node, nod_comm,   &
!!     &          inod_dbl, ip_ref, k_ref, neib_e, e_comm, fail_tbl)
!!        type(node_data), intent(in) :: node
!!        type(element_around_node), intent(in) :: neib_e
!!        type(communication_table), intent(in) :: nod_comm
!!        type(parallel_double_numbering), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: ip_ref(numele)
!!        integer(kind = kint), intent(in) :: k_ref(numele)
!!        type(communication_table), intent(inout) :: e_comm
!!        type(failed_table), intent(inout) :: fail_tbl
!!@endverbatim
!!
      module t_const_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_para_double_numbering
      use t_next_node_ele_4_node
      use t_failed_export_list
      use m_solver_SR
!
      implicit none
!
      type const_comm_table_work
!>        local id for element import table
        integer(kind = kint), allocatable :: inod_lc_import(:,:)
!>        home process for element import table
        integer(kind = kint), allocatable :: ipe_lc_import(:,:)
!>        position for element import table
        real(kind = kreal), allocatable :: xe_import(:)
!
!>        local id for element export table
        integer(kind = kint), allocatable :: inod_lc_export(:,:)
!>        home process for element export table
        integer(kind = kint), allocatable :: ipe_lc_export(:,:)
!>        position for element export table
        real(kind = kreal), allocatable :: xe_export(:)
      end type const_comm_table_work
!
      private :: element_data_reverse_SR
      private :: alloc_element_rev_imports, dealloc_element_rev_imports
      private :: alloc_element_rev_exports, dealloc_element_rev_exports
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct                           &
     &         (txt, numele, nnod_4_ele, ie, x_ele, node, nod_comm,     &
     &          inod_dbl, ip_ref, k_ref, neib_e, e_comm, fail_tbl)
!
      use reverse_SR_int
      use find_element_comm_table
      use const_global_element_ids
      use make_element_comm_table_SR
      use set_element_comm_table
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
      type(parallel_double_numbering), intent(in) :: inod_dbl
!
      integer(kind = kint), intent(in) :: ip_ref(numele)
      integer(kind = kint), intent(in) :: k_ref(numele)
!
      type(communication_table), intent(inout) :: e_comm
      type(failed_table), intent(inout) :: fail_tbl
!
      type(const_comm_table_work) :: wk_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(e_comm)
      call alloc_import_num(e_comm)
      call calypso_mpi_barrier
!
!      write(*,*) 'count_element_import_num', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+1)
      call count_element_import_num                                     &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import, numele, ip_ref)
!
      call alloc_import_item(e_comm)
!
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+3)
      call alloc_element_rev_imports                                    &
     &   (e_comm%ntot_import, nnod_4_ele, wk_comm)
      call set_element_import_item                                      &
     &   (node%numnod, inod_dbl%id_local(1), inod_dbl%ip_home(1),       &
     &    numele, nnod_4_ele, ie,x_ele, ip_ref, k_ref, e_comm%num_neib, &
     &    e_comm%id_neib, e_comm%istack_import, e_comm%item_import,     &
     &    wk_comm%inod_lc_import, wk_comm%ipe_lc_import,                &
     &    wk_comm%xe_import)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+3)
!
      call alloc_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+4)
      call element_num_reverse_SR                                       &
     &   (e_comm%num_neib, e_comm%id_neib, e_comm%num_import, SR_sig1,  &
     &    e_comm%num_export, e_comm%istack_export, e_comm%ntot_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+4)
!
!      write(*,*) 'element_data_reverse_SR2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+5)
      call alloc_element_rev_exports                                    &
     &   (e_comm%ntot_export, nnod_4_ele, wk_comm)
      call element_data_reverse_SR                                      &
     &   (nnod_4_ele, e_comm%num_neib, e_comm%id_neib,                  &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_lc_import, wk_comm%ipe_lc_import,                &
     &    wk_comm%xe_import, wk_comm%inod_lc_export,                    &
     &    wk_comm%ipe_lc_export, wk_comm%xe_export)
      call dealloc_element_rev_imports(wk_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+5)
!
      call alloc_export_item(e_comm)
!      write(*,*) 'set_element_export_item', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+6)
      call set_element_export_item                                      &
     &   (txt, node%numnod, numele, nnod_4_ele,                         &
     &    x_ele, neib_e%istack_4_node, neib_e%iele_4_node,              &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_lc_export, wk_comm%ipe_lc_export,                &
     &    wk_comm%xe_export, e_comm%item_export, fail_tbl)
      call dealloc_element_rev_exports(wk_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+6)
!
!      write(*,*) 'check_element_position', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+8)
      call check_element_position(txt, numele, x_ele, e_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+8)
!
      end subroutine const_comm_table_by_connenct
!
!-----------------------------------------------------------------------
!
      subroutine element_data_reverse_SR                                &
     &         (nnod_4_ele, num_neib_e, id_neib_e,                      &
     &          istack_import_e, istack_export_e,                       &
     &          inod_lc_import, ipe_lc_import, xe_import,               &
     &          inod_lc_export, ipe_lc_export,xe_export)
!
      use m_solver_SR
      use reverse_SR_real
      use reverse_SR_int
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      real(kind = kreal), intent(in)                                    &
     &         :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      real(kind = kreal), intent(inout)                                 &
     &         :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: k1
!
!
      call reverse_send_recv_3(num_neib_e, id_neib_e,                   &
     &    istack_import_e, istack_export_e, xe_import,                  &
     &    SR_sig1, xe_export)
!
      do k1 = 1, nnod_4_ele
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, inod_lc_import(1,k1),     &
     &      SR_sig1, inod_lc_export(1,k1))
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, ipe_lc_import(1,k1),      &
     &      SR_sig1, ipe_lc_export(1,k1))
      end do
!
      end subroutine element_data_reverse_SR
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_element_rev_imports                              &
     &         (ntot_import_e, nnod_4_ele, wk_comm)
!
      integer(kind = kint), intent(in) :: ntot_import_e, nnod_4_ele
      type(const_comm_table_work), intent(inout) :: wk_comm
!
!
      allocate(wk_comm%inod_lc_import(ntot_import_e,nnod_4_ele))
      allocate(wk_comm%ipe_lc_import(ntot_import_e,nnod_4_ele))
      allocate(wk_comm%xe_import(3*ntot_import_e))
!
      if(ntot_import_e .gt. 0) wk_comm%inod_lc_import = 0
      if(ntot_import_e .gt. 0) wk_comm%ipe_lc_import = 0
      if(ntot_import_e .gt. 0) wk_comm%xe_import = 0.0d0
!
      end subroutine alloc_element_rev_imports
!
!------------------------------------------------------------------
!
      subroutine alloc_element_rev_exports                              &
     &         (ntot_export_e, nnod_4_ele, wk_comm)
!
      integer(kind = kint), intent(in) :: ntot_export_e, nnod_4_ele
      type(const_comm_table_work), intent(inout) :: wk_comm
!
!
      allocate(wk_comm%inod_lc_export(ntot_export_e,nnod_4_ele))
      allocate(wk_comm%ipe_lc_export(ntot_export_e,nnod_4_ele))
      allocate(wk_comm%xe_export(3*ntot_export_e))
      if(ntot_export_e .gt. 0) wk_comm%inod_lc_export = 0
      if(ntot_export_e .gt. 0) wk_comm%ipe_lc_export = 0
      if(ntot_export_e .gt. 0) wk_comm%xe_export = 0.0d0
!
      end subroutine alloc_element_rev_exports
!
!------------------------------------------------------------------
!
      subroutine dealloc_element_rev_imports(wk_comm)
!
      type(const_comm_table_work), intent(inout) :: wk_comm
!
      deallocate(wk_comm%inod_lc_import, wk_comm%ipe_lc_import)
      deallocate(wk_comm%xe_import)
!
      end subroutine dealloc_element_rev_imports
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_element_rev_exports(wk_comm)
!
      type(const_comm_table_work), intent(inout) :: wk_comm
!
      deallocate(wk_comm%inod_lc_export, wk_comm%ipe_lc_export)
      deallocate(wk_comm%xe_export)
!
      end subroutine dealloc_element_rev_exports
!
!-----------------------------------------------------------------------
!
      end module t_const_comm_table
