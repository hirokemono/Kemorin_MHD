!>@file   const_field_lines.f90
!!@brief  module const_field_lines
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_const_field_lines                                  &
!!     &         (node, ele, surf, nod_fld,        &
!!     &          fln_prm, fln_tce, fln_bcast, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module const_field_lines
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_para_double_numbering
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_field_lines(node, ele, surf,                   &
     &          isf_4_ele_dbl, iele_4_surf_dbl,                         &
     &          nod_fld, fln_prm, fln_tce, fln_bcast, fline_lc, m_SR)
!
      use t_control_params_4_fline
      use t_comm_table
      use t_next_node_ele_4_node
      use t_phys_data
      use t_local_fline
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_source_of_filed_line
      use t_mesh_SR
      use calypso_SR
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
      use extend_field_line
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      type(trace_data_send_recv) :: fln_SR
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: ist, ied, nline, inum, i
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)                                             &
     &         'num_current_fline', fln_tce%num_current_fline(:)
        write(my_rank+50,*)                                             &
     &         'istack_current_fline', fln_tce%istack_current_fline(:)
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(my_rank+50,*) 'isf_fline_start(1:2,inum)'
        do inum = ist, ied
          write(my_rank+50,*) inum, fln_tce%isf_fline_start(1:2,inum)
        end do
      end if
      call calypso_MPI_barrier
!
      call reset_fline_start(fline_lc)
!
      do
        write(*,*) 'fln_tce%istack_current_fline', my_rank,             &
     &            fln_tce%istack_current_fline(my_rank:my_rank+1),      &
     &            fln_tce%num_current_fline(my_rank+1)
        do inum = 1, fln_tce%num_current_fline(my_rank+1)
          call s_extend_field_line                                      &
     &       (node, ele, surf, nod_fld, fln_prm%fline_fields,           &
     &        fln_prm%max_line_stepping, fln_prm%max_trace_length,      &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%iflag_direction(inum), fln_prm%iphys_4_fline,     &
     &        fln_tce%isf_fline_start(1,inum),                          &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        fln_tce%icount_fline(inum), fln_tce%trace_length(inum),   &
     &        fln_tce%iflag_comm_start(inum), fline_lc)
        end do
!
        call alloc_trace_data_SR_num(fln_prm%fline_fields, fln_SR)
        call s_trace_data_send_recv                                     &
     &     (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,                  &
     &      fln_tce, fln_SR, m_SR)
        call dealloc_trace_data_SR_num(fln_SR)
!
        call s_broadcast_trace_data                                     &
     &     (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,                  &
     &      fln_tce, fln_bcast, nline)
!
        write(*,*) my_rank, 'check'
        do i = 1, fln_tce%num_current_fline(my_rank+1)
          inum =  fln_SR%iRecv(i,1) - fln_tce%iline_original(i)
          if(inum .ne. 0) write(*,*) my_rank, i, inum
          inum =  fln_SR%iRecv(i,2) - fln_tce%iflag_direction(i)
          if(inum .ne. 0) write(*,*) my_rank, i, inum
          inum =  fln_SR%iRecv(i,3) - fln_tce%icount_fline(i)
          if(inum .ne. 0) write(*,*) my_rank, i, inum
          inum =  fln_SR%iRecv(i,4) - fln_tce%isf_fline_start(1,i)
          if(inum .ne. 0) write(*,*) my_rank, i, inum
          inum =  fln_SR%iRecv(i,5) - fln_tce%isf_fline_start(2,i)
          if(inum .ne. 0) write(*,*) my_rank, i, inum
        end do

      deallocate(fln_SR%rSend, fln_SR%iSend)
      deallocate(fln_SR%rRecv, fln_SR%iRecv)
!
        if(i_debug .gt. 0) then
          write(my_rank+50,*) 'istack_current_fline',                   &
     &                       fln_tce%istack_current_fline(:)
!
          write(my_rank+50,*) 'number of lines: ', nline
          write(*,*) 'number of lines: ', my_rank, nline
        end if
       if(nline .le. 0) exit
      end do
!
!      call check_local_fline_dx( (my_rank+60), fline_lc)
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
