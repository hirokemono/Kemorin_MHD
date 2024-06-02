!>@file   trace_particle.f90
!!@brief  module trace_particle
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_trace_particle(dt_init, node, ele, surf,           &
!!     &          isf_4_ele_dbl, iele_4_surf_dbl,                       &
!!     &          nod_fld, fln_prm, fln_tce, fln_bcast, v_prev)
!!        real(kind = kreal), intent(in) :: dt_init
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!@endverbatim
!
      module trace_particle
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
      subroutine s_trace_particle(dt_init, node, ele, surf,             &
     &          isf_4_ele_dbl, iele_4_surf_dbl,                         &
     &          nod_fld, fln_prm, fln_tce, fline_lc,                    &
     &          fln_SR, fln_bcast, v_prev, m_SR)
!
      use t_control_params_4_fline
      use t_comm_table
      use t_next_node_ele_4_node
      use t_phys_data
      use t_source_of_filed_line
      use t_local_fline
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_mesh_SR
      use transfer_to_long_integers
      use trace_particle_in_element
!
      real(kind = kreal), intent(in) :: dt_init
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
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      type(mesh_SR), intent(inout) :: m_SR
!
      real(kind = kreal) :: dt
      integer(kind = kint) :: ist, ied, nline, inum
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)                                             &
     &         'num_current_fline', fln_tce%num_current_fline
        write(my_rank+50,*)                                             &
     &         'istack_current_fline', fln_tce%istack_current_fline(:)
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(my_rank+50,*) 'isf_fline_start(1:2,inum)'
        do inum = ist, ied
          write(my_rank+50,*) inum, fln_tce%isf_fline_start(1:2,inum)
        end do
      end if

      dt = dt_init
      call reset_fline_start(fline_lc)
      do
        do inum = 1, fln_tce%num_current_fline
          call s_trace_particle_in_element                              &
     &       (dt, node, surf, nod_fld, v_prev,                          &
     &        fln_prm%fline_fields, fln_prm%iphys_4_fline,              &
     &        fln_tce%isf_fline_start(1,inum),                          &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        fln_tce%iflag_comm_start(inum))
        end do
!
        if(fln_tce%iflag_comm_start(inum) .eq. 0) then
          call add_traced_list(fln_tce%isf_fline_start(1,inum),         &
     &                         fln_tce%xx_fline_start(1,inum),          &
     &                         fln_tce%v_fline_start(1,inum),           &
     &                         fln_prm%fline_fields%ntot_color_comp,    &
     &                         fln_tce%c_fline_start(1,inum),           &
     &                         fline_lc)
        end if
!
        if(fln_tce%num_current_fline .gt. 4096) then
          call s_trace_data_send_recv                                   &
     &       (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,                &
     &        fln_prm, fln_tce, fln_SR, m_SR, nline)
        else
          call s_broadcast_trace_data                                   &
     &     (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,                  &
     &      fln_prm, fln_tce, fln_bcast, nline)
        end if
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
!
      fln_tce%num_current_fline = fline_lc%nnod_line_l
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
      do inum = 1, fline_lc%nnod_line_l
        call return_to_trace_list(inum, fline_lc,                       &
     &      fln_tce%isf_fline_start(1,inum),                            &
     &      fln_tce%xx_fline_start(1,inum),                             &
     &      fln_tce%v_fline_start(1,inum),                              &
     &      fln_tce%c_fline_start(1,inum))
        end do
      end subroutine s_trace_particle
!
!  ---------------------------------------------------------------------
!
      end module trace_particle
