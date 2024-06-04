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
!!     &         (mesh, para_surf, nod_fld, num_fline, fln_prm, fln_tce,&
!!     &          fln_SR, fln_bcast, fline_lc, m_SR)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!        type(local_fieldline), intent(inout) ::     fline_lc(num_fline)
!!        type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
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
      use t_mesh_SR
      use t_mesh_data
      use t_control_params_4_fline
      use t_comm_table
      use t_phys_data
      use t_paralell_surface_indices
      use t_local_fline
      use t_next_node_ele_4_node
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_mesh_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_field_lines                                    &
     &         (mesh, para_surf, nod_fld, num_fline, fln_prm, fln_tce,  &
     &          fln_SR, fln_bcast, fline_lc, m_SR)
!
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) ::      fline_lc(num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_fln
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call const_field_line(mesh, para_surf, nod_fld, fln_prm(i_fln), &
     &      fln_tce(i_fln), fln_SR(i_fln), fln_bcast(i_fln),            &
     &      fline_lc(i_fln), m_SR)
      end do
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      subroutine output_field_lines                                     &
     &         (istep_fline, time_d, num_fline, fln_prm, fline_lc)
!
      use set_fields_for_fieldline
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_fline
      type(time_data), intent(in) :: time_d
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      type(time_data) :: t_IO
      type(ucd_data) :: fline_ucd
      integer(kind = kint) :: i_fln
!  
!
      do i_fln = 1, num_fline
        call copy_time_step_size_data(time_d, t_IO)
        call copy_local_fieldline_to_IO(fln_prm(i_fln)%fline_fields,    &
     &                                  fline_lc(i_fln), fline_ucd)
        call sel_write_parallel_ucd_file                                &
     &     (istep_fline, fln_prm(i_fln)%fline_file_IO, t_IO, fline_ucd)
        call deallocate_parallel_ucd_mesh(fline_ucd)
!
        call copy_local_particles_to_IO(fln_prm(i_fln)%fline_fields,    &
     &                                  fline_lc(i_fln), fline_ucd)
        fln_prm(i_fln)%fline_file_IO%iflag_format = iflag_sgl_ucd
        call sel_write_parallel_ucd_file                                &
     &     (istep_fline, fln_prm(i_fln)%fline_file_IO, t_IO, fline_ucd)
        call deallocate_parallel_ucd_mesh(fline_ucd)
        call calypso_mpi_barrier
      end do
!
      end subroutine output_field_lines
!
!  ---------------------------------------------------------------------
!
      subroutine const_field_line(mesh, para_surf, nod_fld,             &
     &          fln_prm, fln_tce, fln_SR, fln_bcast, fline_lc, m_SR)
!
      use calypso_SR
      use transfer_to_long_integers
      use extend_field_line
!
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: nline, inum, ip
!
!
      call reset_fline_start(fline_lc)
!
      do
        do inum = 1, fln_tce%num_current_fline
          call s_extend_field_line(mesh%node, mesh%ele, mesh%surf,      &
     &        para_surf, nod_fld, fln_prm%fline_fields,                 &
     &        fln_prm%max_line_stepping, fln_prm%max_trace_length,      &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%iflag_direction(inum), fln_prm%iphys_4_fline,     &
     &        fln_tce%isf_dbl_start(1,inum),                            &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        fln_tce%icount_fline(inum), fln_tce%trace_length(inum),   &
     &        fln_tce%iflag_comm_start(inum), fline_lc, inum)
        end do
!
!        if(fln_tce%num_current_fline .gt. 4096) then
!          call s_trace_data_send_recv(fln_prm, fln_tce, fln_SR,        &
!     &                                m_SR%SR_sig, nline)
!        else
          call s_broadcast_trace_data(fln_prm, fln_tce,                &
     &                                 fln_bcast, nline)
!        end if
!
       if(nline .le. 0) exit
      end do
!
      end subroutine const_field_line
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
