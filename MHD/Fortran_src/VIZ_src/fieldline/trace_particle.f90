!>@file   trace_particle.f90
!!@brief  module trace_particle
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_trace_particle(dt_init, node, ele, surf, para_surf,&
!!     &          nod_fld, fln_prm, fln_tce, fln_bcast, v_prev)
!!        real(kind = kreal), intent(in) :: dt_init
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
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
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_paralell_surface_indices
      use t_tracing_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_mesh_SR
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_particle(dt_init, mesh, para_surf, nod_fld,    &
     &          fln_prm, fln_tce, fline_lc, fln_SR, fln_bcast,          &
     &          v_prev, m_SR)
!
      use transfer_to_long_integers
      use trace_particle_in_element
      use set_fline_seeds_from_list
      use copy_field_smp
!
      real(kind = kreal), intent(in) :: dt_init
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
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
      integer(kind = kint) :: nline, inum
!
!
      dt = dt_init
      call return_to_trace_list(fln_prm, fline_lc, fln_tce)

      call reset_fline_start(fline_lc)
      do
        do inum = 1, fln_tce%num_current_fline
          call s_trace_particle_in_element                              &
     &       (mesh%node, mesh%ele, mesh%surf, para_surf, nod_fld,       &
     &        v_prev, fln_prm%fline_fields, fln_prm%iphys_4_fline,      &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%isf_dbl_start(1,inum),                            &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        dt, fln_tce%iflag_comm_start(inum), inum)
!
          if(fln_tce%iflag_comm_start(inum) .eq. -3) then
            call set_field_at_each_seed_point(mesh%node, mesh%ele,      &
     &          nod_fld, fln_prm%fline_fields, fln_prm%iphys_4_fline,   &
     &          fln_tce%isf_dbl_start(2,inum),                          &
     &          fln_tce%xx_fline_start(1,inum),                         &
     &          fln_tce%v_fline_start(1,inum),                          &
     &          fln_tce%c_fline_start(1,inum))
            fln_tce%iflag_comm_start(inum) = 0
          end if
!
          if(fln_tce%iflag_comm_start(inum) .eq. 0) then
            call add_traced_list(fln_tce%isf_dbl_start(1,inum),         &
     &                         fln_tce%xx_fline_start(1,inum),          &
     &                         fln_tce%v_fline_start(1,inum),           &
     &                         fln_prm%fline_fields%ntot_color_comp,    &
     &                         fln_tce%c_fline_start(1,inum),           &
     &                         fline_lc)
          end if
        end do
!
        if(fln_tce%num_current_fline .gt. 4096) then
          call s_trace_data_send_recv(fln_prm, fln_tce, fln_SR,         &
     &                                m_SR%SR_sig, nline)
        else
          call s_broadcast_trace_data(fln_prm, fln_tce,                 &
     &                                fln_bcast, nline)
        end if
        if(nline .le. 0) exit
      end do
!
!$omp parallel
      call copy_nod_vector_smp(nod_fld%n_point,                        &
     &    nod_fld%d_fld(1,fln_prm%iphys_4_fline), v_prev)
!$omp end parallel
!
      end subroutine s_trace_particle
!
!  ---------------------------------------------------------------------
!
      subroutine add_traced_list(isf_dbl_start, xx4_add, v4_add,        &
     &                           ntot_comp, col_add, fline_lc)
!
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: isf_dbl_start(3)
      real(kind = kreal), intent(in) :: xx4_add(4), v4_add(4)
      real(kind = kreal), intent(in) :: col_add(ntot_comp)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l) = isf_dbl_start(2)
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l) = isf_dbl_start(3)
!
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%v_line_l(1:3,fline_lc%nnod_line_l) =  v4_add(1:3)
      fline_lc%col_line_l(1:ntot_comp,fline_lc%nnod_line_l)             &
     &      = col_add(1:ntot_comp)
!
      end subroutine add_traced_list
!
!  ---------------------------------------------------------------------
!
      subroutine return_to_trace_list(fln_prm, fline_lc, fln_tce)
!
      use calypso_mpi_int
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(local_fieldline), intent(in) :: fline_lc
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ip, ntot_comp
!
      ntot_comp = fln_prm%fline_fields%ntot_color_comp
      fln_tce%num_current_fline = fline_lc%nnod_line_l
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%istack_current_fline(ip)
      end do
!
!
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      do i = 1, fln_tce%num_current_fline
        fln_tce%xx_fline_start(1:3,i) = fline_lc%xx_line_l(1:3,i)
        fln_tce%v_fline_start(1:3,i) = fline_lc%v_line_l(1:3,i)
        fln_tce%c_fline_start(1:ntot_comp,i)                            &
     &                = fline_lc%col_line_l(1:ntot_comp,i)
      end do
      do i = 1, fln_tce%num_current_fline
        fln_tce%isf_dbl_start(1,i) =    my_rank
        fln_tce%isf_dbl_start(2:3,i) =  fline_lc%iedge_line_l(1:2,i)
      end do
!
      end subroutine return_to_trace_list
!
!  ---------------------------------------------------------------------
!
      subroutine local_tracer_from_seeds(fln_prm, fln_tce, fline_lc)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: i, ntot_comp
!
!
      ntot_comp = fln_prm%fline_fields%ntot_color_comp
!
      fline_lc%nnod_line_l = fln_tce%num_current_fline
      fline_lc%nele_line_l = fln_tce%num_current_fline
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
        call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      do i = 1, fln_tce%num_current_fline
        fline_lc%xx_line_l(1:3,i) = fln_tce%xx_fline_start(1:3,i)
        fline_lc%v_line_l(1:3,i)  = fln_tce%v_fline_start(1:3,i)
        fline_lc%col_line_l(1:ntot_comp,i)                              &
     &       = fln_tce%c_fline_start(1:ntot_comp,i)
!
        fline_lc%iedge_line_l(1,i) = fln_tce%isf_dbl_start(2,i)
        fline_lc%iedge_line_l(2,i) = fln_tce%isf_dbl_start(3,i)
      end do
!
      end subroutine local_tracer_from_seeds
!
!  ---------------------------------------------------------------------
!
      end module trace_particle
