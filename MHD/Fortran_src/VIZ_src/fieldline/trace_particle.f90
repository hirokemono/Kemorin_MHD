!>@file   trace_particle.f90
!!@brief  module trace_particle
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_trace_particle(dt, elps_tracer, mesh, para_surf,   &
!!     &          nod_fld, fln_prm, fln_tce, fline_lc,                  &
!!     &          fln_SR, fln_bcast, v_prev, m_SR)
!!        real(kind = kreal), intent(in) :: dt
!!        type(elapsed_lables), intent(in) :: elps_tracer
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!
!!      subroutine local_tracer_from_seeds(ist_smp, ied_smp,            &
!!     &                                   fln_tce, perticle_smp)
!!      integer(kind = kint), intent(in) :: ist_smp, ied_smp
!!      type(each_fieldline_trace), intent(in) :: fln_tce
!!      type(local_fieldline), intent(inout) :: perticle_smp
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
      use m_work_time
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_parallel_surface_indices
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
      subroutine s_trace_particle(dt, elps_tracer, mesh, para_surf,     &
     &          nod_fld, fln_prm, fln_tce, fline_lc,                    &
     &          fln_SR, fln_bcast, v_prev, m_SR)
!
      use t_find_interpolate_in_ele
      use transfer_to_long_integers
      use trace_particle_in_element
      use set_fline_seeds_from_list
      use copy_field_smp
!
      real(kind = kreal), intent(in) :: dt
      type(elapsed_lables), intent(in) :: elps_tracer
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      type(mesh_SR), intent(inout) :: m_SR
!
      type(cal_interpolate_coefs_work) :: itp_ele_work_f
      integer(kind = kint) :: nline, inum
!
!
      call return_to_trace_list(fln_prm, fline_lc, fln_tce)
      fln_tce%trace_length(1:fln_tce%num_current_fline) = 0.0d0

      call alloc_work_4_interpolate(mesh%ele%nnod_4_ele,                &
     &                              itp_ele_work_f)
      call reset_fline_start(fline_lc)
      do
        if(elps_tracer%flag_elapsed)                                    &
     &         call start_elapsed_time(elps_tracer%ist_elapsed+1)
        do inum = 1, fln_tce%num_current_fline
          call s_trace_particle_in_element                              &
     &       (dt, mesh%node, mesh%ele, mesh%surf, para_surf, nod_fld,   &
     &        v_prev, fln_prm%iphys_4_fline,                            &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%isf_dbl_start(1,inum),                            &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%xi_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,1),                               &
     &        fln_tce%trace_length(inum),                               &
     &        fln_tce%iflag_comm_start(inum), itp_ele_work_f, inum)
!
          if(fln_tce%iflag_comm_start(inum) .eq. -3) then
            fln_tce%iflag_comm_start(inum) = 0
          else if(fln_tce%iflag_comm_start(inum) .eq. 0) then
            call set_veclocity_at_each_tracer                           &
     &         (mesh%node, mesh%ele, nod_fld, fln_prm%iphys_4_fline,    &
     &          fln_tce%isf_dbl_start(2,inum),                          &
     &          fln_tce%xx_fline_start(1,inum),                         &
     &          fln_tce%xi_fline_start(1,inum),                         &
     &          fln_tce%v_fline_start(1,1), itp_ele_work_f)
!
            call add_traced_list(fln_tce%iline_original(inum),          &
     &                           fln_tce%isf_dbl_start(1,inum),         &
     &                           fln_tce%xx_fline_start(1,inum),        &
     &                           fln_tce%v_fline_start(1,1),            &
     &                           fline_lc)
          end if
        end do
        call dealloc_work_4_interpolate(itp_ele_work_f)
        if(elps_tracer%flag_elapsed)                                    &
     &          call end_elapsed_time(elps_tracer%ist_elapsed+1)
!
         write(*,*) 'check at', inum
        call check_tracer_restarts                                  &
     &    (inum, mesh, nod_fld, fln_prm, fline_lc)
!
        if(elps_tracer%flag_elapsed)                                    &
     &         call start_elapsed_time(elps_tracer%ist_elapsed+2)
        if(fln_prm%flag_use_broadcast) then
          call s_broadcast_trace_data(fln_prm, fln_tce,                 &
     &                                fln_bcast, nline)
        else
          call s_trace_data_send_recv(fln_prm, fln_tce, fln_SR,         &
     &                                m_SR%SR_sig, nline)
        end if
        if(elps_tracer%flag_elapsed)                                    &
     &          call end_elapsed_time(elps_tracer%ist_elapsed+2)
!
        if(nline .le. 0) exit
      end do
!
      call copy_nod_vector_smp(nod_fld%n_point,                        &
     &    nod_fld%d_fld(1,fln_prm%iphys_4_fline), v_prev)
!
      end subroutine s_trace_particle
!
!  ---------------------------------------------------------------------
!
      subroutine add_traced_list(iglobal_tracer, isf_dbl_start,         &
     &                           xx4_add, v4_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_tracer
      integer(kind = kint), intent(in) :: isf_dbl_start(3)
      real(kind = kreal), intent(in) :: xx4_add(4), v4_add(4)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l) = isf_dbl_start(2)
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l) = isf_dbl_start(3)
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_tracer
      fline_lc%xx_line_l(1:4,fline_lc%nnod_line_l) =   xx4_add(1:4)
      fline_lc%v_line_l(1:4,fline_lc%nnod_line_l) =    v4_add(1:4)
!
      end subroutine add_traced_list
!
!  ---------------------------------------------------------------------
!
      subroutine return_to_trace_list(fln_prm, fline_lc, fln_tce)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(local_fieldline), intent(in) :: fline_lc
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ip, ntot_comp, max_4_smp
!
!
      ntot_comp = fln_prm%fline_fields%ntot_color_comp
      call count_parallel_current_fline(fline_lc%nnod_line_l, fln_tce)
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      do i = 1, fln_tce%num_current_fline
        fln_tce%iline_original(i) =     fline_lc%iglobal_fline(i)
        fln_tce%xx_fline_start(1:4,i) = fline_lc%xx_line_l(1:4,i)
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
      subroutine local_tracer_from_seeds(ist_smp, ied_smp,              &
     &                                   fln_tce, perticle_smp)
!
      integer(kind = kint), intent(in) :: ist_smp, ied_smp
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(local_fieldline), intent(inout) :: perticle_smp
!
      integer(kind = kint) :: i, num
!
!
      num = ied_smp - ist_smp
      perticle_smp%nnod_line_l = num
      perticle_smp%nele_line_l = num
      if(perticle_smp%nele_line_l .ge. perticle_smp%nele_line_buf) then
        call raise_local_fline_connect(perticle_smp)
      end if
      if(perticle_smp%nnod_line_l .ge. perticle_smp%nnod_line_buf) then
        call raise_local_fline_data(perticle_smp)
      end if
!
      do i = 1, num
        perticle_smp%iglobal_fline(i)                                   &
     &     = fln_tce%iline_original(i+ist_smp)
        perticle_smp%xx_line_l(1:4,i)                                   &
     &     = fln_tce%xx_fline_start(1:4,i+ist_smp)
!
        perticle_smp%iedge_line_l(1,i)                                  &
     &     = fln_tce%isf_dbl_start(2,i+ist_smp)
        perticle_smp%iedge_line_l(2,i)                                  &
     &     = fln_tce%isf_dbl_start(3,i+ist_smp)
      end do
!
      end subroutine local_tracer_from_seeds
!
!  ---------------------------------------------------------------------
!
      end module trace_particle
