!>@file   trace_particle_in_element.f90
!!       module trace_particle_in_element
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_extend_field_line                                  &
!!     &         (node, ele, surf, nod_fld, viz_fields,                 &
!!     &          end_trace, iflag_used_ele, iflag_dir,                 &
!!     &          i_fline, isurf_org, x4_start, v4_start,               &
!!     &          c_field, iflag_comm)
!!        real(kind = kreal), intent(inout) :: dt
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) ::   end_trace
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: isurf_org(3)
!!        integer(kind = kint), intent(inout) :: iflag_comm
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout) ::   c_field(1)
!!@endverbatim
!
      module trace_particle_in_element
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_phys_data
      use t_ctl_params_viz_fields
!
      implicit  none
!
      integer(kind = kint), parameter, private :: iflag_forward = 0
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_particle_in_element                            &
     &         (dt, node, surf, nod_fld, v_prev, viz_fields,            &
     &          i_fline, isurf_org, x4_start, v4_start, c_field,        &
     &          iflag_comm)
!
      use t_local_fline
      use trace_in_element
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in) :: i_fline
!
      integer(kind = kint), intent(inout) :: isurf_org(2)
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                    :: c_field(viz_fields%ntot_color_comp)
!
      real(kind = kreal), intent(inout) :: dt
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: end_trace, trace_length, trace_pre
      real(kind = kreal) :: flux, p_ratio, vmag
!
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org

!
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if
!
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
!
      iflag_comm = 0
      do
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
!   extend in the middle of element
        vmag = sqrt(v4_start(1)*v4_start(1) + v4_start(2)*v4_start(2)   &
     &             + v4_start(3)*v4_start(3))
        end_trace = vmag * dt
        trace_pre = zero
        call s_trace_in_element(half, end_trace, trace_length,          &
     &      iele, isf_org, iflag_forward, node, surf, nod_fld,          &
     &      v_prev, viz_fields, isurf_end, isf_tgt,                     &
     &      x4_start, v4_start, c_field, iflag_comm)
        if(iflag_comm .eq. -1) return
        if(trace_length.ge.end_trace) return

        p_ratio = (trace_length - trace_pre) / (vmag * dt)
!$parallel workshare
        v_prev(1:nod_fld%n_point,1)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,1)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline)
        v_prev(1:nod_fld%n_point,3)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,2)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline+1)
        v_prev(1:nod_fld%n_point,3)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,3)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline+2)
!$end parallel workshare
        dt = (one - p_ratio) * dt
!
!   extend to surface of element
        vmag = sqrt(v4_start(1)*v4_start(1) + v4_start(2)*v4_start(2)   &
     &             + v4_start(3)*v4_start(3))
        end_trace = vmag * dt
        trace_pre = zero
        call s_trace_in_element(one, end_trace, trace_length,           &
     &      iele, izero, iflag_forward, node, surf, nod_fld,            &
     &      v_prev, viz_fields, isurf_end, isf_tgt,                     &
     &      x4_start, v4_start, c_field, iflag_comm)
        if(iflag_comm .eq. -1) return
        if(trace_length.ge.end_trace) return

        p_ratio = (trace_length - trace_pre) / (vmag * dt)
!$parallel workshare
        v_prev(1:nod_fld%n_point,1)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,1)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline)
        v_prev(1:nod_fld%n_point,3)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,2)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline+1)
        v_prev(1:nod_fld%n_point,3)                                     &
     &       = (one - p_ratio) * v_prev(1:nod_fld%n_point,3)            &
     &        + p_ratio * nod_fld%d_fld(1:nod_fld%n_point, i_fline+2)
!$end parallel workshare
        dt = (one - p_ratio) * dt
!
        flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)              &
     &        + v4_start(2) * surf%vnorm_surf(isurf_end,2)              &
     &        + v4_start(3) * surf%vnorm_surf(isurf_end,3))             &
     &         * dble(surf%isf_4_ele(iele,isf_tgt) / isurf_end)         &
     &         *(-one)**iflag_forward
!
        if(surf%interior_surf(isurf_end) .eq. izero) then
          isurf_org(1) = iele
          isurf_org(2) = isf_tgt
          iflag_comm = 1
          exit
        end if
!
!   set backside element and surface 
!
        if(flux.ge.zero) then
          if(surf%isf_4_ele(iele,isf_tgt) .lt. 0) then
            isurf_org(1) = surf%iele_4_surf(isurf_end,1,1)
            isurf_org(2) = surf%iele_4_surf(isurf_end,1,2)
          else
            isurf_org(1) = surf%iele_4_surf(isurf_end,2,1)
            isurf_org(2) = surf%iele_4_surf(isurf_end,2,2)
          end if
        else
          iflag_comm = -2
          exit
        end if
      end do
!
      end subroutine s_trace_particle_in_element
!
!  ---------------------------------------------------------------------
!
      end module trace_particle_in_element

