!>@file  trace_in_element.f90
!!       module trace_in_element
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_trace_in_element                                   &
!!     &         (trace_ratio, iele, isf_org, node, surf, nod_fld,      &
!!     &          v_trace, viz_fields, isurf_end, isf_tgt,              &
!!     &          x4_start, v4_start, c_field, dt, iflag_comm)
!!      subroutine fline_trace_in_element                               &
!!     &         (trace_ratio, end_trace, trace_length,                 &
!!     &          iele, isf_org, iflag_dir, node, surf, nod_fld,        &
!!     &          v_trace, viz_fields, isurf_end, isf_tgt,              &
!!     &          x4_start, v4_start, c_field, iflag_comm)
!!        real(kind = kreal), intent(in) :: trace_ratio
!!        real(kind = kreal), intent(in) ::   end_trace
!!        real(kind = kreal), intent(inout) :: trace_length
!!        integer(kind = kint), intent(in) :: iele, isf_org
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        integer(kind = kint), intent(inout) :: isurf_end, isf_tgt
!!        real(kind = kreal), intent(inout) :: x4_start(4)
!!        real(kind = kreal), intent(inout) :: v4_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_field(viz_fields%ntot_color_comp)
!!        integer(kind = kint), intent(inout) :: iflag_comm
!!        real(kind = kreal), intent(inout) :: dt
!!@endverbatim
!
      module trace_in_element
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_in_element                                     &
     &         (trace_ratio, iele, isf_org, node, surf, nod_fld,        &
     &          viz_fields, isurf_end, isf_tgt, v_trace, i_tracer,      &
     &          x4_start, v4_start, c_field, dt, iflag_comm)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use tracer_field_interpolate
!
      real(kind = kreal), intent(in) :: trace_ratio
!
      integer(kind = kint), intent(in) :: iele, isf_org
      integer(kind = kint), intent(in) :: i_tracer
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      integer(kind = kint), intent(inout) :: isurf_end, isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
      real(kind = kreal), intent(inout) :: v_trace(node%numnod,3)
      real(kind = kreal), intent(inout) :: dt
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: xi_surf(2), ratio
      real(kind = kreal) :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
!
      call position_on_each_ele_surfs                                   &
     &   (surf, node%numnod, node%xx, iele, xx4_ele_surf)
      call find_line_end_in_1ele(iflag_forward_line,                    &
     &    isf_org, v4_start, x4_start, xx4_ele_surf,                    &
     &    isf_tgt, x4_tgt, xi_surf)
!
      if(isf_tgt .eq. 0) then
        iflag_comm = -1
        return
      end if
!
      isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
      call cal_field_on_surf_vect4                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,    &
     &    isurf_end, xi_surf, v_trace, v4_tgt)
      call cal_fields_on_line(isurf_end, xi_surf, x4_tgt,               &
     &                        surf, nod_fld, viz_fields, c_tgt)
!
      call ratio_of_trace_to_wall_tracer(trace_ratio, v4_start,         &
     &                                   x4_tgt, x4_start, ratio, dt)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt, v4_tgt, c_tgt,                 &
     &                           x4_start, v4_start, c_field)
      call velocity_at_tracer(ratio, node%numnod,                       &
     &                        nod_fld%d_fld(1,i_tracer), v_trace)
!
      end subroutine s_trace_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine fline_trace_in_element                                 &
     &         (trace_ratio, end_trace, trace_length,                   &
     &          iele, isf_org, iflag_dir, node, surf, nod_fld,          &
     &          v_trace, viz_fields, isurf_end, isf_tgt,                &
     &          x4_start, v4_start, c_field, iflag_comm)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use tracer_field_interpolate
!
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) ::   end_trace
      real(kind = kreal), intent(inout) :: trace_length
!
      integer(kind = kint), intent(in) :: iele, isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!
      integer(kind = kint), intent(inout) :: isurf_end, isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: xi_surf(2), ratio
      real(kind = kreal) :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
!
      call position_on_each_ele_surfs                                   &
     &   (surf, node%numnod, node%xx, iele, xx4_ele_surf)
      call find_line_end_in_1ele(iflag_dir,                             &
     &    isf_org, v4_start, x4_start, xx4_ele_surf,                    &
     &    isf_tgt, x4_tgt, xi_surf)
!
      if(isf_tgt .eq. 0) then
        iflag_comm = -1
        return
      end if
!
      isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
      call cal_field_on_surf_vect4                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,    &
     &    isurf_end, xi_surf, v_trace, v4_tgt)
      call cal_fields_on_line(isurf_end, xi_surf, x4_tgt,               &
     &                        surf, nod_fld, viz_fields, c_tgt)
!
      call ratio_of_trace_to_wall_fline(end_trace, trace_ratio,         &
     &                                  x4_tgt, x4_start,               &
     &                                  ratio, trace_length)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt, v4_tgt, c_tgt,                 &
     &                           x4_start, v4_start, c_field)
!
      end subroutine fline_trace_in_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ratio_of_trace_to_wall_tracer(trace_ratio, v4_start,   &
     &                                        x4_tgt, x4_start,         &
     &                                        ratio, dt)

      real(kind = kreal), intent(in) :: x4_tgt(4), x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(inout) :: ratio, dt
!
      real(kind = kreal) :: trip, dl
!
      dl = dt * sqrt(v4_start(1) * v4_start(1)                          &
     &            +  v4_start(2) * v4_start(2)                          &
     &            +  v4_start(3) * v4_start(3))
      trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1))   &
     &         + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))    &
     &         + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
!
      ratio = trace_ratio * min(one, trip/dl)
      dt = (one - ratio) * dt
!
      end subroutine ratio_of_trace_to_wall_tracer
!
!  ---------------------------------------------------------------------
!
      subroutine velocity_at_tracer(ratio, numnod, v_current, v_prev)

      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: ratio
      real(kind = kreal), intent(in) :: v_current(numnod,3)
      real(kind = kreal), intent(inout) :: v_prev(numnod,3)
!
!$parallel workshare
        v_prev(1:numnod,1) = (one - ratio) * v_prev(1:numnod,1)         &
     &                             + ratio * v_current(1:numnod, 1)
        v_prev(1:numnod,3) = (one - ratio) * v_prev(1:numnod,2)         &
     &                             + ratio * v_current(1:numnod, 2)
        v_prev(1:numnod,3) = (one - ratio) * v_prev(1:numnod,3)         &
     &                             + ratio * v_current(1:numnod, 3)
!$end parallel workshare
!
      end subroutine velocity_at_tracer
!
!  ---------------------------------------------------------------------
!
      subroutine ratio_of_trace_to_wall_fline(end_trace, trace_ratio,   &
     &                                        x4_tgt, x4_start,         &
     &                                        ratio, trace_length)

      real(kind = kreal), intent(in) :: x4_tgt(4), x4_start(4)
      real(kind = kreal), intent(in) :: end_trace
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(inout) :: ratio, trace_length
!
      real(kind = kreal) :: trip, rest_trace
!
!
      if(trace_length .ge. end_trace .and. end_trace .gt. zero) then
        rest_trace =  (end_trace - trace_length) 
        trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1)) &
     &           + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))  &
     &           + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
        ratio = min(rest_trace/trip, trace_ratio)
        trace_length = trace_length + (one - ratio) * trip
      else
        ratio = trace_ratio
      end if
!
      end subroutine ratio_of_trace_to_wall_fline
!
!  ---------------------------------------------------------------------
!
      subroutine update_fline_position(ratio, ntot_color_comp,          &
     &                                 x4_tgt, v4_tgt, c_tgt,           &
     &                                 x4_start, v4_start, c_field)
!
      real(kind = kreal), intent(in) :: ratio
!
      integer(kind = kint), intent(in) :: ntot_color_comp
      real(kind = kreal), intent(in) :: x4_tgt(4), v4_tgt(4)
      real(kind = kreal), intent(in) :: c_tgt(ntot_color_comp)
!
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(ntot_color_comp)
!
       x4_start(1:4) = ratio * x4_tgt(1:4)                              &
     &               + (one - ratio) * x4_start(1:4)
       v4_start(1:4) = ratio * v4_tgt(1:4)                              &
     &               + (one - ratio) * v4_start(1:4)
       c_field(1:ntot_color_comp) =  ratio * c_tgt(1:ntot_color_comp)   &
     &                     + (one - ratio) * c_field(1:ntot_color_comp)
!
      end subroutine update_fline_position
!
!  ---------------------------------------------------------------------
!
      end module trace_in_element

