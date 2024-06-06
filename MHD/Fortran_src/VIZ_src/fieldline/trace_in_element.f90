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
!!     &         (trace_ratio, dt, isf_org, ele, surf, viz_fields,      &
!!     &          x4_ele, v4_pre, v4_ele, c_ele,                        &
!!     &          isf_tgt, x4_start, v4_start, c_field, progress) 
!!      subroutine fline_trace_in_element                               &
!!     &         (trace_ratio, end_trace, trace_length,                 &
!!     &          isf_org, iflag_dir, ele, surf,                        &
!!     &          v_trace, viz_fields, isurf_end, isf_tgt,              &
!!     &          x4_start, v4_start, c_field, iflag_comm)
!!        real(kind = kreal), intent(in) :: trace_ratio
!!        real(kind = kreal), intent(in) :: dt
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
!!      subroutine fline_vector_at_one_element(iele, node, ele, v_trace,&
!!     &                                      v4_ele)
!!      subroutine fline_colors_at_one_element(iele, ele, nod_fld,      &
!!     &                                       viz_fields, c_ele)
!!        integer(kind = kint), intent(in) :: iele
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!!        real(kind = kreal), intent(inout) :: x4_ele(4,ele%nnod_4_ele)
!!        real(kind = kreal), intent(inout) :: v4_ele(4,ele%nnod_4_ele)
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
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
     &         (trace_ratio, dt, isf_org, ele, surf, viz_fields,        &
     &          x4_ele, v4_pre, v4_ele, c_ele,                          &
     &          isf_tgt, x4_start, v4_start, c_field, progress)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use tracer_field_interpolate
!
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: isf_org
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_pre(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
      real(kind = kreal), intent(inout) :: progress
!
      real(kind = kreal) :: v4_current_e(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: ratio
!
!
      if((v4_start(1)**2+v4_start(2)**2+v4_start(3)**2) .le. zero) then
        isf_tgt = -3
        return
      end if
!
!$omp parallel workshare
      v4_current_e(1:4,1:ele%nnod_4_ele)                                &
     &   = (one - progress) * v4_pre(1:4,1:ele%nnod_4_ele)              &
     &           + progress * v4_ele(1:4,1:ele%nnod_4_ele)
!$omp end parallel workshare
!
      call trace_to_element_wall                                        &
     &   (isf_org, iflag_forward_line, ele, surf,                       &
     &    viz_fields, x4_ele, v4_current_e, c_ele, x4_start, v4_start,  &
     &    isf_tgt, x4_tgt, v4_tgt, c_tgt)
!
      call ratio_of_trace_to_wall_tracer(trace_ratio,                   &
     &    v4_start, x4_tgt, x4_start, dt, ratio, progress)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt, v4_tgt, c_tgt,                 &
     &                           x4_start, v4_start, c_field)
!
      end subroutine s_trace_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine fline_trace_in_element                                 &
     &         (trace_ratio, end_trace, trace_length,                   &
     &          isf_org, iflag_dir, ele, surf,                          &
     &          viz_fields, x4_ele, v4_ele, c_ele,                      &
     &          isf_tgt, x4_start, v4_start, c_field)
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
      integer(kind = kint), intent(in) :: isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: x4_start(4)
      real(kind = kreal), intent(inout) :: v4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_field(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: v4_tgt(4), x4_tgt_8(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: ratio
!
!
      if((v4_start(1)**2+v4_start(2)**2+v4_start(3)**2) .le. zero) then
        isf_tgt = -3
        return
      end if
!
      call trace_to_element_wall(isf_org, iflag_dir, ele, surf,         &
     &    viz_fields, x4_ele, v4_ele, c_ele, x4_start, v4_start,        &
     &    isf_tgt, x4_tgt_8, v4_tgt, c_tgt)
      if(isf_tgt .le. 0) return
!
      call ratio_of_trace_to_wall_fline(end_trace, trace_ratio,         &
     &                                  x4_tgt_8, x4_start,             &
     &                                  ratio, trace_length)
      call update_fline_position(ratio, viz_fields%ntot_color_comp,     &
     &                           x4_tgt_8, v4_tgt, c_tgt,               &
     &                           x4_start, v4_start, c_field)
!
      end subroutine fline_trace_in_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine trace_to_element_wall                                  &
     &         (isf_org, iflag_dir, ele, surf, viz_fields,              &
     &          x4_ele, v4_ele, c_ele, x4_start, v4_start,              &
     &          isf_tgt_8, x4_tgt_8, v4_tgt_8, c_tgt_8)
!
      use cal_fline_in_cube
!
      integer(kind = kint), intent(in) :: isf_org
      integer(kind = kint), intent(in) :: iflag_dir
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      real(kind = kreal), intent(in) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt_8
      real(kind = kreal), intent(in) :: x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
!
      real(kind = kreal), intent(inout) :: x4_tgt_8(4)
      real(kind = kreal), intent(inout) :: v4_tgt_8(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt_8(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: xi_surf_8(2)
!

      call find_line_end_in_ele_8(iflag_dir, isf_org,                   &
     &    ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,            &
     &    v4_start, x4_start, x4_ele, isf_tgt_8, x4_tgt_8, xi_surf_8)
!
      call fields_on_surf_from_one_ele                                  &
     &   (isf_tgt_8, xi_surf_8, ele, surf, viz_fields,                  &
     &    v4_ele, c_ele, x4_tgt_8, v4_tgt_8, c_tgt_8)
!
      end subroutine trace_to_element_wall
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ratio_of_trace_to_wall_tracer(trace_ratio,             &
     &          v4_start, x4_tgt, x4_start, dt, ratio, progress)

      real(kind = kreal), intent(in) :: x4_tgt(4), x4_start(4)
      real(kind = kreal), intent(in) :: v4_start(4)
      real(kind = kreal), intent(in) :: dt, trace_ratio
      real(kind = kreal), intent(inout) :: ratio, progress
!
      real(kind = kreal) :: trip, dl, actual
!
      dl = dt * sqrt(v4_start(1) * v4_start(1)                          &
     &            +  v4_start(2) * v4_start(2)                          &
     &            +  v4_start(3) * v4_start(3))                         &
     &        * (one - progress)
      trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1))   &
     &         + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))    &
     &         + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
!
      actual = trace_ratio * min(trip, dl)
      ratio =  actual / trip
      progress = progress + (one - progress) * actual / dl
!
      end subroutine ratio_of_trace_to_wall_tracer
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
!  ---------------------------------------------------------------------
!
      subroutine fline_vector_at_one_element(iele, node, ele, v_trace,  &
     &                                      v4_ele)
!
      integer(kind = kint), intent(in) :: iele
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: v_trace(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: v4_ele(4,ele%nnod_4_ele)
!
      integer(kind = kint) :: k1, inod
!
      do k1 = 1, ele%nnod_4_ele
        inod = ele%ie(iele,k1)
        v4_ele(1:3,k1) = v_trace(inod,1:3)
        v4_ele(4,k1) = one
      end do
!
      end subroutine fline_vector_at_one_element
!
!  ---------------------------------------------------------------------
!
      subroutine fline_colors_at_one_element(iele, ele, nod_fld,        &
     &                                       viz_fields, c_ele)
!
      use tracer_field_interpolate
!
      integer(kind = kint), intent(in) :: iele
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      integer(kind = kint) :: k1, inod
!
      do k1 = 1, ele%nnod_4_ele
        inod = ele%ie(iele,k1)
        call cal_xyz_fields_at_node(inod, nod_fld, viz_fields,          &
     &                              c_ele(1,k1))
      end do
!
      end subroutine fline_colors_at_one_element
!
!  ---------------------------------------------------------------------
!
      subroutine fields_on_surf_from_one_ele                            &
     &         (isf_tgt, xi_surf, ele, surf, viz_fields,                &
     &          v4_ele, c_ele, x4_tgt, v4_tgt, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isf_tgt
      real(kind = kreal), intent(in) :: xi_surf(2)
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(in) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal), intent(in)                                    &
     &           :: c_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
!
      real(kind = kreal), intent(in) :: x4_tgt(4)
      real(kind = kreal), intent(inout) :: v4_tgt(4)
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: c_xyz(viz_fields%ntot_org_comp)
      real(kind = kreal) :: v_work(4*surf%nnod_4_surf)
      real(kind = kreal) :: c_work(viz_fields%ntot_org_comp*surf%nnod_4_surf)
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      integer(kind = kint) :: istack_single(0:1) = (/0, 1/)
      integer(kind = kint) :: inum, ist, jst
!
!
      call field_on_surf_of_one_ele(isf_tgt, ele, surf, xi_surf,        &
     &    ifour, v4_ele(1,1), v4_tgt(1), v_work(1))
      call field_on_surf_of_one_ele(isf_tgt, ele, surf, xi_surf,        &
     &    viz_fields%ntot_org_comp, c_ele(1,1), c_xyz(1), c_work(1))
!
      call position_2_sph(ione, x4_tgt(1), r, theta, phi,               &
     &                    a_r, rs, a_rs)
      do inum = 1, viz_fields%num_color_fields
        ist = viz_fields%istack_org_ncomp(inum-1)
        jst = viz_fields%istack_color_field(inum-1)
        call convert_comps_4_viz                                        &
     &     (ione, istack_single, x4_tgt(1), r, a_r, rs, a_rs,           &
     &      viz_fields%ncomp_color_field(inum),                         &
     &      viz_fields%ncomp_org_color_field(inum),                     &
     &      viz_fields%icomp_color_field(inum),                         &
     &      c_xyz(ist+1), c_tgt(jst+1))
      end do
!
      end subroutine fields_on_surf_from_one_ele
!
!  ---------------------------------------------------------------------
!
      subroutine field_on_surf_of_one_ele(isf_in_ele, ele, surf, xi,    &
     &                                    ncomp, v_ele, v_tgt, v_work)
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isf_in_ele
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      real(kind = kreal), intent(in) :: xi(2)
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: v_ele(ncomp,ele%nnod_4_ele)
!
      real(kind = kreal), intent(inout) :: v_tgt(ncomp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_work(ncomp,surf%nnod_4_surf)
!
      integer(kind = kint) :: k1, inod_lc
!
!
      do k1 = 1, 4
        inod_lc = surf%node_on_sf(k1,isf_in_ele)
        v_work(1:ncomp,k1) = v_ele(1:ncomp,inod_lc)
      end do
!
      call cal_surf_field_value_2d(ncomp, xi, v_work, v_tgt)
!
      end subroutine field_on_surf_of_one_ele
!
!  ---------------------------------------------------------------------
!
      end module trace_in_element

