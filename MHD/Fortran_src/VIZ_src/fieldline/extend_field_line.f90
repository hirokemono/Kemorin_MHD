!>@file  extend_field_line.f90
!!       module extend_field_line
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_extend_field_line                                  &
!!     &         (node, ele, surf, nod_fld, viz_fields, max_line_step,  &
!!     &          end_trace, iflag_used_ele, iflag_dir,                 &
!!     &          i_fline, isurf_org, x4_start, v4_start,               &
!!     &          c_field, icount_line, iflag_comm, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) ::   end_trace
!!        integer(kind = kint), intent(in) :: iflag_dir, max_line_step
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: isurf_org(3)
!!        integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout) ::   c_field(1)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine cal_fields_on_line(isurf, xi_surf, xyz_surf,         &
!!     &                              surf, nod_fld, viz_fields, c_tgt)
!!      subroutine cal_fields_in_element(iele, xi_cube, xyz, ele,       &
!!     &                                 nod_fld, viz_fields, c_tgt)
!!        integer(kind = kint), intent(in) :: isurf
!!        real(kind = kreal), intent(in) :: xi_surf(2)
!!        real(kind = kreal), intent(in) :: xyz_surf(3)
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in) :: iele(1)
!!        real(kind = kreal), intent(in) :: xi_cube(3)
!!        real(kind = kreal), intent(in) :: xyz(3)
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_tgt(viz_fields%ntot_color_comp)
!!@endverbatim
!
      module extend_field_line
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
      subroutine s_extend_field_line                                    &
     &         (node, ele, surf, nod_fld, viz_fields, max_line_step,    &
     &          end_trace, iflag_used_ele, iflag_dir,                   &
     &          i_fline, isurf_org, x4_start, v4_start, c_field,        &
     &          icount_line, trace_length, iflag_comm, fline_lc)
!
      use t_local_fline
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in) :: i_fline
!
      real(kind = kreal), intent(in) ::   end_trace
      integer(kind = kint), intent(in) :: iflag_dir, max_line_step
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!
      integer(kind = kint), intent(inout) :: isurf_org(2)
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                    :: c_field(viz_fields%ntot_color_comp)
!
      type(local_fieldline), intent(inout) :: fline_lc
      real(kind = kreal), intent(inout) :: trace_length
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!
      real(kind = kreal) :: flux
!
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org

!
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if
!
      call add_fline_start(x4_start, viz_fields%ntot_color_comp,        &
     &                     c_field(1), fline_lc)
!
      iflag_comm = 0
      do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
!   extend in the middle of element
        call trace_in_element(half, end_trace, trace_length, &
     &      iele, isf_org, i_fline, iflag_dir,   &
     &      node, surf, nod_fld, viz_fields, isurf_end, isf_tgt, &
     &      x4_start, v4_start, c_field, iflag_comm)
        if(iflag_comm .eq. -1) return
        call add_fline_list(x4_start, viz_fields%ntot_color_comp,       &
     &                       c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) return
!
!   extend to surface of element
        call trace_in_element(one, end_trace, trace_length, &
     &      iele, izero, i_fline, iflag_dir,   &
     &      node, surf, nod_fld, viz_fields, isurf_end, isf_tgt, &
     &      x4_start, v4_start, c_field, iflag_comm)
        if(iflag_comm .eq. -1) return
        call add_fline_list(x4_start, viz_fields%ntot_color_comp,       &
     &                      c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) return
!
        flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)              &
     &        + v4_start(2) * surf%vnorm_surf(isurf_end,2)              &
     &        + v4_start(3) * surf%vnorm_surf(isurf_end,3))             &
     &         * dble(surf%isf_4_ele(iele,isf_tgt) / isurf_end)         &
     &         *(-one)**iflag_dir
!
!         write(60+my_rank,'(a6,i8,1p4e16.7)')  'x_tgt: ', icount_line, &
!     &          v4_start(1:4), flux
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
!
!         write(70+my_rank,*) 'isurf_end', icount_line, iele, isf_tgt,  &
!     &                        surf%isf_4_ele(iele,isf_tgt)
!         write(70+my_rank,*) 'isurf_nxt', icount_line, isurf_org(1:2), &
!     &                        surf%isf_4_ele(isurf_org(1),isurf_org(2))
!
        if(isurf_org(1).eq.0 .or.  iflag_used_ele(iele).eq.0) then
          if(max_line_step .gt. 0                                       &
     &          .and. icount_line.gt.max_line_step) then
            iflag_comm = 0
            exit
          else
            isurf_org(1) = iele
            isurf_org(2) = isf_tgt
            iflag_comm = 1
            write(*,*) 'complete extend within internal'
          end if
        end if
      end do
!
      end subroutine s_extend_field_line
!
!  ---------------------------------------------------------------------
!
      subroutine trace_in_element(trace_ratio, end_trace, trace_length, &
     &          iele, isf_org, iphys_fline, iflag_dir,   &
     &          node, surf, nod_fld, viz_fields, &
     &          isurf_end, isf_tgt, x4_start, v4_start, c_field, iflag_comm)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      real(kind = kreal), intent(in) :: trace_ratio
      real(kind = kreal), intent(in) ::   end_trace
      real(kind = kreal), intent(inout) :: trace_length
!
      integer(kind = kint), intent(in) :: iele, isf_org
      integer(kind = kint), intent(in) :: iphys_fline, iflag_dir
!
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
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: c_tgt(viz_fields%ntot_color_comp)
      real(kind = kreal) :: xi_surf(2), flux, trip, ratio
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
     &    isurf_end, xi_surf, nod_fld%d_fld(1,iphys_fline), v4_tgt)
      call cal_fields_on_line(isurf_end, xi_surf, x4_tgt,               &
     &                        surf, nod_fld, viz_fields, c_tgt)
!
!
        trip = sqrt((x4_tgt(1)-x4_start(1)) * (x4_tgt(1) - x4_start(1)) &
     &           + (x4_tgt(2)-x4_start(2)) * (x4_tgt(2) - x4_start(2))  &
     &           + (x4_tgt(3)-x4_start(3)) * (x4_tgt(3) - x4_start(3)))
        trace_length = trace_length + trace_ratio * trip
        if(trace_length .ge. end_trace .and. end_trace .gt. zero) then
           ratio = (trace_length - end_trace) / trip
           trace_length = end_trace
         else
           ratio = one - trace_ratio
         end if
!
         x4_start(1:4) = ratio * x4_start(1:4)                          &
     &                 + (one - ratio) * x4_tgt(1:4)
         v4_start(1:4) = ratio * v4_start(1:4)                          &
     &                 + (one - ratio) * v4_tgt(1:4)
         c_field(1:viz_fields%ntot_color_comp)                          &
     &        =  ratio * c_field(1:viz_fields%ntot_color_comp)          &
     &         + (one - ratio) * c_tgt(1:viz_fields%ntot_color_comp)
!
      end subroutine trace_in_element
!
!  ---------------------------------------------------------------------
!
      subroutine cal_fields_on_line(isurf, xi_surf, xyz_surf,           &
     &                              surf, nod_fld, viz_fields, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isurf
      real(kind = kreal), intent(in) :: xi_surf(2)
      real(kind = kreal), intent(in) :: xyz_surf(3)
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      real(kind = kreal) :: c_xyz(9)
      integer(kind = kint) :: istack_single(0:1) = (/0, 1/)
!
      integer(kind = kint) :: inum, ifield, ist, jst, nd
!
      call position_2_sph(ione, xyz_surf(1), r, theta, phi,             &
     &                    a_r, rs, a_rs )
      do inum = 1, viz_fields%num_color_fields
        ifield = viz_fields%ifleld_color_field(inum)
        if(ifield .le. 0) then
          jst = viz_fields%istack_color_field(inum-1)
          c_tgt(jst+1) = zero
        else
          ist = nod_fld%istack_component(ifield-1)
          jst = viz_fields%istack_color_field(inum-1)
          do nd = 1, viz_fields%ncomp_org_color_field(inum)
            call cal_field_on_surf_scalar(nod_fld%n_point,              &
     &          surf%numsurf, surf%nnod_4_surf, surf%ie_surf,           &
     &          isurf, xi_surf, nod_fld%d_fld(1,ist+nd), c_xyz(nd))
          end do
          call convert_comps_4_viz                                      &
     &       (ione, istack_single, xyz_surf(1), r, a_r, rs, a_rs,       &
     &        viz_fields%ncomp_color_field(inum),                       &
     &        viz_fields%ncomp_org_color_field(inum),                   &
     &        viz_fields%icomp_color_field(inum),                       &
     &         c_xyz(1), c_tgt(jst+1))
        end if
      end do
!
      end subroutine cal_fields_on_line
!
!  ---------------------------------------------------------------------
!
      subroutine cal_fields_in_element(iele, xi_cube, xyz, ele,         &
     &                                 nod_fld, viz_fields, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use sel_interpolate_scalar
!
      integer(kind = kint), intent(in) :: iele(1)
      real(kind = kreal), intent(in) :: xi_cube(3)
      real(kind = kreal), intent(in) :: xyz(3)
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      real(kind = kreal) :: c_xyz(9)
      integer(kind = kint), parameter                                   &
     &                   :: istack_tbl_wtype_smp(0:4) = (/0,0,0,0,1/)
      integer(kind = kint), parameter :: istack_single(0:1) = (/0,1/)
!
      integer(kind = kint) :: inum, ifield, ist, jst, nd
!
      call position_2_sph(ione, xyz(1), r, theta, phi,                  &
     &                    a_r, rs, a_rs )
      do inum = 1, viz_fields%num_color_fields
        ifield = viz_fields%ifleld_color_field(inum)
        if(ifield .le. 0) then
          jst = viz_fields%istack_color_field(inum-1)
          c_tgt(jst+1) = zero
        else
          ist = nod_fld%istack_component(ifield-1)
          jst = viz_fields%istack_color_field(inum-1)
          do nd = 1, viz_fields%ncomp_org_color_field(inum)
            call s_sel_interpolate_scalar_ele                           &
     &         (1, nod_fld%n_point, ele%numele, ele%nnod_4_ele, ele%ie, &
     &          nod_fld%d_fld(1,ist+nd), istack_single, ione,           &
     &          iele, xi_cube, c_xyz(nd))
          end do
          call convert_comps_4_viz                                      &
     &       (ione, istack_single, xyz(1), r, a_r, rs, a_rs,            &
     &        viz_fields%ncomp_color_field(inum),                       &
     &        viz_fields%ncomp_org_color_field(inum),                   &
     &        viz_fields%icomp_color_field(inum),                       &
     &         c_xyz(1), c_tgt(jst+1))
        end if
      end do
!
      end subroutine cal_fields_in_element
!
!  ---------------------------------------------------------------------
!
      end module extend_field_line

