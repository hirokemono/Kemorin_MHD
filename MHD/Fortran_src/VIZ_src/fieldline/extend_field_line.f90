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
!!     &          end_trace, iflag_used_ele, iflag_dir, i_fline,        &
!!     &          isurf_org_dbl, x4_start, v4_start, c_field,           &
!!     &          c_field, icount_line, iflag_comm, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(in) ::   end_trace
!!        integer(kind = kint), intent(in) :: iflag_dir, max_line_step
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!        integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout) ::   c_field(1)
!!        type(local_fieldline), intent(inout) :: fline_lc
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
      use t_paralell_surface_indices
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
      subroutine s_extend_field_line(node, ele, surf, para_surf,        &
     &          nod_fld, viz_fields, max_line_step,                     &
     &          end_trace, iflag_used_ele, iflag_dir, i_fline,          &
     &          isurf_org_dbl, x4_start, v4_start, c_field,             &
     &          icount_line, trace_length, iflag_comm, fline_lc, inum)
!
      use t_local_fline
      use trace_in_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in) :: i_fline
!
      integer(kind = kint), intent(in) :: inum
      real(kind = kreal), intent(in) ::   end_trace
      integer(kind = kint), intent(in) :: iflag_dir, max_line_step
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
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
      real(kind = kreal) :: x4_ele(4,ele%nnod_4_ele)
      real(kind = kreal) :: v4_ele(4,ele%nnod_4_ele)
      real(kind = kreal)                                                &
     &           :: color_ele(viz_fields%ntot_org_comp, ele%nnod_4_ele)
      integer(kind = kint) :: isf_tgt, isurf_end, jcou
      integer(kind = kint) :: isurf_org(2)
!
!
      if(isurf_org_dbl(2) .eq. 0) then
        iflag_comm = 0
!        write(*,*) 'Exit at initial tracing', my_rank, inum
        return
      end if
      isurf_org(1:2) = isurf_org_dbl(2:3)
!
      call add_fline_start(x4_start, v4_start,                          &
     &    viz_fields%ntot_color_comp, c_field(1), fline_lc)
!
      jcou = 0
      iflag_comm = 0
      do
        jcou = jcou + 1
        icount_line = icount_line + 1
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &                                  node%xx, x4_ele)
        call fline_vector_at_one_element(isurf_org(1), node, ele,       &
     &      nod_fld%d_fld(1,i_fline), v4_ele)
        call fline_colors_at_one_element(isurf_org(1), ele,             &
     &      nod_fld, viz_fields, color_ele)
!
!   extend in the middle of element
        call fline_trace_in_element(half, end_trace, trace_length,      &
     &      isurf_org(2), iflag_dir, ele, surf,                         &
     &      viz_fields, x4_ele, v4_ele, color_ele,                      &
     &      isf_tgt, x4_start, v4_start, c_field)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
!          write(*,*) 'Trace stops by zero vector', my_rank, inum,      &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
!          write(*,*) 'Error at trace to mid point', my_rank, inum,     &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        call add_fline_list(x4_start, v4_start,                         &
     &      viz_fields%ntot_color_comp, c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) return
!
!   extend to surface of element
        call fline_trace_in_element(one, end_trace, trace_length,       &
     &      izero, iflag_dir, ele, surf,                                &
     &      viz_fields, x4_ele, v4_ele, color_ele,                      &
     &      isf_tgt, x4_start, v4_start, c_field)
        if(isf_tgt .lt. 0) then
          iflag_comm = isf_tgt
!          write(*,*) 'Trace stops by zero vector', my_rank, inum,      &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
!          write(*,*) 'Error at trace to end point', my_rank, inum,     &
!     &              ' at ', jcou, ': ', isurf_org(1:2)
          exit
        end if
        call add_fline_list(x4_start, v4_start,                         &
     &      viz_fields%ntot_color_comp, c_field(1), fline_lc)
        if(trace_length.ge.end_trace .and. end_trace.gt.zero) exit
!
        isurf_end = abs(surf%isf_4_ele(isurf_org(1),isf_tgt))
        flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)              &
     &        + v4_start(2) * surf%vnorm_surf(isurf_end,2)              &
     &        + v4_start(3) * surf%vnorm_surf(isurf_end,3))             &
     &         * dble(surf%isf_4_ele(isurf_org(1),isf_tgt) / isurf_end) &
     &         * dble(iflag_dir)
!
!   set backside element and surface 
        if(para_surf%isf_4_ele_dbl(isurf_org(1),isf_tgt,2) .lt. 0) then
          isurf_org_dbl(1:3)                                            &
     &         = para_surf%iele_4_surf_dbl(isurf_end,1,1:3)
        else
          isurf_org_dbl(1:3)                                            &
     &         = para_surf%iele_4_surf_dbl(isurf_end,2,1:3)
        end if
        if(flux .lt. zero) then
!          isurf_org(1) = isurf_org(1)
          isurf_org(2) = isf_tgt
        else
          if(surf%isf_4_ele(isurf_org(1),isf_tgt) .lt. 0) then
            isurf_org(1:2) =     surf%iele_4_surf(isurf_end,1,1:2)
          else
            isurf_org(1:2) =     surf%iele_4_surf(isurf_end,2,1:2)
          end if
!
!          if(surf%interior_surf(isurf_end) .eq. izero) then
          if(isurf_org_dbl(1) .ne. my_rank                              &
     &        .or. isurf_org_dbl(3) .eq. 0) then
!            isurf_org(1) = isurf_org(1)
            isurf_org(2) = isf_tgt
            iflag_comm = 1
!            write(*,*) 'Exit for external surface', my_rank, inum
!       &            ': ', isurf_org_dbl(1:3), ': ',  &
!       &             para_surf%isf_4_ele_dbl(isurf_org(1),isf_tgt,2)
            exit
          end if
        end if
!
        if(max_line_step.gt.0 .and. icount_line.gt.max_line_step) then
            iflag_comm = 0
!            write(*,*) 'Exit by trace counts', my_rank, inum
            exit
        end if
        if(iflag_used_ele(isurf_org(1)) .eq. 0) then
!          isurf_org(2) = isf_tgt
          iflag_comm = 1
!          write(*,*) 'Exit from tracing area', my_rank, inum
          exit
        end if
        if(isurf_org(1) .eq. 0) then
          iflag_comm = -2
!          write(*,*) 'Trace leaves from domain', my_rank, inum
          exit
        end if
      end do
!
      end subroutine s_extend_field_line
!
!  ---------------------------------------------------------------------
!
      end module extend_field_line

