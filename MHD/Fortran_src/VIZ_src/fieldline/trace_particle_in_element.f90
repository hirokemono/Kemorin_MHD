!>@file   trace_particle_in_element.f90
!!       module trace_particle_in_element
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_trace_particle_in_element                          &
!!     &         (node, surf, para_surf, nod_fld, v_prev, viz_fields,   &
!!     &          i_tracer, iflag_used_ele, isurf_org_dbl,              &
!!     &          x4_start, v4_start, c_field, dt, iflag_comm)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        integer(kind = kint), intent(in) :: i_tracer
!!        integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: c_field(viz_fields%ntot_color_comp)
!!        real(kind = kreal), intent(inout) :: dt
!!        real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
!!        integer(kind = kint), intent(inout) :: iflag_comm
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
      use t_paralell_surface_indices
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
      subroutine s_trace_particle_in_element(node, ele, surf,           &
     &          para_surf, nod_fld, v_prev, viz_fields,                 &
     &          i_tracer, iflag_used_ele, isurf_org_dbl,                &
     &          x4_start, v4_start, c_field, dt, iflag_comm)
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
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
      integer(kind = kint), intent(in) :: i_tracer
!
      integer(kind = kint), intent(inout) :: isurf_org_dbl(3)
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout)                                 &
     &                    :: c_field(viz_fields%ntot_color_comp)
!
      real(kind = kreal), intent(inout) :: dt
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      integer(kind = kint), intent(inout) :: iflag_comm
!
      real(kind = kreal) :: flux
!
      integer(kind = kint) :: isurf_org(2)
      integer(kind = kint) :: isf_tgt, isurf_end

!
      if(isurf_org_dbl(2) .eq. 0) then
        iflag_comm = 0
!        write(*,*) 'Exit at initial tracing', my_rank, inum
        return
      end if
      isurf_org(1:2) = isurf_org_dbl(2:3)
!
      iflag_comm = 0
      do
!
!   extend in the middle of element
        call s_trace_in_element                                         &
     &     (half, isurf_org(1), isurf_org(2), node, surf,               &
     &      nod_fld,  viz_fields, isurf_end, isf_tgt, v_prev, i_tracer, &
     &      x4_start, v4_start, c_field, dt, iflag_comm)
        if(iflag_comm .eq. -1) return
!
!   extend to surface of element
        call s_trace_in_element(one, isurf_org(1), izero, node, surf,   &
     &      nod_fld, viz_fields, isurf_end, isf_tgt, v_prev, i_tracer,  &
     &      x4_start, v4_start, c_field, dt, iflag_comm)
        if(iflag_comm .eq. -1) return
!
        flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)              &
     &        + v4_start(2) * surf%vnorm_surf(isurf_end,2)              &
     &        + v4_start(3) * surf%vnorm_surf(isurf_end,3))             &
     &         * dble(surf%isf_4_ele(isurf_org(1),isf_tgt) / isurf_end) &
     &         *(-one)**iflag_forward
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
        if(dt .le. 0) then
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
      end subroutine s_trace_particle_in_element
!
!  ---------------------------------------------------------------------
!
      end module trace_particle_in_element

