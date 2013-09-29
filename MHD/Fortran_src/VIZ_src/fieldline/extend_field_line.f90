!extend_field_line.f90
!
!      module extend_field_line
!
!      Written by H. Matsui on Aug., 2011
!
!      subroutine s_extend_field_line(numnod, numele, numsurf,          &
!     &          nnod_4_surf, xx, ie_surf, isf_4_ele,                   &
!     &          iele_4_surf, interior_surf, vnorm_surf,                &
!     &          max_line_step, iflag_used_ele, iflag_back,             &
!     &          vect_nod, color_nod, isurf_org, x_start, v_start,      &
!     &          c_field, icount_line, iflag_comm)
!
      module extend_field_line
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_extend_field_line(numnod, numele, numsurf,           &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
     &          iele_4_surf, interior_surf, vnorm_surf,                 &
     &          max_line_step, iflag_used_ele, iflag_back,              &
     &          vect_nod, color_nod, isurf_org, x_start, v_start,       &
     &          c_field, icount_line, iflag_comm)
!
      use m_local_fline
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
      integer(kind = kint), intent(in) :: iflag_back, max_line_step
      integer(kind = kint), intent(in) :: iflag_used_ele(numele)
      real(kind = kreal), intent(in) :: vect_nod(numnod,3)
      real(kind = kreal), intent(in) :: color_nod(numnod)
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) ::   v_start(3), x_start(3)
      real(kind = kreal), intent(inout) ::   c_field(1)
!
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      real(kind = kreal) :: x_tgt(3), v_tgt(3), c_tgt(1), xi(2), flux
!
!
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if
!
      call add_fline_start(x_start, c_field(1))
!
       do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
!   extend in the middle of element
!
        call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf, &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v_start, x_start, isf_tgt, x_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, vect_nod, v_tgt)
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, color_nod, c_tgt(1))
!
        isf_org =  0
        x_start(1:3) = half * (x_start(1:3) + x_tgt(1:3))
        v_start(1:3) = half * (v_start(1:3) + v_tgt(1:3))
        c_field(1) =   half * (c_field(1) + c_tgt(1))
!
        call add_fline_list(x_start, c_field(1))
!
!   extend to surface of element
!
        call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf, &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v_start, x_start, isf_tgt, x_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, vect_nod, v_start)
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, color_nod, c_field(1))
        x_start(1:3) =  x_tgt(1:3)
!
        call add_fline_list(x_start, c_field(1))
!
        flux = (v_start(1) * vnorm_surf(isurf_end,1)                    &
     &        + v_start(2) * vnorm_surf(isurf_end,2)                    &
     &        + v_start(3) * vnorm_surf(isurf_end,3))                   &
     &         * dble(isf_4_ele(iele,isf_tgt) / isurf_end)              &
     &         *(-one)**iflag_back
!
!         write(60+my_rank,'(a6,i8,1p4e16.7)')  'x_tgt: ', icount_line, &
!     &          v_start(1:3), flux
!
        if(interior_surf(isurf_end) .eq. izero) then
          isurf_org(1) = iele
          isurf_org(2) = isf_tgt
          isurf_org(3) = ie_surf(isurf_end,1)
          iflag_comm = 1
          exit
        end if
!
!   set backside element and surface 
!
        if(flux.ge.zero) then
          if(isf_4_ele(iele,isf_tgt) .lt. 0) then
            isurf_org(1) = iele_4_surf(isurf_end,1,1)
            isurf_org(2) = iele_4_surf(isurf_end,1,2)
          else
            isurf_org(1) = iele_4_surf(isurf_end,2,1)
            isurf_org(2) = iele_4_surf(isurf_end,2,2)
          end if
        else
          iflag_comm = -2
          exit
        end if
!
!         write(70+my_rank,*) 'isurf_end', icount_line, iele, isf_tgt,  &
!     &                        isf_4_ele(iele,isf_tgt)
!         write(70+my_rank,*) 'isurf_nxt', icount_line, isurf_org(1:2), &
!     &                        isf_4_ele(isurf_org(1),isurf_org(2))
!
        if(isurf_org(1).eq.0 .or.  iflag_used_ele(iele).eq.0            &
     &     .or. icount_line.gt.max_line_step) then
          iflag_comm = 0
          exit
        end if
      end do
!
      end subroutine s_extend_field_line
!
!  ---------------------------------------------------------------------
!
      end module extend_field_line

