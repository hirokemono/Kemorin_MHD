!set_fline_start_surface.f90
!
!      module set_fline_start_surface
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_fline_start_surf(my_rank, i_fln,                 &
!!     &          numnod, numele, numsurf, nnod_4_surf,                 &
!!     &          ie_surf, isf_4_ele, iele_4_surf,                      &
!!     &          fline_prm, fline_src, fline_tce)
!!        type(fieldline_paramters), intent(in) :: fline_prm
!!        type(fieldline_source), intent(in) :: fline_src
!!        type(fieldline_trace), intent(inout) :: fline_tce
!
      module set_fline_start_surface
!
      use m_precision
!
      implicit  none
!
      private :: set_forward_fline_start_surf
      private :: set_backward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_surf(my_rank, i_fln,                   &
     &          numnod, numele, numsurf, nnod_4_surf,                   &
     &          ie_surf, isf_4_ele, iele_4_surf,                        &
     &          fline_prm, fline_src, fline_tce)
!
      use m_constants
      use m_geometry_constants
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(in) :: i_fln
      type(fieldline_paramters), intent(in) :: fline_prm
!
      type(fieldline_source), intent(in) :: fline_src
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint)  :: i, iline, iele, isf_1ele, isurf
      integer(kind = kint)  :: ist_line, inum1,  inum2
      real(kind = kreal), parameter :: xi(2) = (/zero, zero/)
!
!
      ist_line = fline_prm%istack_each_field_line(i_fln-1)
      do i = 1, fline_src%num_line_local(i_fln)
        iline = i + ist_line
        inum1 = i + fline_tce%istack_all_fline(my_rank,i_fln)
        iele =     fline_prm%id_surf_start_fline(1,iline)
        isf_1ele = fline_prm%id_surf_start_fline(2,iline)
!        write(*,*) 'iline', my_rank, i, iline, inum1, &
!     &              iele, isf_1ele, numnod, numsurf
        isurf = abs(isf_4_ele(iele,isf_1ele))
!
        fline_tce%xx_fline_start(1:3,inum1)                             &
     &       =  fline_src%xx_start_fline(1:3,iline)
!
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf, xi, fline_src%vector_nod_fline(1,1,i_fln),  &
     &      fline_tce%v_fline_start(1,inum1))
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf, xi, fline_src%color_nod_fline(1,i_fln),     &
     &      fline_tce%c_fline_start(inum1))
!
        if( fline_prm%id_fline_direction(i_fln) .eq. 1) then
           call set_forward_fline_start_surf                            &
     &        (fline_prm%iflag_outward_flux_fline(iline),               &
     &         iele, isf_1ele, isurf,                                   &
     &         numsurf, nnod_4_surf, ie_surf, iele_4_surf,              &
     &         fline_tce%iflag_fline(inum1),                            &
     &         fline_tce%isf_fline_start(1,inum1))
!
        else if( fline_prm%id_fline_direction(i_fln) .eq. -1) then
           call set_backward_fline_start_surf                           &
     &         (fline_prm%iflag_outward_flux_fline(iline),              &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fline_tce%iflag_fline(inum1),                           &
     &          fline_tce%isf_fline_start(1,inum1))
!
        else
           call set_forward_fline_start_surf                            &
     &         (fline_prm%iflag_outward_flux_fline(iline),              &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fline_tce%iflag_fline(inum1),                           &
     &          fline_tce%isf_fline_start(1,inum1))
!
          inum2 = inum1 + fline_src%num_line_local(i_fln)
          fline_tce%xx_fline_start(1:3,inum2)                           &
     &          = fline_tce%xx_fline_start(1:3,inum1)
          fline_tce%v_fline_start(1:3,inum2)                            &
     &          = fline_tce%v_fline_start(1:3,inum1)
          fline_tce%c_fline_start(inum2)                                &
     &          = fline_tce%c_fline_start(inum1)
!
           call set_backward_fline_start_surf                           &
     &         (fline_prm%iflag_outward_flux_fline(iline),              &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fline_tce%iflag_fline(inum2),                           &
     &          fline_tce%isf_fline_start(1,inum2))
        end if
      end do
!
      end subroutine set_fline_start_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_forward_fline_start_surf(iflag_outward_flux,       &
     &          iele, isf_1ele, isurf, numsurf, nnod_4_surf,            &
     &          ie_surf, iele_4_surf, iflag_fline, isf_fline_start)
!
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(inout) :: iflag_fline
      integer(kind = kint), intent(inout) :: isf_fline_start(3)
!
      integer(kind = kint) :: inod
!
!
      inod =  ie_surf(isurf,1)
!
      iflag_fline = 0
      if(iflag_outward_flux .eq. 0) then
        isf_fline_start(1) = iele
        isf_fline_start(2) = isf_1ele
        isf_fline_start(3) = inod
      else
        isf_fline_start(1) = iele_4_surf(isurf,2,1)
        isf_fline_start(2) = iele_4_surf(isurf,2,2)
        isf_fline_start(3) = inod
      end if
!
      end subroutine set_forward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      subroutine set_backward_fline_start_surf(iflag_outward_flux,      &
     &          iele, isf_1ele, isurf, numsurf, nnod_4_surf,            &
     &          ie_surf, iele_4_surf, iflag_fline, isf_fline_start)
!
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(inout) :: iflag_fline
      integer(kind = kint), intent(inout) :: isf_fline_start(3)
!
      integer(kind = kint) :: inod
!
!
      inod =  ie_surf(isurf,1)
!
      iflag_fline = 1
!
      if(iflag_outward_flux .eq. 1) then
        isf_fline_start(1) = iele
        isf_fline_start(2) = isf_1ele
        isf_fline_start(3) = inod
      else
        isf_fline_start(1) = iele_4_surf(isurf,2,1)
        isf_fline_start(2) = iele_4_surf(isurf,2,2)
        isf_fline_start(3) = inod
      end if
!
      end subroutine set_backward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      end module set_fline_start_surface
