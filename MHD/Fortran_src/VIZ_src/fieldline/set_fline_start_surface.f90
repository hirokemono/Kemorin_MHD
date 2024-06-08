!set_fline_start_surface.f90
!
!      module set_fline_start_surface
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_fline_start_surf(surf, nod_fld,                  &
!!     &                                fln_prm, fln_src, fln_tce)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module set_fline_start_surface
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_tracing_data
      use calypso_mpi
!
      implicit  none
!
      private :: choose_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_surf(ele, surf, isf_4_ele_dbl, nod_fld,&
     &                                fln_prm, fln_src, fln_tce)
!
      use m_constants
      use m_geometry_constants
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      use cal_field_on_surf_viz
      use trace_in_element
      use tracer_field_interpolate
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint)  :: i, iele, isf_1ele, isurf
      integer(kind = kint)  :: inum1, inum2
      real(kind = kreal) :: xyz_surf(3)
      real(kind = kreal), parameter :: xi(2) = (/zero, zero/)
!
      integer(kind = kint)  :: isf_dbl_st_tmp(2)
!
!
      do i = 1, fln_src%num_line_local
        inum1 = i
        iele =     fln_prm%id_surf_start_fline(1,i)
        isf_1ele = fln_prm%id_surf_start_fline(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf_1ele))
!
        if(fln_prm%id_fline_direction .ne. iflag_both_trace) then
           fln_tce%iflag_direction(inum1) = fln_prm%id_fline_direction
           call choose_fline_start_surf                                 &
     &        (fln_src%iflag_outward_flux_fline(i),                     &
     &         iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,         &
     &         fln_tce%iflag_direction(inum1), isf_dbl_st_tmp)
!
          fln_tce%isf_dbl_start(1,inum1) = my_rank
          fln_tce%isf_dbl_start(2,inum1) = isf_dbl_st_tmp(1)
          fln_tce%isf_dbl_start(3,inum1) = isf_dbl_st_tmp(2)
          fln_tce%xx_fline_start(1:4,inum1)                             &
     &       = fln_src%xx4_initial_fline(1:4,i)
          fln_tce%trace_length(inum1) = 0.0d0
          fln_tce%icount_fline(inum1) = 0
!
          xyz_surf(1:3) = surf%x_surf(isurf,1:3)
          call cal_fields_on_line(isurf, xi, xyz_surf,                  &
     &                          surf, nod_fld, fln_prm%fline_fields,    &
     &                          fln_tce%c_fline_start(1,inum1))
        else
           fln_tce%iflag_direction(inum1) = iflag_forward_trace
           call choose_fline_start_surf                                 &
     &        (fln_src%iflag_outward_flux_fline(i),                     &
     &         iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,         &
     &         fln_tce%iflag_direction(inum1),isf_dbl_st_tmp)
          fln_tce%isf_dbl_start(1,inum1) = my_rank
          fln_tce%isf_dbl_start(2,inum1) = isf_dbl_st_tmp(1)
          fln_tce%isf_dbl_start(3,inum1) = isf_dbl_st_tmp(2)
          fln_tce%xx_fline_start(1:4,inum1)                             &
     &       = fln_src%xx4_initial_fline(1:4,i)
          fln_tce%trace_length(inum1) = 0.0d0
          fln_tce%icount_fline(inum1) = 0
!
          xyz_surf(1:3) = surf%x_surf(isurf,1:3)
          call cal_fields_on_line(isurf, xi, xyz_surf,                  &
     &                          surf, nod_fld, fln_prm%fline_fields,    &
     &                          fln_tce%c_fline_start(1,inum1))
!
          inum2 = inum1 + fln_src%num_line_local
           call choose_fline_start_surf                                 &
     &         (fln_src%iflag_outward_flux_fline(i),                    &
     &          iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,        &
     &          fln_tce%iflag_direction(inum2), isf_dbl_st_tmp)
          fln_tce%isf_dbl_start(1,inum2) = my_rank
          fln_tce%isf_dbl_start(2,inum2) = isf_dbl_st_tmp(1)
          fln_tce%isf_dbl_start(3,inum2) = isf_dbl_st_tmp(2)
          fln_tce%trace_length(inum2) = 0.0d0
          fln_tce%icount_fline(inum2) = 0
          fln_tce%iflag_direction(inum2) = iflag_backward_trace
          call copy_global_start_fline(inum2, inum1,                    &
     &                                 fln_prm%fline_fields, fln_tce)
        end if
      end do
!
      end subroutine set_fline_start_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine choose_fline_start_surf                                &
     &         (iflag_outward_flux, iele, isf_1ele, isurf, ele, surf,   &
     &          isf_4_ele_dbl, iflag_direction, isf_dbl_start)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
!
      integer(kind = kint), intent(in) :: iflag_direction
      integer(kind = kint), intent(inout) :: isf_dbl_start(2)
!
!
      if((iflag_direction*iflag_outward_flux) .le. 0) then
        isf_dbl_start(1) = iele
        isf_dbl_start(2) = isf_1ele
      else if(isf_4_ele_dbl(iele,isf_1ele,2) .le. 0) then
        isf_dbl_start(1) = surf%iele_4_surf(isurf,1,1)
        isf_dbl_start(2) = surf%iele_4_surf(isurf,1,2)
      else
        isf_dbl_start(1) = surf%iele_4_surf(isurf,2,1)
        isf_dbl_start(2) = surf%iele_4_surf(isurf,2,2)
      end if
!
      end subroutine choose_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      end module set_fline_start_surface
