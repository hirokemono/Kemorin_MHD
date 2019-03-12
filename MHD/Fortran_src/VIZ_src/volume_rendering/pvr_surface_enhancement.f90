!>@file  pvr_surface_enhancement.f90
!!       module pvr_surface_enhancement
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Set flag and opacities to enhance surfaces
!!
!!@verbatim
!!      subroutine set_pvr_bc_enhanse_flag                              &
!!     &         (surf_grp, num_enhanse_grp, enhanse_grp, draw_type,    &
!!     &          fixed_opacity, iflag_enhanse, enhansed_opacity)
!!      subroutine set_opacity_for_boundaries(surf_grp, sf_grp_v,       &
!!     &          view_param, iflag_enhanse, enhansed_opacity,          &
!!     &          numele, numsurf, isf_4_ele, arccos_sf)
!!        type(surface_group_data), intent(in) :: surf_grp
!!@endverbatim
!
      module pvr_surface_enhancement
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_group_data
      use t_surface_group_geometry
      use t_surface_group_connect
      use t_control_params_4_pvr
!
      use calypso_mpi
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: LABEL_INCREASE = 'increase'
      character(len = kchara), parameter                                &
     &                        :: LABEL_DECREASE = 'decrease'
!
      character(len = kchara), parameter :: LABEL_EDGE = 'boarder'
      character(len = kchara), parameter                                &
     &                        :: LABEL_FORWARD = 'forward_surface'
      character(len = kchara), parameter                                &
     &                        :: LABEL_REVERSE = 'reverse_surface'
!
      integer(kind = kint), parameter :: IFLAG_NONE =          0
      integer(kind = kint), parameter :: IFLAG_SHOW_EDGE =     2
      integer(kind = kint), parameter :: IFLAG_SHOW_FORWARD =  1
      integer(kind = kint), parameter :: IFLAG_SHOW_REVERSE = -1
!
      real(kind = kreal), parameter :: coef_op = 4.0e-2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_bc_enhanse_flag                                &
     &         (surf_grp, num_enhanse_grp, enhanse_grp, draw_type,      &
     &          fixed_opacity, iflag_enhanse, enhansed_opacity)
!
      use t_control_params_4_pvr
      use skip_comment_f
!
      type(surface_group_data), intent(in) :: surf_grp
      integer(kind = kint), intent(in) :: num_enhanse_grp
      character(len=kchara), intent(in) :: enhanse_grp(num_enhanse_grp)
      character(len=kchara), intent(in) :: draw_type(num_enhanse_grp)
      real(kind = kreal), intent(in) :: fixed_opacity(num_enhanse_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: iflag_enhanse(surf_grp%num_grp)
      real(kind = kreal), intent(inout)                                 &
     &                     :: enhansed_opacity(surf_grp%num_grp)
!
      integer(kind = kint) :: igrp, jgrp
!
!
      iflag_enhanse(1:surf_grp%num_grp) = IFLAG_NONE
      do jgrp = 1, num_enhanse_grp
        do igrp = 1, surf_grp%num_grp
          if(cmp_no_case(enhanse_grp(jgrp),                             &
     &                   surf_grp%grp_name(igrp))) then
            if(cmp_no_case(draw_type(jgrp), LABEL_EDGE)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_EDGE
            else if(cmp_no_case(draw_type(jgrp), LABEL_FORWARD)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_FORWARD
            else if(cmp_no_case(draw_type(jgrp), LABEL_REVERSE)) then
              iflag_enhanse(igrp) = IFLAG_SHOW_REVERSE
            end if
            enhansed_opacity(igrp) = fixed_opacity(jgrp)
            exit
          end if
        end do
      end do
!
      end subroutine set_pvr_bc_enhanse_flag
!
!  ---------------------------------------------------------------------
!
      subroutine set_opacity_for_boundaries(surf_grp, sf_grp_v,         &
     &          view_param, iflag_enhanse, enhansed_opacity,            &
     &          numele, numsurf, isf_4_ele, arccos_sf)
!
      use set_position_pvr_screen
!
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(pvr_view_parameter), intent(in) :: view_param
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                     :: iflag_enhanse(surf_grp%num_grp)
      real(kind = kreal), intent(in)                                    &
     &                     :: enhansed_opacity(surf_grp%num_grp)
!
      real(kind = kreal), intent(inout) :: arccos_sf(numsurf)
!
      integer(kind = kint) :: igrp
      integer(kind = kint) :: ist, ied, inum, isurf, iele, k1
      real(kind = kreal) :: size_v, ratio
      real(kind = kreal), allocatable :: norm_sf_model(:,:)
!
!
      allocate(norm_sf_model(surf_grp%num_item,4))
!
!$omp parallel workshare
      norm_sf_model = zero
!$omp end parallel workshare
!
      call chenge_direction_pvr_modelview(view_param%modelview_mat,     &
     &    surf_grp%num_item, sf_grp_v%vnorm_sf_grp, norm_sf_model)
!
!$omp parallel do
      do inum = 1, surf_grp%num_item
        if(norm_sf_model(inum,3) .eq. zero) then
          norm_sf_model(inum,3) =  1e-6
        end if
      end do
!$omp end parallel do
!
      do igrp = 1, surf_grp%num_grp
        ist = surf_grp%istack_grp(igrp-1)+1
        ied = surf_grp%istack_grp(igrp)
        do inum = ist, ied
          iele = surf_grp%item_sf_grp(1,inum)
          k1 =   surf_grp%item_sf_grp(2,inum)
          isurf = abs(isf_4_ele(iele,k1))
          size_v = sqrt(norm_sf_model(inum,1)*norm_sf_model(inum,1)     &
     &                + norm_sf_model(inum,2)*norm_sf_model(inum,2)     &
     &                + norm_sf_model(inum,3)*norm_sf_model(inum,3))
          ratio = coef_op * size_v / norm_sf_model(inum,3)
!
          if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_EDGE) then
            if(abs(ratio) .gt. ONE) then
              arccos_sf(isurf) = enhansed_opacity(igrp)
            else
              arccos_sf(isurf) = zero
            end if
          else if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_FORWARD) then
            if(ratio .lt. zero) then
              arccos_sf(isurf) = zero
            else
              arccos_sf(isurf) = enhansed_opacity(igrp)
            end if
          else if(iflag_enhanse(igrp) .eq. IFLAG_SHOW_REVERSE) then
            if(ratio .lt. zero) then
              arccos_sf(isurf) = enhansed_opacity(igrp)
            else
              arccos_sf(isurf) = zero
            end if
          else
            arccos_sf(isurf) = zero
          end if
        end do
      end do
!
      end subroutine set_opacity_for_boundaries
!
!  ---------------------------------------------------------------------
!
      end module pvr_surface_enhancement
