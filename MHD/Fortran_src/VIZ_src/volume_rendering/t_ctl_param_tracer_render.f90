!>@file   t_ctl_param_tracer_render.f90
!!       module t_ctl_param_tracer_render
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_control_pvr_tracer(num_tracer, tcr_prm,          &
!!     &          num_pvr_tracer_ctl, pvr_trc_c, tracer_pvr_prm)
!!      subroutine dealloc_pvr_tracer_param(tracer_pvr_prm)
!!        integer(kind = kint), intent(in) :: num_pvr_tracer_ctl
!!        type(pvr_tracer_ctl), intent(in)                              &
!!     &                     :: pvr_trc_c(num_pvr_tracer_ctl)
!!        integer(kind = kint), intent(in) :: num_tracer
!!        type(fieldline_paramter), intent(in) :: tcr_prm(num_tracer)
!!        type(tracer_render_param), intent(inout) :: tracer_pvr_prm
!!@endverbatim
!
      module t_ctl_param_tracer_render
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for tracer rendering
      type tracer_render_param
!>    Number of isosurfaces
        integer(kind = kint) :: num_pvr_tracer
!>    Number of isosurfaces
        character(len = kchara), allocatable :: tracer_prefix(:)
!>    Number of isosurfaces
        integer(kind = kint), allocatable :: id_tracer_model(:)
!>    fiale value for isosurfaces
        real(kind = kreal), allocatable :: rendering_radius(:)
!>    Opacity value for isosurfaces
        real(kind = kreal), allocatable :: tracer_opacity(:)
      end type tracer_render_param
!
      private :: alloc_pvr_tracer_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_pvr_tracer(num_tracer, tcr_prm,            &
     &          num_pvr_tracer_ctl, pvr_trc_c, tracer_pvr_prm)
!
      use t_control_data_pvr_tracers
      use t_control_params_4_fline
!
      integer(kind = kint), intent(in) :: num_pvr_tracer_ctl
      type(pvr_tracer_ctl), intent(in)                                  &
     &                     :: pvr_trc_c(num_pvr_tracer_ctl)
!
      integer(kind = kint), intent(in) :: num_tracer
      type(fieldline_paramter), intent(in) :: tcr_prm(num_tracer)
!
      type(tracer_render_param), intent(inout) :: tracer_pvr_prm
!
      integer(kind = kint) ::  i, j, icou
      character(len = kchara) :: fname_model, fname_ctl
      character(len = kchara) :: tmpchara
!
      integer(kind = kint) :: iflag
!
!
      icou = 0
      do i = 1, num_pvr_tracer_ctl
        fname_ctl = pvr_trc_c(i)%tracer_file_prefix%charavalue
        do j = 1, num_tracer
          fname_model = tcr_prm(j)%fline_file_IO%file_prefix
          if(fname_model .eq. fname_ctl) then
            icou = icou + 1
            exit
          end if
        end do
      end do
!
      call alloc_pvr_tracer_param(icou, tracer_pvr_prm)
      if(tracer_pvr_prm%num_pvr_tracer .le. 0) return
!
      icou = 0
      do i = 1, num_pvr_tracer_ctl
        fname_ctl = pvr_trc_c(i)%tracer_file_prefix%charavalue
        do j = 1, num_tracer
          fname_model = tcr_prm(j)%fline_file_IO%file_prefix
          if(fname_model .eq. fname_ctl) then
            icou = icou + 1
            tracer_pvr_prm%tracer_prefix(icou) =   fname_model
            tracer_pvr_prm%id_tracer_model(icou) = j
            exit
          end if
        end do
      end do
!
!
      do i = 1, tracer_pvr_prm%num_pvr_tracer
        iflag = pvr_trc_c(i)%render_radius_ctl%iflag
        if(iflag .gt. 0) then
          tracer_pvr_prm%rendering_radius(i)                            &
     &       = pvr_trc_c(i)%render_radius_ctl%realvalue
        end if
!
        iflag = pvr_trc_c(i)%opacity_ctl%iflag
        if(iflag .gt. 0) then
          tracer_pvr_prm%tracer_opacity(i)                              &
     &        = pvr_trc_c(i)%opacity_ctl%realvalue
        end if
      end do
!
      end subroutine set_control_pvr_tracer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_tracer_param(tracer_pvr_prm)
!
      type(tracer_render_param), intent(inout) :: tracer_pvr_prm
!
!
      deallocate(tracer_pvr_prm%tracer_prefix)
      deallocate(tracer_pvr_prm%id_tracer_model)
      deallocate(tracer_pvr_prm%rendering_radius)
      deallocate(tracer_pvr_prm%tracer_opacity)
!
      end subroutine dealloc_pvr_tracer_param
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_tracer_param(num, tracer_pvr_prm)
!
      integer(kind = kint), intent(in) :: num
      type(tracer_render_param), intent(inout) :: tracer_pvr_prm
!
!
      tracer_pvr_prm%num_pvr_tracer = num
      allocate(tracer_pvr_prm%tracer_prefix(num))
      allocate(tracer_pvr_prm%id_tracer_model(num))
      allocate(tracer_pvr_prm%rendering_radius(num))
      allocate(tracer_pvr_prm%tracer_opacity(num))
!
      if(num .le. 0) return
      tracer_pvr_prm%id_tracer_model(:) =  0
      tracer_pvr_prm%rendering_radius(:) = zero
      tracer_pvr_prm%tracer_opacity(:) =   zero
!
      end subroutine alloc_pvr_tracer_param
!
! -----------------------------------------------------------------------
!
      end module t_ctl_param_tracer_render
