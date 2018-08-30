!>@file   t_pvr_image_data.f90
!!@brief  module t_pvr_image_data
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine s_num_rendering_and_images(num_pvr_ctl, pvr_ctl,     &
!!     &          num_pvr, num_pvr_rendering, num_pvr_images,           &
!!     &          istack_pvr_images)
!!      subroutine set_streo_rendering_flags                            &
!!     &         (num_pvr_ctl, pvr_ctl, pvr_param, pvr_data)
!!        type(volume_rendering_controls), intent(in)                   &
!!     &                     :: pvr_ctls(num_pvr_ctl)
!!        type(PVR_control_params), intent(inout)                       &
!!     &                     :: pvr_param(num_pvr_ctl)
!!        type(PVR_image_generator), intent(inout)                      &
!!     &                     :: pvr_data(num_pvr_ctl)
!!@endverbatim
!
      module t_pvr_image_data
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
!
      implicit none
!
!>  Structure for PVR images
      type pvr_mul_image_data
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
      end type pvr_mul_image_data
!
      private :: alloc_istack_pvr_image_4_merge
      private :: count_num_rendering_and_images
      private :: set_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_num_rendering_and_images(num_pvr_ctl, pvr_ctl,       &
     &          num_pvr, num_pvr_rendering, pvr_images)
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
      integer(kind = kint), intent(inout) :: num_pvr
      integer(kind = kint), intent(inout) :: num_pvr_rendering
!
      type(pvr_mul_image_data), intent(inout) :: pvr_images
!
!
      call count_num_rendering_and_images(num_pvr_ctl, pvr_ctl,         &
     &    num_pvr, num_pvr_rendering, pvr_images%num_pvr_images)
!
      call alloc_istack_pvr_image_4_merge(pvr_images)
!
      call set_num_rendering_and_images(num_pvr_ctl, pvr_ctl,           &
     &    pvr_images%num_pvr_images, pvr_images%istack_pvr_images)
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr_rendering', num_pvr_rendering
      write(*,*) 'num_pvr_images', pvr_images%num_pvr_images
      write(*,*) 'istack_pvr_images', pvr_images%istack_pvr_images
!
      end subroutine s_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istack_pvr_image_4_merge(pvr_images)
!
      type(pvr_mul_image_data), intent(inout) :: pvr_images
!
      deallocate(pvr_images%istack_pvr_images)
!
      end subroutine dealloc_istack_pvr_image_4_merge
!
!  ---------------------------------------------------------------------
!
      subroutine set_streo_rendering_flags                              &
     &         (num_pvr_ctl, pvr_ctl, pvr_param, pvr_data)
!
      use skip_comment_f
      use t_rendering_vr_image
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
      type(PVR_control_params), intent(inout)                           &
     &                     :: pvr_param(num_pvr_ctl)
      type(PVR_image_generator), intent(inout)                          &
     &                     :: pvr_data(num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr_ctl
        pvr_data(i_pvr)%view%iflag_stereo_pvr =  0
        pvr_param(i_pvr)%file%iflag_anaglyph =   0
        if(yes_flag(pvr_ctl(i_pvr)%streo_ctl%charavalue)) then
          pvr_data(i_pvr)%view%iflag_stereo_pvr = 1
!
          if(yes_flag(pvr_ctl(i_pvr)%anaglyph_ctl%charavalue)) then
            pvr_param(i_pvr)%file%iflag_anaglyph = 1
          end if
        end if
      end do
!
      end subroutine set_streo_rendering_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_pvr_image_4_merge(pvr_images)
!
      type(pvr_mul_image_data), intent(inout) :: pvr_images
!
      integer(kind = kint) :: num
!
      num = pvr_images%num_pvr_images
      allocate(pvr_images%istack_pvr_images(0:num))
      pvr_images%istack_pvr_images = 0
!
      end subroutine alloc_istack_pvr_image_4_merge
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rendering_and_images(num_pvr_ctl, pvr_ctls,  &
     &          num_pvr, num_pvr_rendering, num_pvr_images)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
      integer(kind = kint), intent(inout) :: num_pvr
      integer(kind = kint), intent(inout) :: num_pvr_rendering
      integer(kind = kint), intent(inout) :: num_pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      num_pvr = num_pvr_ctl
      num_pvr_rendering = num_pvr_ctl
      num_pvr_images = num_pvr_ctl
      do i_pvr = 1, num_pvr
          write(*,*) 'pvr_ctls(i_pvr)%streo_ctl%charavalue: ', &
     &        i_pvr, trim(pvr_ctls(i_pvr)%streo_ctl%charavalue)
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          num_pvr_rendering = num_pvr_rendering + 1
!
          write(*,*) 'pvr_ctls(i_pvr)%anaglyph_ctl%charavalue: ', &
     &        i_pvr, trim(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            num_pvr_images = num_pvr_images + 0
          else
            num_pvr_images = num_pvr_images + 1
          end if
        end if
      end do
!
      end subroutine count_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_rendering_and_images(num_pvr_ctl, pvr_ctls,    &
     &          num_pvr_images, istack_pvr_images)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
      integer(kind = kint), intent(in) :: num_pvr_images
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr_images)
!
      integer(kind = kint) :: i_pvr, icou
!
!
      icou = 0
      do i_pvr = 1, num_pvr_ctl
        icou = icou + 1
        istack_pvr_images(icou) = istack_pvr_images(icou-1) + 1
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            istack_pvr_images(icou) = istack_pvr_images(icou) + 1
          else
            icou = icou + 1
            istack_pvr_images(icou) = istack_pvr_images(icou-1) + 1
          end if
        end if
      end do
!
      end subroutine set_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_data
