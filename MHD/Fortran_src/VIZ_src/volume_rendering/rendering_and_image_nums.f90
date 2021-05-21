!>@file   rendering_and_image_nums.f90
!!@brief  module rendering_and_image_nums
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine count_num_rendering_and_images(num_pvr, pvr_param,   &
!!     &          num_pvr_rendering, num_pvr_images)
!!      subroutine s_num_rendering_and_images(num_pe, num_pvr,          &
!!     &          pvr_param, pvr_ctl, num_pvr_rendering, num_pvr_images,&
!!     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!!        integer, intent(in) :: num_pe
!!        integer(kind = kint), intent(in) :: num_pvr
!!        integer(kind = kint), intent(in) :: num_pvr_rendering
!!        integer(kind = kint), intent(in) :: num_pvr_images
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        integer(kind = kint), intent(inout)                           &
!!       &              :: istack_pvr_render(0:num_pvr)
!!        integer(kind = kint), intent(inout)                           &
!!       &              :: istack_pvr_images(0:num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!@endverbatim
!
      module rendering_and_image_nums
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
      use t_rendering_vr_image
      use t_pvr_image_array
!
      implicit none
!
      private :: set_pvr_file_parameters
      private :: set_pvr_file_prefix, set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rendering_and_images(num_pvr, pvr_param,     &
     &          num_pvr_rendering, num_pvr_images)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_pvr
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      integer(kind = kint), intent(inout) :: num_pvr_rendering
      integer(kind = kint), intent(inout) :: num_pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      num_pvr_rendering = num_pvr
      num_pvr_images =    num_pvr
      do i_pvr = 1, num_pvr
        if(pvr_param(i_pvr)%view%iflag_stereo_pvr .gt. 0) then
          num_pvr_rendering = num_pvr_rendering + 1
!
          if(pvr_param(i_pvr)%view%iflag_anaglyph .eq. 0) then
            num_pvr_images = num_pvr_images + 1
          end if
        end if
      end do
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr_rendering', num_pvr_rendering
      write(*,*) 'num_pvr_images', num_pvr_images
!
      end subroutine count_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine s_num_rendering_and_images(num_pe, num_pvr,            &
     &          pvr_param, pvr_ctl, num_pvr_rendering, num_pvr_images,  &
     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      use set_composition_pe_range
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in) :: num_pvr_images
!
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist
!
!
      call s_set_composition_pe_range(num_pe, num_pvr,                  &
     &    pvr_param, pvr_ctl,  num_pvr_rendering, num_pvr_images,       &
     &    istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      do i_pvr = 1, num_pvr
        ist = istack_pvr_images(i_pvr-1)
        call set_pvr_file_parameters                                    &
     &     (pvr_ctl(i_pvr), pvr_param(i_pvr)%view, pvr_rgb(ist+1))
      end do
!
!      if(iflag_debug .eq. 0) return
      if(my_rank .gt. 0) return
!      write(*,*) 'istack_pvr_render', istack_pvr_render
!      write(*,*) 'istack_pvr_images', istack_pvr_images
      write(*,*) 'ID, File, ouput_PE, end_composition_PE, Num_PE'
      do i_pvr = 1, num_pvr_images
        write(*,*) i_pvr, trim(pvr_rgb(i_pvr)%pvr_prefix), '  ',        &
     &             pvr_rgb(i_pvr)%irank_image_file, &
     &                               pvr_rgb(i_pvr)%irank_end_composit, &
     &                                 pvr_rgb(i_pvr)%npe_img_composit, &
     &                                 trim(pvr_rgb(i_pvr)%pvr_prefix)
      end do
!
      end subroutine s_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_parameters(pvr_ctl, view_param, pvr_rgb)
!
      use skip_comment_f
      use set_control_each_pvr
      use set_parallel_file_name
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_view_parameter), intent(in) :: view_param
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
      character(len=kchara) :: pvr_prefix
!
!
      call set_pvr_file_prefix(pvr_ctl, pvr_prefix)
      call set_pvr_file_control(pvr_ctl,                                &
     &    pvr_rgb(1)%iflag_monitoring, pvr_rgb(1)%id_pvr_file_type,     &
     &    pvr_rgb(1)%id_pvr_transparent)
      if(view_param%iflag_stereo_pvr .gt. 0) then
        if(view_param%iflag_anaglyph .gt. 0) then
          pvr_rgb(1)%pvr_prefix = pvr_prefix
        else
          pvr_rgb(1)%pvr_prefix = add_left_label(pvr_prefix)
          pvr_rgb(2)%pvr_prefix = add_right_label(pvr_prefix)
          call set_pvr_file_control(pvr_ctl,                            &
     &        pvr_rgb(2)%iflag_monitoring, pvr_rgb(2)%id_pvr_file_type, &
     &        pvr_rgb(2)%id_pvr_transparent)
        end if
      else
        pvr_rgb(1)%pvr_prefix = pvr_prefix
      end if
!
      end subroutine set_pvr_file_parameters
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_prefix(pvr_ctl, pvr_prefix)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      character(len = kchara), intent(inout) :: pvr_prefix
!
!
      if(pvr_ctl%file_head_ctl%iflag .gt. 0) then
        pvr_prefix = pvr_ctl%file_head_ctl%charavalue
      else 
        pvr_prefix = 'pvr'
      end if
!
      end subroutine set_pvr_file_prefix
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_control(pvr_ctl,                          &
     &          iflag_monitoring, id_pvr_file_type, id_pvr_transparent)
!
      use t_control_params_4_pvr
      use set_area_4_viz
      use skip_comment_f
      use output_image_sel_4_png
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      integer(kind = kint), intent(inout) :: iflag_monitoring
      integer(kind = kint), intent(inout) :: id_pvr_file_type
      integer(kind = kint), intent(inout) :: id_pvr_transparent
!
      character(len = kchara) :: tmpchara
!
!
      tmpchara = pvr_ctl%file_fmt_ctl%charavalue
      if(cmp_no_case(tmpchara, 'png')) then
        id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, 'bmp')) then
        id_pvr_file_type = iflag_BMP
      else
        id_pvr_file_type = iflag_BMP
      end if
!
      tmpchara = pvr_ctl%transparent_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'rgba')                             &
     &   .or. cmp_no_case(tmpchara, 'transparent')) then
        id_pvr_transparent = 1
      else if(cmp_no_case(tmpchara, 'rgb')                              &
     &   .or. cmp_no_case(tmpchara, 'solid')) then
        id_pvr_transparent = 0
      else
        id_pvr_transparent = 0
      end if
!
      iflag_monitoring = 0
      if(yes_flag(pvr_ctl%monitoring_ctl%charavalue)) then
        iflag_monitoring = 1
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'id_pvr_file_type', id_pvr_file_type
        write(*,*) 'id_pvr_transparent', id_pvr_transparent
      end if
!
      end subroutine set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      end module rendering_and_image_nums
