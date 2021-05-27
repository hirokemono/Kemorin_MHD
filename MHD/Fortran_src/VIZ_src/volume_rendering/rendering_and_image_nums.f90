!>@file   rendering_and_image_nums.f90
!!@brief  module rendering_and_image_nums
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine count_num_rendering_and_images(num_pvr, pvr_param,   &
!!     &          istack_pvr_render, istack_pvr_images,                 &
!!     &          num_pvr_rendering, num_pvr_images)
!!      subroutine count_num_anaglyph_and_images(num_pvr, pvr_param,    &
!!     &          num_pvr_rendering, num_pvr_images,                    &
!!     &          istack_pvr_render, istack_pvr_images)
!!      subroutine set_rendering_and_image_pes(num_pe, num_pvr,         &
!!     &          pvr_param, pvr_ctl, num_pvr_rendering, num_pvr_images,&
!!     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!!      subroutine set_anaglyph_rendering_pes(num_pe, num_pvr,          &
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
      private :: set_each_pvr_file_param, set_stereo_pvr_file_param
      private :: set_pvr_file_prefix, set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_rendering_and_images(num_pvr, pvr_param,     &
     &          num_pvr_rendering, num_pvr_images,                      &
     &          istack_pvr_render, istack_pvr_images)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      integer(kind = kint), intent(inout) :: num_pvr_rendering
      integer(kind = kint), intent(inout) :: num_pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      istack_pvr_render(0) = 0
      istack_pvr_images(0) = 0
      do i_pvr = 1, num_pvr
        if(pvr_param(i_pvr)%view%iflag_stereo_pvr .gt. 0) then
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 2
          if(pvr_param(i_pvr)%view%iflag_anaglyph .gt. 0) then
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
          else
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 2
          end if
        else if(pvr_param(i_pvr)%view%flag_quilt) then
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1)         &
     &                            + pvr_param(i_pvr)%view%n_row         &
     &                             * pvr_param(i_pvr)%view%n_column
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1)         &
     &                            + pvr_param(i_pvr)%view%n_row         &
     &                             * pvr_param(i_pvr)%view%n_column
        else
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 1
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
        end if
      end do
      num_pvr_rendering = istack_pvr_render(num_pvr)
      num_pvr_images =    istack_pvr_images(num_pvr)
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr',           num_pvr
      write(*,*) 'num_pvr_rendering', num_pvr_rendering
      write(*,*) 'num_pvr_images',    num_pvr_images
!
      end subroutine count_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_anaglyph_and_images(num_pvr, pvr_param,      &
     &          num_pvr_rendering, num_pvr_images,                      &
     &          istack_pvr_render, istack_pvr_images)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      integer(kind = kint), intent(inout) :: num_pvr_rendering
      integer(kind = kint), intent(inout) :: num_pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      istack_pvr_render(0) = 0
      istack_pvr_images(0) = 0
      do i_pvr = 1, num_pvr
        if(pvr_param(i_pvr)%view%iflag_stereo_pvr .gt. 0) then
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 2
          if(pvr_param(i_pvr)%view%iflag_anaglyph .gt. 0) then
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
          else
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 2
          end if
        else if(pvr_param(i_pvr)%view%flag_quilt) then
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1)         &
     &                            + pvr_param(i_pvr)%view%n_row         &
     &                             * pvr_param(i_pvr)%view%n_column
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1)         &
     &                            + pvr_param(i_pvr)%view%n_row         &
     &                             * pvr_param(i_pvr)%view%n_column
        else
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 1
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
        end if
      end do
      num_pvr_rendering = istack_pvr_render(num_pvr)
      num_pvr_images =    istack_pvr_images(num_pvr)
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr',           num_pvr
      write(*,*) 'num_pvr_rendering', num_pvr_rendering
      write(*,*) 'num_pvr_images',    num_pvr_images
!
      end subroutine count_num_anaglyph_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_rendering_and_image_pes(num_pe, num_pvr,           &
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
      integer(kind = kint), intent(in) :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(in) :: istack_pvr_images(0:num_pvr)
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
        ist = istack_pvr_images(i_pvr-1) + 1
        if(pvr_param(i_pvr)%view%iflag_stereo_pvr .gt. 0) then
          call set_stereo_pvr_file_param(pvr_ctl(i_pvr), pvr_rgb(ist))
        else
          call set_each_pvr_file_param(pvr_ctl(i_pvr), pvr_rgb(ist))
        end if
      end do
!
!      if(iflag_debug .eq. 0) return
      if(my_rank .gt. 0) return
      write(*,*) 'ID, File, ouput_PE, end_composition_PE, Num_PE'
      do i_pvr = 1, num_pvr_images
        write(*,*) i_pvr, trim(pvr_rgb(i_pvr)%pvr_prefix), '  ',        &
     &             pvr_rgb(i_pvr)%irank_image_file, &
     &                               pvr_rgb(i_pvr)%irank_end_composit, &
     &                                 pvr_rgb(i_pvr)%npe_img_composit, &
     &                                 trim(pvr_rgb(i_pvr)%pvr_prefix)
      end do
!
      end subroutine set_rendering_and_image_pes
!
!  ---------------------------------------------------------------------
!
      subroutine set_anaglyph_rendering_pes(num_pe, num_pvr,            &
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
      integer(kind = kint), intent(in) :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(in) :: istack_pvr_images(0:num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr
!
!
      call set_anaglyph_composite_pe_range(num_pe, num_pvr,             &
     &    pvr_param, pvr_ctl,  num_pvr_rendering, num_pvr_images,       &
     &    istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      do i_pvr = 1, num_pvr
        call set_each_pvr_file_param(pvr_ctl(i_pvr), pvr_rgb(i_pvr))
      end do
!
!      if(iflag_debug .eq. 0) return
      if(my_rank .gt. 0) return
      write(*,*) 'ID, File, ouput_PE, end_composition_PE, Num_PE'
      do i_pvr = 1, num_pvr_images
        write(*,*) i_pvr, trim(pvr_rgb(i_pvr)%pvr_prefix), '  ',        &
     &             pvr_rgb(i_pvr)%irank_image_file, &
     &                               pvr_rgb(i_pvr)%irank_end_composit, &
     &                                 pvr_rgb(i_pvr)%npe_img_composit, &
     &                                 trim(pvr_rgb(i_pvr)%pvr_prefix)
      end do
!
      end subroutine set_anaglyph_rendering_pes
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_file_param(pvr_ctl, pvr_rgb)
!
      use skip_comment_f
      use set_control_each_pvr
      use set_parallel_file_name
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call set_pvr_file_control(pvr_ctl,                                &
     &    pvr_rgb%iflag_monitoring, pvr_rgb%id_pvr_file_type,           &
     &    pvr_rgb%id_pvr_transparent)
      pvr_rgb%pvr_prefix = set_pvr_file_prefix(pvr_ctl)
!
      end subroutine set_each_pvr_file_param
!
!  ---------------------------------------------------------------------
!
      subroutine set_stereo_pvr_file_param(pvr_ctl, pvr_rgb)
!
      use skip_comment_f
      use set_control_each_pvr
      use set_parallel_file_name
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
      character(len=kchara) :: pvr_prefix
!
!
      pvr_prefix = set_pvr_file_prefix(pvr_ctl)
      call set_pvr_file_control(pvr_ctl,                                &
     &    pvr_rgb(1)%iflag_monitoring, pvr_rgb(1)%id_pvr_file_type,     &
     &    pvr_rgb(1)%id_pvr_transparent)
      call set_pvr_file_control(pvr_ctl,                                &
     &    pvr_rgb(2)%iflag_monitoring, pvr_rgb(2)%id_pvr_file_type,     &
     &    pvr_rgb(2)%id_pvr_transparent)
      pvr_rgb(1)%pvr_prefix = add_left_label(pvr_prefix)
      pvr_rgb(2)%pvr_prefix = add_right_label(pvr_prefix)
!
      end subroutine set_stereo_pvr_file_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      character(len = kchara) function set_pvr_file_prefix(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
!
      if(pvr_ctl%file_head_ctl%iflag .gt. 0) then
        set_pvr_file_prefix = pvr_ctl%file_head_ctl%charavalue
      else 
        set_pvr_file_prefix = 'pvr'
      end if
!
      end function set_pvr_file_prefix
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
      if(cmp_no_case(tmpchara, hd_PNG)) then
        id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, hd_BMP)) then
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
