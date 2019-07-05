!>@file   rendering_and_image_nums.f90
!!@brief  module rendering_and_image_nums
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine count_num_rendering_and_images(num_pvr_ctl, pvr_ctls,&
!!     &          num_pvr, num_pvr_rendering, num_pvr_images)
!!      subroutine s_num_rendering_and_images(num_pe, num_pvr, pvr_ctl, &
!!     &          num_pvr_rendering, num_pvr_images,                    &
!!     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!!
!!      subroutine set_num_rendering_and_images(num_pvr_ctl, pvr_ctls,  &
!!     &          istack_pvr_render, istack_pvr_images)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
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
      subroutine count_num_rendering_and_images(num_pvr_ctl, pvr_ctls,  &
     &          num_pvr, num_pvr_rendering, num_pvr_images)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
!
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
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          num_pvr_rendering = num_pvr_rendering + 1
!
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            num_pvr_images = num_pvr_images + 0
          else
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
      subroutine s_num_rendering_and_images(num_pe, num_pvr, pvr_ctl,   &
     &          num_pvr_rendering, num_pvr_images,                      &
     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      use set_composition_pe_range
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in) :: num_pvr_images
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr
!
!
      call s_set_composition_pe_range(num_pe, num_pvr, pvr_ctl,         &
     &    num_pvr_rendering, num_pvr_images,                            &
     &    istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      call set_pvr_file_parameters(num_pvr, pvr_ctl,                    &
     &    istack_pvr_images, num_pvr_images, pvr_rgb)
!
!      if(iflag_debug .eq. 0) return
      if(my_rank .gt. 0) return
!      write(*,*) 'istack_pvr_render', istack_pvr_render
!      write(*,*) 'istack_pvr_images', istack_pvr_images
      write(*,*) 'ID, File, start_composition_PE, ouput_PE, Num_PE'
      do i_pvr = 1, num_pvr_images
        write(*,*) i_pvr, trim(pvr_rgb(i_pvr)%pvr_prefix), '  ',        &
     &             pvr_rgb(i_pvr)%irank_start_composit,                 &
     &             pvr_rgb(i_pvr)%irank_image_file,                     &
     &             pvr_rgb(i_pvr)%npe_img_composit,                     &
     &             trim(pvr_rgb(i_pvr)%pvr_prefix)
      end do
!
      end subroutine s_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_parameters(num_pvr_ctl, pvr_ctls,         &
     &          istack_pvr_images, num_pvr_images, pvr_rgb)
!
      use skip_comment_f
      use set_control_each_pvr
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      integer(kind = kint), intent(in) :: num_pvr_images
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr_ctl)
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist
      character(len=kchara) :: pvr_prefix
!
!
      do i_pvr = 1, num_pvr_ctl
        ist = istack_pvr_images(i_pvr-1) + 1
!
        call set_pvr_file_prefix(pvr_ctls(i_pvr), pvr_prefix)
        call set_pvr_file_control(pvr_ctls(i_pvr), pvr_rgb(ist))
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            pvr_rgb(ist)%pvr_prefix = pvr_prefix
          else
            pvr_rgb(ist  )%pvr_prefix = add_left_label(pvr_prefix)
            pvr_rgb(ist+1)%pvr_prefix = add_right_label(pvr_prefix)
            call set_pvr_file_control                                   &
     &         (pvr_ctls(i_pvr), pvr_rgb(ist+1))
          end if
        else
          pvr_rgb(ist)%pvr_prefix = pvr_prefix
        end if
      end do
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
      subroutine set_pvr_file_control(pvr_ctl, pvr_rgb)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use set_area_4_viz
      use skip_comment_f
      use output_image_sel_4_png
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len = kchara) :: tmpchara
!
!
      tmpchara = pvr_ctl%file_fmt_ctl%charavalue
      if(cmp_no_case(tmpchara, 'png')) then
        pvr_rgb%id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, 'bmp')) then
        pvr_rgb%id_pvr_file_type = iflag_BMP
      else
        pvr_rgb%id_pvr_file_type = iflag_BMP
      end if
!
      tmpchara = pvr_ctl%transparent_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'rgba')                             &
     &   .or. cmp_no_case(tmpchara, 'transparent')) then
        pvr_rgb%id_pvr_transparent = 1
      else if(cmp_no_case(tmpchara, 'rgb')                              &
     &   .or. cmp_no_case(tmpchara, 'solid')) then
        pvr_rgb%id_pvr_transparent = 0
      else
        pvr_rgb%id_pvr_transparent = 0
      end if
!
      pvr_rgb%iflag_monitoring = 0
      if(yes_flag(pvr_ctl%monitoring_ctl%charavalue)) then
        pvr_rgb%iflag_monitoring = 1
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'id_pvr_file_type', pvr_rgb%id_pvr_file_type
        write(*,*) 'id_pvr_transparent', pvr_rgb%id_pvr_transparent
      end if
!
      end subroutine set_pvr_file_control
!
!  ---------------------------------------------------------------------
!
      end module rendering_and_image_nums
