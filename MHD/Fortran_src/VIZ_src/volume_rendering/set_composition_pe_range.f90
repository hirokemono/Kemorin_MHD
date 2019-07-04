!>@file   set_composition_pe_range.f90
!!@brief  module set_composition_pe_range
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine s_set_composition_pe_range(num_pe, num_pvr, pvr_ctl, &
!!     &          num_pvr_rendering, num_pvr_images,                    &
!!     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!@endverbatim
!
      module set_composition_pe_range
!
      use m_precision
!
      use t_control_data_4_pvr
      use t_pvr_image_array
!
      implicit none
!
      private :: set_num_rendering_and_images
      private :: set_rank_to_write_tmp, set_rank_to_write_images
      private :: set_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_composition_pe_range(num_pe, num_pvr, pvr_ctl,   &
     &          num_pvr_rendering, num_pvr_images,                      &
     &          istack_pvr_render, istack_pvr_images, pvr_rgb)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in) :: num_pvr_images
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint), allocatable:: irank_image_tmp(:)
      integer(kind = kint), allocatable:: irank_end_tmp(:)
      integer(kind = kint), allocatable:: maxpe_composit_tmp(:)
!
!
      allocate(maxpe_composit_tmp(num_pvr_rendering))
      allocate(irank_image_tmp(num_pvr_rendering))
      allocate(irank_end_tmp(num_pvr_rendering))
!
      call set_num_rendering_and_images(num_pvr, pvr_ctl,               &
     &    istack_pvr_render, istack_pvr_images)
!
      call set_maxpe_composit_tmp(num_pe, num_pvr, pvr_ctl,             &
     &    istack_pvr_render, istack_pvr_images,                         &
     &    num_pvr_rendering, maxpe_composit_tmp)
!
      call set_rank_to_write_tmp(num_pe, num_pvr_rendering,             &
     &    maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
      call set_rank_to_write_images                                     &
     &   (num_pvr, istack_pvr_render, istack_pvr_images,                &
     &    num_pvr_rendering, irank_image_tmp, irank_end_tmp,            &
     &    num_pvr_images, pvr_rgb)
!
      deallocate(irank_image_tmp, irank_end_tmp, maxpe_composit_tmp)
!
      end subroutine s_set_composition_pe_range
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_num_rendering_and_images(num_pvr_ctl, pvr_ctls,    &
     &          istack_pvr_render, istack_pvr_images)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_render(0:num_pvr_ctl)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_pvr_images(0:num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr
!
!
      istack_pvr_render(0) = 0
      istack_pvr_images(0) = 0
      do i_pvr = 1, num_pvr_ctl
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 2
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
          else
            istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 2
          end if
        else
          istack_pvr_render(i_pvr) = istack_pvr_render(i_pvr-1) + 1
          istack_pvr_images(i_pvr) = istack_pvr_images(i_pvr-1) + 1
        end if
      end do
!
      end subroutine set_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_tmp(num_pe, num_pvr_rendering,       &
     &          maxpe_composit_tmp, irank_image_tmp, irank_end_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in)                                  &
     &              :: maxpe_composit_tmp(num_pvr_rendering)
!
      integer(kind = kint), intent(inout)                               &
     &              :: irank_image_tmp(num_pvr_rendering)
      integer(kind = kint), intent(inout)                               &
     &              :: irank_end_tmp(num_pvr_rendering)
!
      integer(kind = kint) :: i_pvr, num
      real(kind = kreal) :: address
!
!
      do i_pvr = 1, num_pvr_rendering
        address = dble((i_pvr-1) * num_pe) / dble(num_pvr_rendering)
        irank_image_tmp(i_pvr) = aint(address)
      end do

      do i_pvr = 1, num_pvr_rendering - 1 
        num = irank_image_tmp(i_pvr+1) - irank_image_tmp(i_pvr)
        if(num .le. 0) then
           irank_end_tmp(i_pvr) = irank_image_tmp(i_pvr)
        else if(num .gt. maxpe_composit_tmp(i_pvr)) then
           irank_end_tmp(i_pvr)                                         &
     &       = irank_image_tmp(i_pvr) + maxpe_composit_tmp(i_pvr)
        else
          irank_end_tmp(i_pvr) = irank_image_tmp(i_pvr+1) - 1
        end if
      end do
      irank_end_tmp(num_pvr_rendering) = num_pe - 1
!
      end subroutine set_rank_to_write_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine set_rank_to_write_images                               &
     &         (num_pvr_ctl, istack_pvr_render, istack_pvr_images,      &
     &          num_pvr_rendering, irank_image_tmp, irank_end_tmp,      &
     &          num_pvr_images, pvr_rgb)
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pvr_render(0:num_pvr_ctl)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pvr_images(0:num_pvr_ctl)
!
      integer(kind = kint), intent(in) :: num_pvr_rendering
      integer(kind = kint), intent(in)                                  &
     &              :: irank_image_tmp(num_pvr_rendering)
      integer(kind = kint), intent(in)                                  &
     &              :: irank_end_tmp(num_pvr_rendering)
      integer(kind = kint), intent(in) :: num_pvr_images
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: ist_img, ist_rdr, num_img, num_rdr
!
!
      do i_pvr = 1, num_pvr_ctl
        ist_img = istack_pvr_render(i_pvr-1)
        ist_rdr = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_render(i_pvr) - ist_img
        num_rdr = istack_pvr_images(i_pvr) - ist_rdr
        if(num_rdr .eq. 2) then
          if(num_img .eq. 1) then
            pvr_rgb(ist_img+1)%irank_image_file                         &
     &         = irank_image_tmp(ist_rdr+1)
            pvr_rgb(ist_img+1)%irank_end_composit                       &
     &         = irank_end_tmp(ist_rdr+2)
          else
            pvr_rgb(ist_img+1)%irank_image_file                         &
     &          = irank_image_tmp(ist_rdr+1)
            pvr_rgb(ist_img+2)%irank_image_file                         &
     &          = irank_image_tmp(ist_rdr+1)
            pvr_rgb(ist_img+1)%irank_end_composit                       &
     &          = irank_end_tmp(ist_rdr+2)
            pvr_rgb(ist_img+2)%irank_end_composit                       &
     &          = irank_end_tmp(ist_rdr+2)
          end if
        else
          pvr_rgb(ist_img+1)%irank_image_file                           &
     &        = irank_image_tmp(ist_rdr+1)
          pvr_rgb(ist_img+1)%irank_end_composit                         &
     &        = irank_end_tmp(ist_rdr+1)
        end if
      end do
!
      do i_pvr = 1, num_pvr_images
        pvr_rgb(i_pvr)%npe_img_composit                                 &
     &      = pvr_rgb(i_pvr)%irank_end_composit                         &
     &       - pvr_rgb(i_pvr)%irank_image_file + 1
      end do
!
      end subroutine set_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_maxpe_composit_tmp(num_pe, num_pvr_ctl,            &
     &          pvr_ctl, istack_pvr_render, istack_pvr_images,          &
     &          num_pvr_rendering, maxpe_composit_tmp)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: num_pvr_ctl
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pvr_render(0:num_pvr_ctl)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_pvr_images(0:num_pvr_ctl)
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
!
      integer(kind = kint), intent(in) :: num_pvr_rendering
!
      integer(kind = kint), intent(inout)                               &
     &              :: maxpe_composit_tmp(num_pvr_rendering)
!
      integer(kind = kint) :: i_pvr, nmax
      integer(kind = kint) :: ist_img, num_img, num_rdr
!
!
      do i_pvr = 1, num_pvr_ctl
        ist_img = istack_pvr_render(i_pvr-1)
        num_img = istack_pvr_render(i_pvr) - ist_img
        num_rdr = istack_pvr_images(i_pvr) - istack_pvr_images(i_pvr-1)
!
        if(pvr_ctl(i_pvr)%maxpe_composit_ctl%iflag .gt. 0) then
          nmax = pvr_ctl(i_pvr)%maxpe_composit_ctl%intvalue
        else
          nmax = num_pe
        end if
        if(num_rdr .eq. 2) then
          if(num_img .eq. 1) then
            maxpe_composit_tmp(ist_img+1) = 2*nmax
          else
            maxpe_composit_tmp(ist_img+1) = nmax
            maxpe_composit_tmp(ist_img+2) = nmax
          end if
        else
          maxpe_composit_tmp(ist_img+1) = nmax
        end if
      end do
!
      end subroutine set_maxpe_composit_tmp
!
!  ---------------------------------------------------------------------
!
      end module set_composition_pe_range
