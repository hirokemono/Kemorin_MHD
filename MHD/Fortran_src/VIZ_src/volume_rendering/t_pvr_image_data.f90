!>@file   t_pvr_image_data.f90
!!@brief  module t_pvr_image_data
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine s_num_rendering_and_images                           &
!!     &         (nprocs, num_pvr_ctl, pvr_ctl,                         &
!!     &          num_pvr, num_pvr_rendering, pvr_images)
!!      subroutine set_streo_rendering_flags                            &
!!     &         (num_pvr_ctl, pvr_ctl, pvr_data)
!!        type(volume_rendering_controls), intent(in)                   &
!!     &                     :: pvr_ctls(num_pvr_ctl)
!!        type(PVR_image_generator), intent(inout)                      &
!!     &                     :: pvr_data(num_pvr_ctl)
!!
!!      subroutine set_rank_to_write_images(nprocs, num_pvr_images, img)
!!      subroutine set_pvr_file_parameters(num_pvr_ctl, pvr_ctls,       &
!!     &          num_pvr_images, img)
!!@endverbatim
!
      module t_pvr_image_data
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
      use t_control_params_4_pvr
!
      implicit none
!
!>  Structure for PVR images
      type pvr_mul_image_data
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
!
!>        Structure for each PVR image
        type(pvr_output_parameter), allocatable :: img(:)
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
      subroutine s_num_rendering_and_images                             &
     &         (nprocs, num_pvr_ctl, pvr_ctl,                           &
     &          num_pvr, num_pvr_rendering, pvr_images)
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
      integer(kind = kint), intent(inout) :: num_pvr
      integer(kind = kint), intent(inout) :: num_pvr_rendering
!
      type(pvr_mul_image_data), intent(inout) :: pvr_images
!
      integer(kind = kint) :: i_pvr
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
      call set_rank_to_write_images(nprocs,                             &
     &    pvr_images%num_pvr_images, pvr_images%img)
      call set_pvr_file_parameters(num_pvr_ctl, pvr_ctl ,               &
     &    pvr_images%num_pvr_images, pvr_images%img)
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr_rendering', num_pvr_rendering
      write(*,*) 'num_pvr_images', pvr_images%num_pvr_images
      write(*,*) 'istack_pvr_images', pvr_images%istack_pvr_images
      do i_pvr = 1, pvr_images%num_pvr_images
        write(*,*) 'irank_image_file',                                  &
     &            pvr_images%img(i_pvr)%irank_image_file,               &
     &            trim(pvr_images%img(i_pvr)%pvr_prefix)
      end do
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
      deallocate(pvr_images%img)
!
      end subroutine dealloc_istack_pvr_image_4_merge
!
!  ---------------------------------------------------------------------
!
      subroutine set_streo_rendering_flags                              &
     &         (num_pvr_ctl, pvr_ctl, pvr_data)
!
      use skip_comment_f
      use t_rendering_vr_image
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
      type(PVR_image_generator), intent(inout)                          &
     &                     :: pvr_data(num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr_ctl
        pvr_data(i_pvr)%view%iflag_stereo_pvr =  0
        pvr_data(i_pvr)%view%iflag_anaglyph =    0
        if(yes_flag(pvr_ctl(i_pvr)%streo_ctl%charavalue)) then
          pvr_data(i_pvr)%view%iflag_stereo_pvr = 1
!
          if(yes_flag(pvr_ctl(i_pvr)%anaglyph_ctl%charavalue)) then
            pvr_data(i_pvr)%view%iflag_anaglyph = 1
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
      allocate(pvr_images%img(num))
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
      subroutine set_rank_to_write_images(nprocs, num_pvr_images, img)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_pvr_images
      type(pvr_output_parameter), intent(inout) :: img(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, icou, nstep
!
!
      if(num_pvr_images .gt. nprocs) then
        nstep = 1
      else
        call cal_divide_and_rest(nstep, icou, nprocs, num_pvr_images)
      end if
!
      do i_pvr = 1, num_pvr_images
        icou = (i_pvr-1) * nstep
        img(i_pvr)%irank_image_file = mod(icou, nprocs)
      end do
!
      end subroutine set_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_parameters(num_pvr_ctl, pvr_ctls,         &
     &          num_pvr_images, img)
!
      use skip_comment_f
      use set_control_each_pvr
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctls(num_pvr_ctl)
      integer(kind = kint), intent(in) :: num_pvr_images
!
      type(pvr_output_parameter), intent(inout) :: img(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, icou
      character(len=kchara) :: pvr_prefix
!
!
      icou = 0
      do i_pvr = 1, num_pvr_ctl
        icou = icou + 1
        call set_pvr_file_prefix(pvr_ctls(i_pvr), pvr_prefix)
        call set_pvr_file_control(pvr_ctls(i_pvr), img(icou))
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            img(icou)%pvr_prefix = pvr_prefix
          else
            call add_left_label(pvr_prefix, img(icou)%pvr_prefix)
!
            icou = icou + 1
            call set_pvr_file_control(pvr_ctls(i_pvr), img(icou))
            call add_right_label(pvr_prefix, img(icou)%pvr_prefix)
          end if
        else
          img(icou)%pvr_prefix = pvr_prefix
        end if
      end do
!
      end subroutine set_pvr_file_parameters
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_data
