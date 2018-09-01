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
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_rendering_vr_image
!
      implicit none
!
!>  Structure for PVR images
      type pvr_multi_rendering
!>        Number of rendering for volume rendering
        integer(kind = kint) :: num_pvr_rendering = 0
!>        Number of rendreing for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_render(:)
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
!
!>        Structure for projection data
        type(pvr_projection_data), allocatable :: pvr_proj(:)
!
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Structure for field parameter for PVR
        type(pvr_output_parameter), allocatable :: file_param(:)
!>        Viewer coordinate information
        type(pvr_image_type), allocatable :: pvr_rgb(:)
      end type pvr_multi_rendering
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
     &         (nprocs, num_pvr_ctl, pvr_ctl, num_pvr, pvr_images)
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr_ctl)
      integer(kind = kint), intent(inout) :: num_pvr
!
      type(pvr_multi_rendering), intent(inout) :: pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      call count_num_rendering_and_images                               &
     &   (num_pvr_ctl, pvr_ctl, num_pvr,                                &
     &    pvr_images%num_pvr_rendering, pvr_images%num_pvr_images)
!
      call alloc_istack_pvr_image_4_merge(num_pvr, pvr_images)
!
      call set_num_rendering_and_images(num_pvr_ctl, pvr_ctl,           &
     &    pvr_images%istack_pvr_render, pvr_images%istack_pvr_images)
!
      call set_rank_to_write_images(nprocs,                             &
     &    pvr_images%num_pvr_images, pvr_images%file_param)
      call set_pvr_file_parameters(num_pvr_ctl, pvr_ctl,                &
     &    pvr_images%istack_pvr_images, pvr_images%num_pvr_images,      &
     &    pvr_images%file_param)
!
      if(iflag_debug .eq. 0) return
      write(*,*) 'num_pvr', num_pvr
      write(*,*) 'num_pvr_rendering', pvr_images%num_pvr_rendering
      write(*,*) 'num_pvr_images', pvr_images%num_pvr_images
      write(*,*) 'istack_pvr_render', pvr_images%istack_pvr_render
      write(*,*) 'istack_pvr_images', pvr_images%istack_pvr_images
      do i_pvr = 1, pvr_images%num_pvr_images
        write(*,*) 'irank_image_file',                                  &
     &            pvr_images%file_param(i_pvr)%irank_image_file,    &
     &            trim(pvr_images%file_param(i_pvr)%pvr_prefix)
      end do
!
      end subroutine s_num_rendering_and_images
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istack_pvr_image_4_merge(pvr_images)
!
      type(pvr_multi_rendering), intent(inout) :: pvr_images
!
      integer(kind = kint) :: i_pvr
!
!
      deallocate(pvr_images%istack_pvr_render)
      deallocate(pvr_images%istack_pvr_images)
!
      do i_pvr = 1, pvr_images%num_pvr_images
        call dealloc_pvr_image_array_type(pvr_images%pvr_rgb(i_pvr))
      end do
      deallocate(pvr_images%file_param, pvr_images%pvr_rgb)
!
!
      do i_pvr = 1, pvr_images%num_pvr_rendering
        call dealloc_pvr_surf_domain_item                               &
     &     (pvr_images%pvr_proj(i_pvr)%bound)
      end do
      deallocate(pvr_images%pvr_proj)
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
      subroutine alloc_istack_pvr_image_4_merge(num_pvr, pvr_images)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_multi_rendering), intent(inout) :: pvr_images
!
!
      allocate(pvr_images%istack_pvr_render(0:num_pvr))
      allocate(pvr_images%istack_pvr_images(0:num_pvr))
      pvr_images%istack_pvr_render = 0
      pvr_images%istack_pvr_images = 0
!
      allocate(pvr_images%pvr_proj(pvr_images%num_pvr_rendering))
!
      allocate(pvr_images%file_param(pvr_images%num_pvr_images))
      allocate(pvr_images%pvr_rgb(pvr_images%num_pvr_images))
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
      subroutine set_rank_to_write_images                               &
     &         (nprocs, num_pvr_images, file_param)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_pvr_images
      type(pvr_output_parameter), intent(inout)                         &
     &                           :: file_param(num_pvr_images)
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
        file_param(i_pvr)%irank_image_file = mod(icou, nprocs)
      end do
!
      end subroutine set_rank_to_write_images
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_parameters(num_pvr_ctl, pvr_ctls,         &
     &          istack_pvr_images, num_pvr_images, file_param)
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
      type(pvr_output_parameter), intent(inout)                         &
     &                           :: file_param(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist
      character(len=kchara) :: pvr_prefix
!
!
      do i_pvr = 1, num_pvr_ctl
        ist = istack_pvr_images(i_pvr-1) + 1
!
        call set_pvr_file_prefix(pvr_ctls(i_pvr), pvr_prefix)
        call set_pvr_file_control(pvr_ctls(i_pvr), file_param(ist))
        if(yes_flag(pvr_ctls(i_pvr)%streo_ctl%charavalue)) then
          if(yes_flag(pvr_ctls(i_pvr)%anaglyph_ctl%charavalue)) then
            file_param(ist)%pvr_prefix = pvr_prefix
          else
            call add_left_label                                         &
     &         (pvr_prefix, file_param(ist)%pvr_prefix)
            call add_right_label                                        &
     &         (pvr_prefix, file_param(ist+1)%pvr_prefix)
            call set_pvr_file_control                                   &
     &         (pvr_ctls(i_pvr), file_param(ist+1))
          end if
        else
          file_param(ist)%pvr_prefix = pvr_prefix
        end if
      end do
!
      end subroutine set_pvr_file_parameters
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_image_data
