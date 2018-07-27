!t_groups_sph_dynamic.f90
!
!      module t_groups_sph_dynamic
!
!      Written by H. Matsui on Aug., 2018
!
!!      subroutine alloc_sph_dynamic_grp_stack(sph_d_grp)
!!      subroutine alloc_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!!      subroutine dealloc_sph_dynamic_grp_stack(sph_d_grp)
!!      subroutine dealloc_sph_dynamic_grp_item(sph_d_grp)
!!        type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!
!!      subroutine check_sph_dynamic_grp_num(sph_d_grp)
!!      subroutine check_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(sph_shell_parameters), intent(in) :: sph_params
!
      module t_groups_sph_dynamic
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!
      type sph_dynamic_model_group
        integer(kind = kint) :: ngrp_dynamic
        integer(kind = kint), allocatable :: igrp_gl_dynamic(:,:)
!
        integer(kind = kint) :: ngrp_rt(2)
!
        integer(kind = kint), allocatable :: istack_dynamic_kr(:)
        integer(kind = kint), allocatable :: istack_dynamic_lt(:)
!
        integer(kind = kint) :: ntot_dynamic_rt(2)
        integer(kind = kint), allocatable :: kr_gl_dynamic(:)
        integer(kind = kint), allocatable :: lt_gl_dynamic(:)
!
        integer(kind = kint), allocatable :: kr_dynamic(:)
        integer(kind = kint), allocatable :: lt_dynamic(:)
!
        integer(kind = kint), allocatable :: kgrp_dynamic(:)
        integer(kind = kint), allocatable :: lgrp_dynamic(:)
      end type sph_dynamic_model_group
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_dynamic_grp_stack(sph_d_grp)
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
!
      allocate(sph_d_grp%istack_dynamic_kr(0:sph_d_grp%ngrp_rt(1)))
      allocate(sph_d_grp%istack_dynamic_lt(0:sph_d_grp%ngrp_rt(2)))
      allocate(sph_d_grp%igrp_gl_dynamic(sph_d_grp%ngrp_dynamic,2))
!
      sph_d_grp%istack_dynamic_kr = 0
      sph_d_grp%istack_dynamic_lt = 0
      if(sph_d_grp%ngrp_dynamic .gt. 0) sph_d_grp%igrp_gl_dynamic = 0
!
      end subroutine alloc_sph_dynamic_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
!
      allocate(sph_d_grp%kr_dynamic(sph_d_grp%ntot_dynamic_rt(1)))
      allocate(sph_d_grp%kr_gl_dynamic(sph_d_grp%ntot_dynamic_rt(1)))
      allocate(sph_d_grp%lt_dynamic(sph_d_grp%ntot_dynamic_rt(2)))
      allocate(sph_d_grp%lt_gl_dynamic(sph_d_grp%ntot_dynamic_rt(2)))
      allocate(sph_d_grp%kgrp_dynamic(sph_rtp%nidx_rtp(1)))
      allocate(sph_d_grp%lgrp_dynamic(sph_rtp%nidx_rtp(2)))
!
      if(sph_d_grp%ntot_dynamic_rt(1) .gt. 0) then
        sph_d_grp%kr_dynamic =    0
        sph_d_grp%kr_gl_dynamic = 0
      end if
      if(sph_d_grp%ntot_dynamic_rt(2) .gt. 0) then
        sph_d_grp%lt_dynamic =    0
        sph_d_grp%lt_gl_dynamic = 0
      end if
      if(sph_rtp%nidx_rtp(1) .gt. 0) sph_d_grp%kgrp_dynamic = 0
      if(sph_rtp%nidx_rtp(2) .gt. 0) sph_d_grp%lgrp_dynamic = 0
!
      end subroutine alloc_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_dynamic_grp_stack(sph_d_grp)
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
!
      deallocate(sph_d_grp%istack_dynamic_kr)
      deallocate(sph_d_grp%istack_dynamic_lt)
      deallocate(sph_d_grp%igrp_gl_dynamic)
!
      end subroutine dealloc_sph_dynamic_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_dynamic_grp_item(sph_d_grp)
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
!
      deallocate(sph_d_grp%kr_dynamic)
      deallocate(sph_d_grp%kr_gl_dynamic)
      deallocate(sph_d_grp%lt_dynamic)
      deallocate(sph_d_grp%lt_gl_dynamic)
      deallocate(sph_d_grp%kgrp_dynamic)
      deallocate(sph_d_grp%lgrp_dynamic)
!
      end subroutine dealloc_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_dynamic_grp_num(sph_d_grp)
!
      use t_spheric_rtp_data
!
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
!
       write(*,*) 'ngrp_rt', my_rank, sph_d_grp%ngrp_rt
       write(*,*) 'ngrp_dynamic', my_rank, sph_d_grp%ngrp_dynamic
!
      end subroutine check_sph_dynamic_grp_num
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!
      use t_spheric_rtp_data
      use set_parallel_file_name
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint) :: i, kr, kst, ked, lst, led, lt
      character(len = kchara) :: fhead = 'grouping_check'
      character(len = kchara) :: fname_tmp, fname
!
!
      call add_int_suffix(my_rank, fhead, fname_tmp)
      call add_dat_extension(fname_tmp, fname)
      open(my_rank+50, file=fname)
!
      do i = 1, sph_d_grp%ngrp_rt(1)
        kst = sph_d_grp%istack_dynamic_kr(i-1) + 1
        ked = sph_d_grp%istack_dynamic_kr(i)
        write(my_rank+50,*) 'istack_dynamic_kr', my_rank,               &
     &           sph_rtp%irank_sph_rtp(1), i, kst, ked
        do kr = kst, ked
          write(my_rank+50,*)                                           &
     &      kr, sph_d_grp%kr_gl_dynamic(kr), sph_d_grp%kr_dynamic(kr),  &
     &          sph_rtp%radius_1d_rtp_r(sph_d_grp%kr_dynamic(kr))
        end do
      end do
!
      write(my_rank+50,*) 'kgrp_dynamic', my_rank
      do kr = 1, sph_rtp%nidx_rtp(1)
        write(my_rank+50,*)  kr, sph_d_grp%kgrp_dynamic(kr) 
      end do
!
      do i = 1, sph_d_grp%ngrp_rt(2)
        lst = sph_d_grp%istack_dynamic_lt(i-1) + 1
        led = sph_d_grp%istack_dynamic_lt(i)
        write(my_rank+50,*) 'istack_dynamic_lt', my_rank,               &
     &           sph_rtp%irank_sph_rtp(2), i, lst, led
        do lt = lst, led
          write(my_rank+50,*)                                           &
     &     lt, sph_d_grp%lt_gl_dynamic(lt), sph_d_grp%lt_dynamic(lt)
        end do
      end do
!
      write(my_rank+50,*) 'lgrp_dynamic', my_rank
      do lt = 1, sph_rtp%nidx_rtp(2)
        write(my_rank+50,*)  lt, sph_d_grp%lgrp_dynamic(lt) 
      end do
      close(my_rank+50)
!
      end subroutine check_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
!
      end module t_groups_sph_dynamic
