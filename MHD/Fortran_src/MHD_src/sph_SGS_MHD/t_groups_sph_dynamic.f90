!t_groups_sph_dynamic.f90
!
!      module t_groups_sph_dynamic
!
!      Written by H. Matsui on Aug., 2018
!
!!      subroutine alloc_sph_dynamic_grp_stack(sph_d_grp)
!!      subroutine alloc_sph_dynamic_grp_item(sph_d_grp)
!!      subroutine dealloc_sph_dynamic_grp_stack(sph_d_grp)
!!      subroutine dealloc_sph_dynamic_grp_item(sph_d_grp)
!!        type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!!
!!      subroutine alloc_mk_sph_dgrp_flag(sph_rtp, wk_dgrp)
!!      subroutine alloc_mk_sph_dgrp_stack(wk_dgrp)
!!      subroutine alloc_mk_sph_istack_dynamic(SGS_param, wk_dgrp)
!!      subroutine dealloc_mk_sph_dgrp_flag(wk_dgrp)
!!      subroutine dealloc_mk_sph_dgrp_stack(wk_dgrp)
!!      subroutine dealloc_mk_sph_istack_dynamic(wk_dgrp)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!!
!!      subroutine check_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!      subroutine ckeck_dynamic_grp_iflag(sph_params, sph_rtp, wk_dgrp)
!!      subroutine ckeck_make_dynamic_grp_stacks(wk_dgrp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
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
        integer(kind = kint) :: ntot_dynamic_kr
        integer(kind = kint) :: ntot_dynamic_lt
!
        integer(kind = kint), allocatable :: kr_gl_dynamic(:)
        integer(kind = kint), allocatable :: lt_gl_dynamic(:)
!
        integer(kind = kint), allocatable :: kr_dynamic(:)
        integer(kind = kint), allocatable :: lt_dynamic(:)
      end type sph_dynamic_model_group
!
!
      type make_sph_dynamic_model_grp
        integer(kind = kint) :: nprocs_rt(2)
!
        integer(kind = kint), allocatable :: iflag_kr_l(:)
        integer(kind = kint), allocatable :: iflag_kr_g(:)
!
        integer(kind = kint), allocatable :: iflag_lt_l(:)
        integer(kind = kint), allocatable :: iflag_lt_g(:)
!
        integer(kind = kint), allocatable :: istack_global_kr(:)
        integer(kind = kint), allocatable :: istack_global_lt(:)
!
        integer(kind = kint), allocatable :: istack_r_gl_ngrp(:)
        integer(kind = kint), allocatable :: istack_t_gl_ngrp(:)
!
        integer(kind = kint), allocatable :: istack_rgrp(:)
        integer(kind = kint), allocatable :: istack_tgrp(:)
      end type make_sph_dynamic_model_grp
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
      subroutine alloc_sph_dynamic_grp_item(sph_d_grp)
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
!
      allocate(sph_d_grp%kr_dynamic(sph_d_grp%ntot_dynamic_kr))
      allocate(sph_d_grp%kr_gl_dynamic(sph_d_grp%ntot_dynamic_kr))
      allocate(sph_d_grp%lt_dynamic(sph_d_grp%ntot_dynamic_lt))
      allocate(sph_d_grp%lt_gl_dynamic(sph_d_grp%ntot_dynamic_lt))
!
      if(sph_d_grp%ntot_dynamic_kr .gt. 0) sph_d_grp%kr_dynamic =    0
      if(sph_d_grp%ntot_dynamic_kr .gt. 0) sph_d_grp%kr_gl_dynamic = 0
      if(sph_d_grp%ntot_dynamic_lt .gt. 0) sph_d_grp%lt_dynamic =    0
      if(sph_d_grp%ntot_dynamic_lt .gt. 0) sph_d_grp%lt_gl_dynamic = 0
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
!
      end subroutine dealloc_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_dgrp_flag(sph_rtp, wk_dgrp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%iflag_kr_l(sph_rtp%nidx_global_rtp(1)))
      allocate(wk_dgrp%iflag_kr_g(sph_rtp%nidx_global_rtp(1)))
      allocate(wk_dgrp%iflag_lt_l(sph_rtp%nidx_global_rtp(2)))
      allocate(wk_dgrp%iflag_lt_g(sph_rtp%nidx_global_rtp(2)))
!
      if(sph_rtp%nidx_global_rtp(1) .gt. 0) then
        wk_dgrp%iflag_kr_l = 0
        wk_dgrp%iflag_kr_g = 0
      end if
      if(sph_rtp%nidx_global_rtp(2) .gt. 0) then
        wk_dgrp%iflag_lt_l = 0
        wk_dgrp%iflag_lt_g = 0
      end if
!
      end subroutine alloc_mk_sph_dgrp_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_dgrp_stack(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%istack_global_kr(0:wk_dgrp%nprocs_rt(1)))
      allocate(wk_dgrp%istack_global_lt(0:wk_dgrp%nprocs_rt(2)))
      allocate(wk_dgrp%istack_r_gl_ngrp(0:wk_dgrp%nprocs_rt(1)))
      allocate(wk_dgrp%istack_t_gl_ngrp(0:wk_dgrp%nprocs_rt(2)))
      wk_dgrp%istack_global_kr = 0
      wk_dgrp%istack_global_lt = 0
      wk_dgrp%istack_r_gl_ngrp = 0
      wk_dgrp%istack_t_gl_ngrp = 0
!
      end subroutine alloc_mk_sph_dgrp_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_istack_dynamic(SGS_param, wk_dgrp)
!
      use t_SGS_control_parameter
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%istack_rgrp(0:SGS_param%ngrp_rave_dynamic))
      allocate(wk_dgrp%istack_tgrp(0:SGS_param%ngrp_medave_dynamic))
      wk_dgrp%istack_rgrp = 0
      wk_dgrp%istack_tgrp = 0
!
      end subroutine alloc_mk_sph_istack_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_mk_sph_dgrp_flag(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      deallocate(wk_dgrp%iflag_kr_l, wk_dgrp%iflag_lt_l)
      deallocate(wk_dgrp%iflag_kr_g, wk_dgrp%iflag_lt_g)
!
      end subroutine dealloc_mk_sph_dgrp_flag
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_mk_sph_dgrp_stack(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      deallocate(wk_dgrp%istack_global_kr)
      deallocate(wk_dgrp%istack_global_lt)
      deallocate(wk_dgrp%istack_r_gl_ngrp)
      deallocate(wk_dgrp%istack_t_gl_ngrp)
!
      end subroutine dealloc_mk_sph_dgrp_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_mk_sph_istack_dynamic(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      deallocate(wk_dgrp%istack_rgrp)
      deallocate(wk_dgrp%istack_tgrp)
!
      end subroutine dealloc_mk_sph_istack_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint) :: i, kr, kst, ked, lst, led, lt
!
!
      do i = 1, sph_d_grp%ngrp_rt(1)
        kst = sph_d_grp%istack_dynamic_kr(i-1) + 1
        ked = sph_d_grp%istack_dynamic_kr(i)
        write(*,*) 'istack_dynamic_kr', my_rank,                        &
     &           sph_rtp%irank_sph_rtp(1), i, kst, ked
        do kr = kst, ked
          write(*,*)                                                    &
     &      kr, sph_d_grp%kr_gl_dynamic(kr), sph_d_grp%kr_dynamic(kr),  &
     &          sph_rtp%radius_1d_rtp_r(sph_d_grp%kr_dynamic(kr))
        end do
      end do
!
      do i = 1, sph_d_grp%ngrp_rt(2)
        lst = sph_d_grp%istack_dynamic_lt(i-1) + 1
        led = sph_d_grp%istack_dynamic_lt(i)
        write(*,*) 'istack_dynamic_lt', my_rank,                        &
     &           sph_rtp%irank_sph_rtp(2), i, lst, led
        do lt = lst, led
          write(*,*)                                                    &
     &     lt, sph_d_grp%lt_gl_dynamic(lt), sph_d_grp%lt_dynamic(lt)
        end do
      end do
!
      end subroutine check_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine ckeck_dynamic_grp_iflag(sph_params, sph_rtp, wk_dgrp)
!
      use t_spheric_parameter
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      integer(kind = kint) :: kr_gl, lt_gl
!
!
      write(*,*) 'nprocs_rt', wk_dgrp%nprocs_rt
      write(*,*) 'kr_gl, iflag_kr_g(kr_gl)'
      do kr_gl = sph_params%nlayer_ICB, sph_params%nlayer_CMB
        write(*,*) kr_gl, wk_dgrp%iflag_kr_g(kr_gl)
      end do
      write(*,*) 'lt_gl, iflag_lt_g(lt_gl)'
      do lt_gl = 1, sph_rtp%nidx_global_rtp(2)
        write(*,*) lt_gl, wk_dgrp%iflag_lt_g(lt_gl)
      end do
!
      end subroutine ckeck_dynamic_grp_iflag
!
! -----------------------------------------------------------------------
!
      subroutine ckeck_make_dynamic_grp_stacks(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
!
      write(*,*) 'istack_global_kr', wk_dgrp%istack_global_kr
      write(*,*) 'istack_r_gl_ngrp', wk_dgrp%istack_r_gl_ngrp
      write(*,*) 'istack_rgrp',      wk_dgrp%istack_rgrp
      write(*,*) 'istack_global_lt', wk_dgrp%istack_global_lt
      write(*,*) 'istack_t_gl_ngrp', wk_dgrp%istack_t_gl_ngrp
      write(*,*) 'istack_tgrp',      wk_dgrp%istack_tgrp
!
      end subroutine ckeck_make_dynamic_grp_stacks
!
! -----------------------------------------------------------------------
!
      end module t_groups_sph_dynamic
